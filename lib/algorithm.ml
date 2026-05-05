(*
 * Rule enumeration algorithm.
 *
 * Each iteration n:
 *   1. Enumerate canonical terms of size n from previous irreducibles + variable holes.
 *   2. In parallel: simplify each term, evaluate on inputs, compare behaviour
 *      against existing irreducibles.
 *   3. Sequentially merge decisions: add size-/KBO- rules, collect candidates.
 *   4. Group candidates by behaviour; KBO-smallest per group becomes irreducible,
 *      others get KBO-simplifying rules.
 *)

let rec list_compare elt_compare a b =
  match a, b with
  | [], [] -> 0 | [], _ -> -1 | _, [] -> 1
  | x :: xs, y :: ys ->
    match elt_compare x y with 0 -> list_compare elt_compare xs ys | c -> c

let rec list_equal eq a b =
  match a, b with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && list_equal eq xs ys
  | _ -> false

type 'a rule_sets = {
  mutable size_rules : Types.rule list;
  mutable kbo_rules : Types.rule list;
  mutable behaviors : (Types.term * 'a list) list;
  inputs : 'a Eval.input list;
  norm_cache : (Types.term, Types.term * bool) Hashtbl.t;
  use_smt : bool;
}

let create ~use_smt inputs = {
  size_rules = []; kbo_rules = []; behaviors = [];
  inputs; norm_cache = Hashtbl.create 2048;
  use_smt;
}

let irreducibles rs = List.map fst rs.behaviors
let all_rules rs = rs.size_rules @ rs.kbo_rules

let group_by cmp key_of_value values =
  let rec loop values acc = match values with
    | [] -> acc
    | v :: rest ->
      let k = key_of_value v in
      let rec insert = function
        | [] -> [(k, [v])]
        | (k', vs) :: tl when cmp k k' = 0 -> (k', v :: vs) :: tl
        | pair :: tl -> pair :: insert tl
      in loop rest (insert acc)
  in loop values []

type iter_summary = {
  size : int;  enumerated : int;
  new_size_rules : Types.rule list;
  new_kbo_rules : Types.rule list;
  new_irreducibles : Types.term list;
  total_size_rules : int;  total_kbo_rules : int;  total_irreducible : int;
  time_total : float;  time_enum : float;  time_process : float;
  time_norm : float;  time_eval : float;  time_match : float;
  time_apply : float;  time_group : float;
}

type 'a term_decision =
  | D_size_rule of Types.rule
  | D_kbo_rule of Types.rule
  | D_replace of Types.term * Types.term * 'a list
  | D_skip
  | D_candidate of Types.term * 'a list

type match_kind = Size | Kbo | Replace | Skip

let process_term (dom : 'a Domain.t) ~inputs ~norm_index ~behaviors ~norm_cache
      ~use_smt ~smt_vars ~prof_norm ~prof_eval ~prof_match t =
  let rec find_best bv simplified best = function
    | [] -> best
    | (irr, irr_bv) :: rest ->
      let eq = if bv = [] && irr_bv = [] then
        if not use_smt then false
        else match Smt.check_equiv "bool" smt_vars simplified irr with
          | Smt.Equivalent -> true | _ -> false
      else list_equal dom.Domain.equal bv irr_bv
      in
      if not eq then find_best bv simplified best rest
      else
        let irr_sz = Types.size irr in let t_sz = Types.size simplified in
        if irr_sz < t_sz then
          match best with
          | None -> find_best bv simplified (Some (irr, Size)) rest
          | Some (prev, Size) ->
            find_best bv simplified (Some ((if Kbo.lt irr prev then irr else prev), Size)) rest
          | _ -> find_best bv simplified best rest
        else if irr_sz = t_sz then
          match Kbo.kbo_compare simplified irr with
          | 0 -> Some (irr, Skip)
          | c when c > 0 ->
            (match best with
             | None | Some (_, Size) -> find_best bv simplified (Some (irr, Kbo)) rest
             | Some (prev, Kbo) ->
               find_best bv simplified (Some ((if Kbo.lt irr prev then irr else prev), Kbo)) rest
             | _ -> find_best bv simplified best rest)
          | _ ->
            (match best with
             | None | Some (_, Size) -> find_best bv simplified (Some (irr, Replace)) rest
             | Some (prev, Kbo) | Some (prev, Replace) ->
               find_best bv simplified (Some ((if Kbo.lt irr prev then irr else prev), Replace)) rest
             | _ -> find_best bv simplified best rest)
        else find_best bv simplified best rest
  in
  let decide bv simplified =
    match find_best bv simplified None behaviors with
    | None -> Some (D_candidate (simplified, bv))
    | Some (irr, Size) -> Some (D_size_rule (simplified, irr))
    | Some (irr, Kbo) -> Some (D_kbo_rule (simplified, irr))
    | Some (irr, Replace) -> Some (D_replace (irr, simplified, bv))
    | Some (_, Skip) -> Some D_skip
  in
  match Hashtbl.find_opt norm_cache t with
  | Some (simplified, size_reduced) ->
    if size_reduced then None else decide (Eval.behavior dom inputs simplified) simplified
  | None ->
    let t0 = Sys.time () in
    let simplified, size_reduced = Rewrite.normalize ~index:norm_index t in
    let t1 = Sys.time () in
    Hashtbl.replace norm_cache t (simplified, size_reduced);
    if size_reduced then (prof_norm := !prof_norm +. (t1 -. t0); None)
    else
      let bv = Eval.behavior dom inputs simplified in
      let t2 = Sys.time () in
      let d = decide bv simplified in
      prof_norm := !prof_norm +. (t1 -. t0);
      prof_eval := !prof_eval +. (t2 -. t1);
      prof_match := !prof_match +. (Sys.time () -. t2);
      d

let parallel_map ~num_domains f lst =
  let len = List.length lst in
  if num_domains <= 1 || len < num_domains * 2 then List.filter_map f lst
  else
    let chunk_size = (len + num_domains - 1) / num_domains in
    let rec take n acc = function
      | [] -> (List.rev acc, [])
      | rest when n <= 0 -> (List.rev acc, rest)
      | x :: xs -> take (n - 1) (x :: acc) xs
    in
    let rec split acc = function
      | [] -> List.rev acc
      | rest -> let chunk, remaining = take chunk_size [] rest in
                split (chunk :: acc) remaining
    in
    let domains = List.map (fun chunk ->
      Stdlib.Domain.spawn (fun () -> List.filter_map f chunk)
    ) (split [] lst) in
    List.concat (List.map Stdlib.Domain.join domains)

let apply_decisions (dom : 'a Domain.t) (rs : 'a rule_sets)
      (decisions : 'a term_decision list) =
  let new_size_rules = ref [] in
  let new_kbo_rules = ref [] in
  let candidates = ref [] in
  List.iter (function
    | D_size_rule rule ->
      rs.size_rules <- rule :: rs.size_rules;
      new_size_rules := rule :: !new_size_rules
    | D_kbo_rule rule ->
      rs.kbo_rules <- rule :: rs.kbo_rules;
      new_kbo_rules := rule :: !new_kbo_rules
    | D_replace (_old_irr, new_term, stored_bv) ->
      let idx = ref (-1) in
      List.iteri (fun i (_, b) ->
        if !idx = -1 && list_equal dom.Domain.equal stored_bv b then idx := i
      ) rs.behaviors;
      (match !idx with
       | -1 -> ()
       | i ->
         let current_irr = fst (List.nth rs.behaviors i) in
         match Kbo.kbo_compare new_term current_irr with
         | 0 -> ()
         | c when c > 0 ->
           rs.kbo_rules <- (new_term, current_irr) :: rs.kbo_rules;
           new_kbo_rules := (new_term, current_irr) :: !new_kbo_rules
         | _ ->
           let rule = (current_irr, new_term) in
           rs.kbo_rules <- rule :: rs.kbo_rules;
           new_kbo_rules := rule :: !new_kbo_rules;
           rs.behaviors <- List.mapi (fun j (t', b) ->
             if j = i then (new_term, b) else (t', b)) rs.behaviors)
    | D_skip -> ()
    | D_candidate (t, bv) -> candidates := (t, bv) :: !candidates
  ) decisions;
  (!new_size_rules, !new_kbo_rules, !candidates)

let run_iteration (dom : 'a Domain.t) (rs : 'a rule_sets) (n : int)
      (max_vars : int) ~num_domains : iter_summary =
  let t_start = Sys.time () in

  let enumerated = Enum.enumerate_terms dom.Domain.signature (irreducibles rs) n max_vars in
  let t_enum = Sys.time () -. t_start in

  let inputs = rs.inputs in
  let norm_index = Rewrite.index_rules (all_rules rs) in
  let behaviors = rs.behaviors in
  let norm_cache = rs.norm_cache in
  let pn = ref 0. in let pe = ref 0. in let pm = ref 0. in
  let use_smt = rs.use_smt && inputs = [] in
  let smt_vars = if not use_smt then [] else
    List.map (fun v -> (v, "bool"))
      (List.map Types.var_name (List.init max_vars (fun i -> i))) in
  let nd = if use_smt then 1 else num_domains in  (* Z3 not thread-safe *)
  let decisions =
    parallel_map ~num_domains:nd
      (process_term dom ~inputs ~norm_index ~behaviors ~norm_cache
         ~use_smt ~smt_vars ~prof_norm:pn ~prof_eval:pe ~prof_match:pm)
      enumerated
  in
  let t_process = Sys.time () -. t_start -. t_enum in

  let t_a0 = Sys.time () in
  let new_kbo_rules = ref [] in
  let new_size_rules = ref [] in
  let _sr, _kbo_from_dec, candidates = apply_decisions dom rs decisions in
  new_size_rules := _sr; new_kbo_rules := _kbo_from_dec;
  let t_apply = Sys.time () -. t_a0 in

  let t_g0 = Sys.time () in
  let new_irreducibles = ref [] in
  let cmp = list_compare dom.Domain.compare in
  let groups = group_by cmp snd candidates in
  let groups =
    if not rs.use_smt || rs.inputs <> [] then groups
    else
        List.concat_map (fun (bv, term_pairs) ->
        let all_terms = List.map fst term_pairs in
        match all_terms with [] -> [] | first :: rest ->
          let groups = ref [[first]] in  (* list of groups, each is a term list *)
          List.iter (fun t ->
            let idx = ref (-1) in
            List.iteri (fun i g ->
              if !idx = -1 then
                let rep = List.hd g in
                let eq = try match Smt.check_equiv "bool" smt_vars t rep with
                  | Smt.Equivalent -> true | _ -> false with _ -> false
                in
                if eq then idx := i
            ) !groups;
            match !idx with
            | -1 -> groups := [t] :: !groups  (* new group *)
            | i -> groups := List.mapi (fun j g ->
                if j = i then t :: g else g) !groups
          ) rest;
          List.map (fun g -> (bv, List.map (fun t -> (t, bv)) g)) !groups
        ) groups
  in
  (* Post-merge: sequential SMT grouping is order-dependent; merge equivalent groups *)
  let groups =
    if not rs.use_smt || rs.inputs <> [] then groups else
    let rec merge_all acc = function
      | [] -> acc
      | (bv, tp) :: rest ->
        let rep = fst (List.hd tp) in
        let same, diff = List.partition (fun (_, tp2) ->
          let rep2 = fst (List.hd tp2) in
          try match Smt.check_equiv "bool" smt_vars rep rep2 with
          | Smt.Equivalent -> true | _ -> false with _ -> false
        ) rest in
        let merged_tp = List.fold_left (fun acc' (_, tp2) -> acc' @ tp2) tp same in
        merge_all ((bv, merged_tp) :: acc) diff
    in List.rev (merge_all [] groups)
  in
  let new_irr_pairs = ref [] in
  List.iter (fun (_bv, term_pairs) ->
    let terms = List.map fst term_pairs in
    let best = Kbo.minimum terms in
    new_irreducibles := best :: !new_irreducibles;
    new_irr_pairs := (best, _bv) :: !new_irr_pairs;
    List.iter (fun other ->
      if not (Types.term_eq other best) then (
        let rule = (other, best) in
        rs.kbo_rules <- rule :: rs.kbo_rules;
        new_kbo_rules := rule :: !new_kbo_rules))
      terms)
    groups;
  (* Sort and add irreducibles in deterministic KBO order *)
  let sorted = List.sort (fun (a,_) (b,_) -> Kbo.kbo_compare a b) !new_irr_pairs in
  List.iter (fun (irr, bv) -> rs.behaviors <- (irr, bv) :: rs.behaviors) (List.rev sorted);
  new_irreducibles := List.map fst sorted;
  let t_group = Sys.time () -. t_g0 in

  { size = n;
    enumerated = List.length enumerated;
    new_size_rules = List.rev !new_size_rules;
    new_kbo_rules = List.rev !new_kbo_rules;
    new_irreducibles = List.rev !new_irreducibles;
    total_size_rules = List.length rs.size_rules;
    total_kbo_rules = List.length rs.kbo_rules;
    total_irreducible = List.length rs.behaviors;
    time_total = Sys.time () -. t_start;
    time_enum = t_enum;
    time_process = t_process;
    time_norm = !pn;  time_eval = !pe;  time_match = !pm;
    time_apply = t_apply;
    time_group = t_group;
  }

let run ?max_size ?(forced_inputs = []) ?(on_iteration = fun _ -> ())
      ?(num_domains = 0) ?(use_smt = false) (dom : 'a Domain.t)
      ~num_random_inputs ~max_vars =
  let default_max = match max_size with Some m -> m | None -> 12 in
  let random_inputs =
    if num_random_inputs > 0
    then Eval.generate_inputs dom num_random_inputs max_vars else [] in
  let inputs = forced_inputs @ random_inputs in
  if inputs = [] && not use_smt then
    failwith "Algorithm.run: no inputs and SMT disabled (use --smt or provide inputs)";
  let rs = create ~use_smt inputs in
  let nproc = try Stdlib.Domain.recommended_domain_count () with _ -> 1 in
  let threads = if num_domains > 0 then num_domains else nproc in
  let results = ref [] in
  let n = ref 1 in
  let continue = ref true in
  while !continue && !n <= default_max do
    let summary = run_iteration dom rs !n max_vars ~num_domains:threads in
    if summary.new_size_rules = [] && summary.new_kbo_rules = []
       && summary.new_irreducibles = [] then continue := false
    else (on_iteration summary; results := summary :: !results; incr n)
  done;
  (rs, List.rev !results)
