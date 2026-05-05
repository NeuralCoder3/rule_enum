(* Rule enumeration algorithm with polymorphic symbol types. *)

let rec list_compare elt_compare a b = match a, b with
  | [], [] -> 0 | [], _ -> -1 | _, [] -> 1
  | x :: xs, y :: ys -> match elt_compare x y with 0 -> list_compare elt_compare xs ys | c -> c

let rec list_equal eq a b = match a, b with
  | [], [] -> true | x :: xs, y :: ys -> eq x y && list_equal eq xs ys | _ -> false

type ('s, 'a) rule_sets = {
  mutable size_rules : 's Types.rule list;
  mutable kbo_rules  : 's Types.rule list;
  mutable behaviors  : ('s Types.term * 'a list) list;
  inputs          : 'a Eval.input list;
  norm_cache      : ('s Types.term, 's Types.term * bool) Hashtbl.t;
  use_smt         : bool;
}

let create ~use_smt inputs = {
  size_rules = []; kbo_rules = []; behaviors = [];
  inputs; norm_cache = Hashtbl.create 2048; use_smt;
}

let irreducibles rs = List.map fst rs.behaviors
let all_rules rs = rs.size_rules @ rs.kbo_rules

let group_by cmp key_of_value values =
  let rec loop vs acc = match vs with
    | [] -> acc | v :: rest ->
      let k = key_of_value v in
      let rec insert = function
        | [] -> [(k, [v])]
        | (k', vs') :: tl when cmp k k' = 0 -> (k', v :: vs') :: tl
        | p :: tl -> p :: insert tl
      in loop rest (insert acc)
  in loop values []

type 's iter_summary = {
  size : int;  enumerated : int;
  new_size_rules : 's Types.rule list;
  new_kbo_rules  : 's Types.rule list;
  new_irreducibles : 's Types.term list;
  total_size_rules : int;  total_kbo_rules : int;  total_irreducible : int;
  time_total : float;  time_enum : float;  time_process : float;
  time_norm : float;  time_eval : float;  time_match : float;
  time_apply : float;  time_group : float;
}

type ('s, 'a) term_decision =
  | D_size_rule of 's Types.rule
  | D_kbo_rule  of 's Types.rule
  | D_replace   of 's Types.term * 's Types.term * 'a list
  | D_skip
  | D_candidate of 's Types.term * 'a list

type match_kind = Size | Kbo | Replace | Skip

let process_term (dom : ('s, 'a) Domain.t) ~inputs ~norm_index ~behaviors ~norm_cache
      ~use_smt ~smt_vars ~sym_cmp t =
  let rec find_best bv simplified best = function
    | [] -> best
    | (irr, irr_bv) :: rest ->
      let eq = if bv = [] && irr_bv = [] then
        if not use_smt then false
        else match Smt.check_equiv dom smt_vars simplified irr with
          | Smt.Equivalent -> true | _ -> false
      else list_equal dom.Domain.equal bv irr_bv
      in if not eq then find_best bv simplified best rest
      else let irr_sz = Types.size irr and t_sz = Types.size simplified in
      if irr_sz < t_sz then
        match best with
        | None -> find_best bv simplified (Some (irr, Size)) rest
        | Some (prev, Size) -> find_best bv simplified (Some ((if Kbo.lt sym_cmp irr prev then irr else prev), Size)) rest
        | _ -> find_best bv simplified best rest
      else if irr_sz = t_sz then match Kbo.kbo_compare sym_cmp simplified irr with
        | 0 -> Some (irr, Skip)
        | c when c > 0 ->
          (match best with None | Some (_, Size) -> find_best bv simplified (Some (irr, Kbo)) rest
           | Some (prev, Kbo) -> find_best bv simplified (Some ((if Kbo.lt sym_cmp irr prev then irr else prev), Kbo)) rest
           | _ -> find_best bv simplified best rest)
        | _ ->
          (match best with None | Some (_, Size) -> find_best bv simplified (Some (irr, Replace)) rest
           | Some (prev, Kbo) | Some (prev, Replace) ->
             find_best bv simplified (Some ((if Kbo.lt sym_cmp irr prev then irr else prev), Replace)) rest
           | _ -> find_best bv simplified best rest)
      else find_best bv simplified best rest
  in let decide bv simplified = match find_best bv simplified None behaviors with
    | None -> Some (D_candidate (simplified, bv))
    | Some (irr, Size) -> Some (D_size_rule (simplified, irr))
    | Some (irr, Kbo) -> Some (D_kbo_rule (simplified, irr))
    | Some (irr, Replace) -> Some (D_replace (irr, simplified, bv))
    | Some (_, Skip) -> Some D_skip
  in match Hashtbl.find_opt norm_cache t with
    | Some (simplified, size_reduced) ->
      if size_reduced then None
      else decide (Eval.behavior dom inputs simplified) simplified
    | None ->
      let simplified, size_reduced = Rewrite.normalize ~index:norm_index t in
      Hashtbl.replace norm_cache t (simplified, size_reduced);
      if size_reduced then None
      else decide (Eval.behavior dom inputs simplified) simplified

let parallel_map ~num_domains f lst =
  let len = List.length lst in
  if num_domains <= 1 || len < num_domains * 2 then List.filter_map f lst
  else let chunk_size = (len + num_domains - 1) / num_domains in
  let rec take n acc = function
    | [] -> (List.rev acc, []) | rest when n <= 0 -> (List.rev acc, rest)
    | x :: xs -> take (n - 1) (x :: acc) xs in
  let rec split acc = function
    | [] -> List.rev acc
    | rest -> let ch, rem = take chunk_size [] rest in split (ch :: acc) rem in
  let domains = List.map (fun ch -> Stdlib.Domain.spawn (fun () -> List.filter_map f ch)) (split [] lst) in
  List.concat (List.map Stdlib.Domain.join domains)

let apply_decisions (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets)
      (decisions : ('s, 'a) term_decision list) =
  let new_size_rules = ref [] in let new_kbo_rules = ref [] in let candidates = ref [] in
  List.iter (function
    | D_size_rule rule -> rs.size_rules <- rule :: rs.size_rules; new_size_rules := rule :: !new_size_rules
    | D_kbo_rule rule  -> rs.kbo_rules  <- rule :: rs.kbo_rules;  new_kbo_rules  := rule :: !new_kbo_rules
    | D_replace (_old_irr, new_term, stored_bv) ->
      let idx = ref (-1) in
      List.iteri (fun i (_, ei) ->
        if !idx = -1 && list_equal dom.Domain.equal stored_bv ei then idx := i)
        rs.behaviors;
      (match !idx with -1 -> () | i ->
        let current_irr = fst (List.nth rs.behaviors i) in
        match Kbo.kbo_compare dom.Domain.sym_compare new_term current_irr with
        | 0 -> ()
        | c when c > 0 ->
          rs.kbo_rules <- (new_term, current_irr) :: rs.kbo_rules; new_kbo_rules := (new_term, current_irr) :: !new_kbo_rules
        | _ ->
          let rule = (current_irr, new_term) in
          rs.kbo_rules <- rule :: rs.kbo_rules; new_kbo_rules := rule :: !new_kbo_rules;
          let new_bv = Eval.behavior dom rs.inputs new_term in
          rs.behaviors <- List.mapi (fun j (t', b) -> if j = i then (new_term, new_bv) else (t', b)) rs.behaviors)
    | D_skip -> ()
    | D_candidate (t, bv) -> candidates := (t, bv) :: !candidates
  ) decisions;
  (!new_size_rules, !new_kbo_rules, !candidates)

let make_examples dom inputs t = List.map (fun inp -> (inp, Eval.eval dom inp t)) inputs

let run_iteration (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets) (n : int)
      (max_vars : int) ~num_domains ~sym_cmp : 's iter_summary =
  let t_start = Sys.time () in
  let enumerated = Enum.enumerate_terms dom.Domain.all_symbols (irreducibles rs) n max_vars in
  let t_enum = Sys.time () -. t_start in
  let inputs = rs.inputs in
  let norm_index = Rewrite.index_rules (all_rules rs) in
  let use_smt = rs.use_smt && inputs = [] in
  let smt_vars = if not use_smt then [] else
    List.map Types.var_name (List.init max_vars (fun i -> i)) in
  let nd = if use_smt then 1 else num_domains in
  let decisions = parallel_map ~num_domains:nd
    (process_term dom ~inputs ~norm_index ~behaviors:rs.behaviors ~norm_cache:rs.norm_cache ~use_smt ~smt_vars ~sym_cmp) enumerated in
  let t_process = Sys.time () -. t_start -. t_enum in
  let new_kbo_rules = ref [] in let new_size_rules = ref [] in
  let _sr, _kr, candidates = apply_decisions dom rs decisions in
  new_size_rules := _sr; new_kbo_rules := _kr;
  let t_apply = Sys.time () -. t_start -. t_enum -. t_process in
  let new_irreducibles = ref [] in
  let cmp = list_compare dom.Domain.compare in
  let groups = group_by cmp snd candidates in
  let groups =
    if not rs.use_smt || rs.inputs <> [] then groups else
    List.concat_map (fun (bv, term_pairs) ->
      let all_terms = List.map fst term_pairs in match all_terms with [] -> [] | first :: rest ->
      let groups = ref [[first]] in
      List.iter (fun t -> let idx = ref (-1) in
        List.iteri (fun i g -> if !idx = -1 then
          let rep = List.hd g in
          let eq = try match Smt.check_equiv dom smt_vars t rep with
            Smt.Equivalent -> true | _ -> false with _ -> false in
          if eq then idx := i) !groups;
        match !idx with -1 -> groups := [t] :: !groups
        | i -> groups := List.mapi (fun j g -> if j = i then t :: g else g) !groups) rest;
      List.map (fun g -> (bv, List.map (fun t -> (t, bv)) g)) !groups) groups
  in
  let groups =
    if not rs.use_smt || rs.inputs <> [] then groups else
    let rec merge_all acc = function
      | [] -> acc | (bv, tp) :: rest ->
        let rep = fst (List.hd tp) in
        let same, diff = List.partition (fun (_, tp2) ->
          try match Smt.check_equiv dom smt_vars rep (fst (List.hd tp2)) with
            Smt.Equivalent -> true | _ -> false with _ -> false) rest in
        let merged = List.fold_left (fun a (_, tp2) -> a @ tp2) tp same in
        merge_all ((bv, merged) :: acc) diff
    in List.rev (merge_all [] groups)
  in
  let new_irr_pairs = ref [] in
  List.iter (fun (_bv, term_pairs) ->
    let terms = List.map fst term_pairs in
    let best = Kbo.minimum sym_cmp terms in
    new_irreducibles := best :: !new_irreducibles;
    new_irr_pairs := (best, _bv) :: !new_irr_pairs;
    List.iter (fun other -> if not (Types.term_eq sym_cmp other best) then
      (rs.kbo_rules <- (other, best) :: rs.kbo_rules; new_kbo_rules := (other, best) :: !new_kbo_rules)) terms)
    groups;
  let sorted = List.sort (fun (a,_) (b,_) -> Kbo.kbo_compare sym_cmp a b) !new_irr_pairs in
  List.iter (fun (irr, bv) -> rs.behaviors <- (irr, bv) :: rs.behaviors) (List.rev sorted);
  new_irreducibles := List.map fst sorted;
  { size = n; enumerated = List.length enumerated;
    new_size_rules = List.rev !new_size_rules; new_kbo_rules = List.rev !new_kbo_rules;
    new_irreducibles = List.rev !new_irreducibles;
    total_size_rules = List.length rs.size_rules; total_kbo_rules = List.length rs.kbo_rules;
    total_irreducible = List.length rs.behaviors;
    time_total = Sys.time () -. t_start; time_enum = t_enum;
    time_process = t_process; time_norm = 0.; time_eval = 0.; time_match = 0.;
    time_apply = t_apply; time_group = 0.;
  }

let run ?max_size ?(forced_inputs = []) ?(on_iteration = fun _ -> ()) ?(num_domains = 0)
      ?(use_smt = false) (dom : ('s, 'a) Domain.t) ~num_random_inputs ~max_vars =
  let default_max = match max_size with Some m -> m | None -> 12 in
  let random_inputs = if num_random_inputs > 0 then Eval.generate_inputs dom num_random_inputs max_vars else [] in
  let inputs = forced_inputs @ random_inputs in
  if inputs = [] && not use_smt then
    failwith "Algorithm.run: no inputs and SMT disabled";
  let rs = create ~use_smt inputs in
  let nproc = try Stdlib.Domain.recommended_domain_count () with _ -> 1 in
  let threads = if num_domains > 0 then num_domains else nproc in
  let results = ref [] in let n = ref 1 in let continue = ref true in
  while !continue && !n <= default_max do
    let summary = run_iteration dom rs !n max_vars ~num_domains:threads
        ~sym_cmp:dom.Domain.sym_compare in
    if summary.new_size_rules = [] && summary.new_kbo_rules = [] && summary.new_irreducibles = [] then
      continue := false
    else (on_iteration summary; results := summary :: !results; incr n)
  done; (rs, List.rev !results)
