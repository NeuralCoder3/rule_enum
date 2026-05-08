(* Rule enumeration algorithm with polymorphic symbol types. *)

let rec list_compare elt_compare a b = match a, b with
  | [], [] -> 0 | [], _ -> -1 | _, [] -> 1
  | x :: xs, y :: ys -> match elt_compare x y with 0 -> list_compare elt_compare xs ys | c -> c

let rec list_equal eq a b = match a, b with
  | [], [] -> true | x :: xs, y :: ys -> eq x y && list_equal eq xs ys | _ -> false

type ('s, 'a) rule_sets = {
  mutable size_rules    : 's Types.rule list;
  mutable kbo_rules     : 's Types.rule list;
  mutable behaviors     : ('s Types.term * 'a list) list;
  mutable forced_inputs : (string * int) list list;
  inputs              : 'a Eval.input list;
  norm_cache          : ('s Types.term, 's Types.term * bool) Hashtbl.t;
  norm_cache_mutex    : Mutex.t;
  use_smt             : bool;
  use_smt_forced      : bool;
}

let create ~use_smt ~use_smt_forced inputs = {
  size_rules = []; kbo_rules = []; behaviors = []; forced_inputs = [];
  inputs; norm_cache = Hashtbl.create 2048;
  norm_cache_mutex = Mutex.create (); use_smt; use_smt_forced;
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

let process_term (dom : ('s, 'a) Domain.t) ~inputs ~norm_index ~behaviors ~norm_cache ~norm_cache_mutex
      ~use_smt ~smt_vars ~sym_cmp t =
  (* Pick the better of two same-kind candidates by Kbo.compare_total
     (deterministic syntactic tiebreaker). Used when multiple equivalent
     irreducibles can serve as the rule target. *)
  let prefer a b = if Kbo.compare_total sym_cmp a b < 0 then a else b in
  let rec find_best bv simplified best = function
    | [] -> best
    | (irr, irr_bv) :: rest ->
      let eq = if bv = [] && irr_bv = [] then
        if not use_smt then false
        else match Smt.check_equiv dom smt_vars simplified irr with
          | Smt.Equivalent -> true | _ -> false
      else list_equal dom.Domain.equal bv irr_bv
      in if not eq then find_best bv simplified best rest
      else
        (* Determine the relation between irr and simplified under the
           proof's partial KBO (var-count gated). Each branch is a candidate
           for the "best" decision; we never form a rule unless KBO permits. *)
        let irr_sz = Types.size irr and t_sz = Types.size simplified in
        match Kbo.kbo sym_cmp irr simplified with
        | Kbo.Equal ->
          (* Structurally identical to irr. Skip unless a Size match exists. *)
          (match best with
           | Some (_, Size) -> find_best bv simplified best rest
           | _ -> Some (irr, Skip))
        | Kbo.Less when irr_sz < t_sz ->
          (* irr ≺ₖ simplified and strictly smaller in size — Size rule. *)
          (match best with
           | None -> find_best bv simplified (Some (irr, Size)) rest
           | Some (prev, Size) -> find_best bv simplified (Some (prefer irr prev, Size)) rest
           | Some (_, (Kbo | Replace | Skip)) -> find_best bv simplified (Some (irr, Size)) rest)
        | Kbo.Less ->
          (* irr ≺ₖ simplified at the same size — KBO rule t → irr. *)
          (match best with
           | None -> find_best bv simplified (Some (irr, Kbo)) rest
           | Some (_, Size) -> find_best bv simplified best rest
           | Some (prev, Kbo) -> find_best bv simplified (Some (prefer irr prev, Kbo)) rest
           | Some (_, (Replace | Skip)) -> find_best bv simplified (Some (irr, Kbo)) rest)
        | Kbo.Greater ->
          (* simplified ≺ₖ irr — t is a better representative; replace irr. *)
          (match best with
           | None -> find_best bv simplified (Some (irr, Replace)) rest
           | Some (_, Size) -> find_best bv simplified best rest
           | Some (_, Kbo) -> find_best bv simplified best rest
           | Some (prev, Replace) -> find_best bv simplified (Some (prefer irr prev, Replace)) rest
           | Some (_, Skip) -> find_best bv simplified (Some (irr, Replace)) rest)
        | Kbo.Incomparable ->
          (* Equivalent in behavior but no KBO ordering — no rule possible
             with this irr. Continue; if no other irr works either, the term
             becomes a candidate and may be added as a separate irreducible. *)
          find_best bv simplified best rest
  in let decide bv simplified = match find_best bv simplified None behaviors with
    | None -> Some (D_candidate (simplified, bv))
    | Some (irr, Size) -> Some (D_size_rule (simplified, irr))
    | Some (irr, Kbo) -> Some (D_kbo_rule (simplified, irr))
    | Some (irr, Replace) -> Some (D_replace (irr, simplified, bv))
    | Some (_, Skip) -> Some D_skip
  in
  Mutex.lock norm_cache_mutex;
  let cached = Hashtbl.find_opt norm_cache t in
  Mutex.unlock norm_cache_mutex;
  match cached with
    | Some (simplified, size_reduced) ->
      if size_reduced then None
      else decide (Eval.behavior dom inputs simplified) simplified
    | None ->
      let simplified, size_reduced = Rewrite.normalize ~index:norm_index t in
      Mutex.lock norm_cache_mutex;
      Hashtbl.replace norm_cache t (simplified, size_reduced);
      Mutex.unlock norm_cache_mutex;
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
      ~inputs (decisions : ('s, 'a) term_decision list) =
  let new_size_rules = ref [] in let new_kbo_rules = ref [] in let candidates = ref [] in
  List.iter (function
    | D_size_rule rule -> rs.size_rules <- rule :: rs.size_rules; new_size_rules := rule :: !new_size_rules
    | D_kbo_rule rule  -> rs.kbo_rules  <- rule :: rs.kbo_rules;  new_kbo_rules  := rule :: !new_kbo_rules
    | D_replace (old_irr, new_term, _stored_bv) ->
      let idx = ref (-1) in
      List.iteri (fun i (term, _) ->
        if !idx = -1 && Types.term_eq dom.Domain.sym_compare term old_irr then idx := i)
        rs.behaviors;
      (match !idx with -1 -> () | i ->
        let current_irr = fst (List.nth rs.behaviors i) in
        match Kbo.kbo dom.Domain.sym_compare new_term current_irr with
        | Kbo.Equal -> ()
        | Kbo.Greater ->
          (* current_irr ≺ₖ new_term, so we can rewrite new_term to current_irr. *)
          rs.kbo_rules <- (new_term, current_irr) :: rs.kbo_rules;
          new_kbo_rules := (new_term, current_irr) :: !new_kbo_rules
        | Kbo.Less ->
          (* new_term ≺ₖ current_irr — replace and add rule current_irr → new_term. *)
          let rule = (current_irr, new_term) in
          rs.kbo_rules <- rule :: rs.kbo_rules; new_kbo_rules := rule :: !new_kbo_rules;
          let new_bv = Eval.behavior dom inputs new_term in
          rs.behaviors <- List.mapi (fun j (t', b) -> if j = i then (new_term, new_bv) else (t', b)) rs.behaviors
        | Kbo.Incomparable ->
          (* No KBO ordering between current_irr and new_term — neither can
             rewrite to the other. Keep both as separate canonical reps of
             this ≈-class. *)
          let new_bv = Eval.behavior dom inputs new_term in
          rs.behaviors <- (new_term, new_bv) :: rs.behaviors)
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
  let all_inputs = inputs @ if rs.use_smt_forced then List.map (fun vars ->
    List.map (fun (v, n) -> (v, dom.Domain.int_to_val n)) vars) rs.forced_inputs
  else [] in
  if rs.use_smt_forced then
    rs.behaviors <- List.map (fun (irr, _) -> (irr, Eval.behavior dom all_inputs irr)) rs.behaviors;
  let norm_index = Rewrite.index_rules (all_rules rs) in
  let use_smt = rs.use_smt && inputs = [] in
  let smt_vars = if not use_smt then [] else
    List.map Types.var_name (List.init max_vars (fun i -> i)) in
  let nd = if use_smt then 1 else num_domains in
  let decisions = parallel_map ~num_domains:nd
    (process_term dom ~inputs:all_inputs ~norm_index ~behaviors:rs.behaviors
      ~norm_cache:rs.norm_cache ~norm_cache_mutex:rs.norm_cache_mutex
      ~use_smt ~smt_vars ~sym_cmp) enumerated in
  let t_process = Sys.time () -. t_start -. t_enum in
  let new_kbo_rules = ref [] in let new_size_rules = ref [] in
  let _sr, _kr, candidates = apply_decisions dom rs ~inputs:all_inputs decisions in
  new_size_rules := _sr; new_kbo_rules := _kr;
  let t_apply = Sys.time () -. t_start -. t_enum -. t_process in
  let new_irreducibles = ref [] in
  let cmp = list_compare dom.Domain.compare in
  let groups = group_by cmp snd candidates in
  let groups = if not rs.use_smt || rs.inputs <> [] then groups else
    let cex = ref [] in
    let groups = List.concat_map (fun (bv, term_pairs) ->
      let all_terms = List.map fst term_pairs in match all_terms with [] -> [] | first :: rest ->
      let groups = ref [[first]] in
      List.iter (fun t -> let idx = ref (-1) in
        List.iteri (fun i g -> if !idx = -1 then
          let rep = List.hd g in
          (try match Smt.check_equiv dom smt_vars t rep with
            Smt.Equivalent -> idx := i
          |           Smt.CounterExample assigns -> if rs.use_smt_forced then cex := assigns :: !cex
          | _ -> () with _ -> ())) !groups;
        match !idx with -1 -> groups := [t] :: !groups
        | i -> groups := List.mapi (fun j g -> if j = i then t :: g else g) !groups) rest;
      List.map (fun g -> (bv, List.map (fun t -> (t, bv)) g)) !groups) groups in
    List.iter (fun assigns ->
      rs.forced_inputs <- assigns :: rs.forced_inputs) !cex;
    groups
  in
  let groups =
    if not rs.use_smt || rs.inputs <> [] then groups else
    let cex2 = ref [] in
    let rec merge_all acc = function
      | [] -> acc | (bv, tp) :: rest ->
        let rep = fst (List.hd tp) in
        let same, diff = List.partition (fun (_, tp2) ->
          match Smt.check_equiv dom smt_vars rep (fst (List.hd tp2)) with
            Smt.Equivalent -> true
          | Smt.CounterExample assigns -> (if rs.use_smt_forced then cex2 := assigns :: !cex2); false
          | _ -> false) rest in
        let merged = List.fold_left (fun a (_, tp2) -> a @ tp2) tp same in
        merge_all ((bv, merged) :: acc) diff
    in
    List.iter (fun assigns -> rs.forced_inputs <- assigns :: rs.forced_inputs) !cex2;
    List.rev (merge_all [] groups)
  in
  let new_irr_pairs = ref [] in
  List.iter (fun (_bv, term_pairs) ->
    let terms = List.map fst term_pairs in
    (* KBO-minimal elements of the group: those with no strictly KBO-smaller
       equivalent. With partial KBO, multiple terms may be mutually
       incomparable — each becomes a separate canonical rep (matches the
       proof's `smtMin l = l ⇒ l ∈ I_can` clause). *)
    let minimals = List.filter (fun t ->
      not (List.exists (fun s -> Kbo.lt sym_cmp s t) terms)) terms in
    List.iter (fun m ->
      new_irreducibles := m :: !new_irreducibles;
      new_irr_pairs := (m, _bv) :: !new_irr_pairs) minimals;
    (* Non-minimals rewrite to some KBO-strictly-smaller minimal. By KBO
       well-foundedness on the finite group, such a minimal always exists. *)
    List.iter (fun other ->
      let is_min = List.exists (fun m -> Types.term_eq sym_cmp other m) minimals in
      if not is_min then
        match List.find_opt (fun m -> Kbo.lt sym_cmp m other) minimals with
        | Some target ->
          rs.kbo_rules <- (other, target) :: rs.kbo_rules;
          new_kbo_rules := (other, target) :: !new_kbo_rules
        | None -> ()) terms)
    groups;
  let sorted = List.sort (fun (a,_) (b,_) -> Kbo.compare_total sym_cmp a b) !new_irr_pairs in
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
      ?(use_smt = false) ?(use_smt_forced = false) (dom : ('s, 'a) Domain.t) ~num_random_inputs ~max_vars =
  let default_max = match max_size with Some m -> m | None -> 12 in
  let random_inputs = if num_random_inputs > 0 then Eval.generate_inputs dom num_random_inputs max_vars else [] in
  let inputs = forced_inputs @ random_inputs in
  if inputs = [] && not use_smt then
    failwith "Algorithm.run: no inputs and SMT disabled";
  let rs = create ~use_smt ~use_smt_forced inputs in
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
