(* Rule enumeration algorithm.

   Per-size iteration:
   1. Enumerate canonical terms at size n with `numDistinctVCs ≤ k`.
   2. Partition into var_only (no Hole _) and with_holes.
   3. Subpass 1: normalize + decide on var_only. Commit new var rules.
   4. Rebuild the rule index with the just-added var rules included.
   5. Subpass 2: normalize + decide on with_holes against the updated
      index. Var rules from step 3 may already kill many hole-containing
      candidates; survivors emit constP rules or new hole-irreducibles.

   3-tier equivalence check:
   * Tier 1 — global behavior vector against the shared random-input
     pool, indexed by `behaviors_by_bv` Hashtbl.
   * Tier 2 — per-term example sets `(input, value) list`. When two
     terms share a bv-bucket, we cross-evaluate each on the other's
     example list before declaring equivalence. Disagreement on any
     shared input distinguishes them.
   * Tier 3 — `Smt.check_equiv` fallback. Counterexamples extend both
     terms' example sets and (optionally) the global input pool.

   Single-threaded throughout. *)

let rec list_compare elt_compare a b = match a, b with
  | [], [] -> 0 | [], _ -> -1 | _, [] -> 1
  | x :: xs, y :: ys -> match elt_compare x y with 0 -> list_compare elt_compare xs ys | c -> c

let rec list_equal eq a b = match a, b with
  | [], [] -> true | x :: xs, y :: ys -> eq x y && list_equal eq xs ys | _ -> false

(* Per-term example set: list of (input-assignment, expected-value). Used
   for Tier 2 of the equivalence check. Initially seeded from the global
   inputs; grows when SMT/cross-eval produces new distinguishing inputs. *)
type 'a examples = ('a Eval.input * 'a) list

type ('s, 'a) rule_sets = {
  mutable size_rules    : 's Types.rule list;
  mutable kbo_rules     : 's Types.rule list;
  mutable behaviors     : ('s Types.term * 'a list * 'a examples) list;
  mutable forced_inputs : (string * int) list list;
  inputs              : 'a Eval.input list;
  use_smt             : bool;
  use_smt_forced      : bool;
}

let create ~use_smt ~use_smt_forced inputs = {
  size_rules = []; kbo_rules = []; behaviors = []; forced_inputs = [];
  inputs; use_smt; use_smt_forced;
}

let irreducibles rs = List.map (fun (t, _, _) -> t) rs.behaviors
let all_rules rs = rs.size_rules @ rs.kbo_rules

(* O(n) group via Hashtbl keyed by the bv. The `cmp` arg is unused — we
   rely on Hashtbl's structural hashing / equality (bv keys are
   `'a list` of equality-comparable values). *)
let group_by _cmp key_of_value values =
  let h = Hashtbl.create (max 16 (List.length values)) in
  List.iter (fun v ->
    let k = key_of_value v in
    let prev = try Hashtbl.find h k with Not_found -> [] in
    Hashtbl.replace h k (v :: prev)) values;
  Hashtbl.fold (fun k vs acc -> (k, vs) :: acc) h []

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
  | D_replace   of 's Types.term * 's Types.term * 'a list * 'a examples
  | D_skip
  | D_candidate of 's Types.term * 'a list * 'a examples

type match_kind = Size | Kbo | Replace | Skip

(* Tier 2 helper. Given two terms with the same Tier-1 bv, cross-evaluate
   on the shared and accumulated example sets. Returns true iff both
   agree on every example known to either side. *)
let tier2_agrees (dom : ('s, 'a) Domain.t) t1 ex1 t2 ex2 =
  let agree_on_one t (inp, expected) =
    dom.Domain.equal (Eval.eval dom inp t) expected
  in
  List.for_all (agree_on_one t1) ex2 &&
  List.for_all (agree_on_one t2) ex1

let process_term (dom : ('s, 'a) Domain.t) ~inputs:_ ~compiled_inputs
      ~norm_index ~behaviors ~behaviors_by_bv
      ~use_smt ~smt_vars ~sym_cmp t =
  let prefer a b = if Kbo.compare_total sym_cmp a b < 0 then a else b in
  (* equiv_irrs simplified bv returns the candidate equivalent irreducibles. *)
  let equiv_irrs simplified ex bv =
    let candidates =
      if bv = [] then
        if not use_smt then []
        else List.filter (fun (_, irr_bv, _) -> irr_bv = []) behaviors
      else match Hashtbl.find_opt behaviors_by_bv bv with
        | Some irrs -> irrs
        | None -> []
    in
    (* Tier 2: cross-evaluate examples. Catches cases where bv-bucketed
       terms agree on the global pool but diverge on per-term accumulated
       examples (e.g., from past SMT counterexamples). *)
    let tier2 = List.filter (fun (irr, _, irr_ex) ->
      tier2_agrees dom simplified ex irr irr_ex) candidates
    in
    (* Tier 3: SMT confirmation if enabled. *)
    if not use_smt then tier2
    else List.filter (fun (irr, _, _) ->
      match Smt.check_equiv dom smt_vars simplified irr with
      | Smt.Equivalent -> true
      | _ -> false) tier2
  in
  let rec find_best simplified best = function
    | [] -> best
    | (irr, _irr_bv, _irr_ex) :: rest ->
        let irr_sz = Types.size irr and t_sz = Types.size simplified in
        match Kbo.kbo sym_cmp irr simplified with
        | Kbo.Equal ->
          (match best with
           | Some (_, Size) -> find_best simplified best rest
           | _ -> Some (irr, Skip))
        | Kbo.Less when irr_sz < t_sz ->
          (match best with
           | None -> find_best simplified (Some (irr, Size)) rest
           | Some (prev, Size) -> find_best simplified (Some (prefer irr prev, Size)) rest
           | Some (_, (Kbo | Replace | Skip)) -> find_best simplified (Some (irr, Size)) rest)
        | Kbo.Less ->
          (match best with
           | None -> find_best simplified (Some (irr, Kbo)) rest
           | Some (_, Size) -> find_best simplified best rest
           | Some (prev, Kbo) -> find_best simplified (Some (prefer irr prev, Kbo)) rest
           | Some (_, (Replace | Skip)) -> find_best simplified (Some (irr, Kbo)) rest)
        | Kbo.Greater ->
          (match best with
           | None -> find_best simplified (Some (irr, Replace)) rest
           | Some (_, Size) -> find_best simplified best rest
           | Some (_, Kbo) -> find_best simplified best rest
           | Some (prev, Replace) -> find_best simplified (Some (prefer irr prev, Replace)) rest
           | Some (_, Skip) -> find_best simplified (Some (irr, Replace)) rest)
        | Kbo.Incomparable -> find_best simplified best rest
  in
  let decide ex bv simplified =
    match find_best simplified None (equiv_irrs simplified ex bv) with
    | None -> Some (D_candidate (simplified, bv, ex))
    | Some (irr, Size) -> Some (D_size_rule (simplified, irr))
    | Some (irr, Kbo) -> Some (D_kbo_rule (simplified, irr))
    | Some (irr, Replace) -> Some (D_replace (irr, simplified, bv, ex))
    | Some (_, Skip) -> Some D_skip
  in
  let simplified, size_reduced = Rewrite.normalize ~sym_cmp ~index:norm_index t in
  if size_reduced then None
  else
    let bv = Eval.behavior_compiled dom compiled_inputs simplified in
    (* Per-term examples accumulate divergent inputs (e.g., from SMT
       counterexamples). For the common case (no SMT), they stay empty:
       Tier 1 bv-equality already implies equivalence on the global pool,
       and Tier 2 cross-check is trivially satisfied. We avoid the
       O(n·inputs) make_examples allocation in the hot path. *)
    let ex = [] in
    decide ex bv simplified

let apply_decisions (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets)
      ~inputs (decisions : ('s, 'a) term_decision list) =
  let new_size_rules = ref [] in let new_kbo_rules = ref [] in let candidates = ref [] in
  List.iter (function
    | D_size_rule rule -> rs.size_rules <- rule :: rs.size_rules; new_size_rules := rule :: !new_size_rules
    | D_kbo_rule rule  -> rs.kbo_rules  <- rule :: rs.kbo_rules;  new_kbo_rules  := rule :: !new_kbo_rules
    | D_replace (old_irr, new_term, _stored_bv, _stored_ex) ->
      let idx = ref (-1) in
      List.iteri (fun i (term, _, _) ->
        if !idx = -1 && Types.term_eq dom.Domain.sym_compare term old_irr then idx := i)
        rs.behaviors;
      (match !idx with -1 -> () | i ->
        let current_irr, _, _ = List.nth rs.behaviors i in
        match Kbo.kbo dom.Domain.sym_compare new_term current_irr with
        | Kbo.Equal -> ()
        | Kbo.Greater ->
          rs.kbo_rules <- (new_term, current_irr) :: rs.kbo_rules;
          new_kbo_rules := (new_term, current_irr) :: !new_kbo_rules
        | Kbo.Less ->
          let rule = (current_irr, new_term) in
          rs.kbo_rules <- rule :: rs.kbo_rules; new_kbo_rules := rule :: !new_kbo_rules;
          let new_bv = Eval.behavior dom inputs new_term in
          rs.behaviors <- List.mapi (fun j (t', b, e) ->
            if j = i then (new_term, new_bv, []) else (t', b, e)) rs.behaviors
        | Kbo.Incomparable ->
          let new_bv = Eval.behavior dom inputs new_term in
          rs.behaviors <- (new_term, new_bv, []) :: rs.behaviors)
    | D_skip -> ()
    | D_candidate (t, bv, ex) -> candidates := (t, bv, ex) :: !candidates
  ) decisions;
  (!new_size_rules, !new_kbo_rules, !candidates)

let make_examples = Eval.make_examples

let build_bv_index behaviors =
  let h = Hashtbl.create (max 16 (List.length behaviors)) in
  List.iter (fun ((_, bv, _) as entry) ->
    let bucket = try Hashtbl.find h bv with Not_found -> [] in
    Hashtbl.replace h bv (entry :: bucket)) behaviors;
  h

(* Subpass: normalize + decide on a list of enumerated terms.
   Returns (new_size_rules_added, new_kbo_rules_added, candidates). *)
let run_subpass (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets)
      ~all_inputs ~compiled_inputs ~use_smt ~smt_vars ~sym_cmp enumerated =
  let norm_index = Rewrite.index_rules (all_rules rs) in
  let behaviors_by_bv = build_bv_index rs.behaviors in
  let decisions = List.filter_map
    (process_term dom ~inputs:all_inputs ~compiled_inputs
       ~norm_index ~behaviors:rs.behaviors
       ~behaviors_by_bv ~use_smt ~smt_vars ~sym_cmp) enumerated in
  apply_decisions dom rs ~inputs:all_inputs decisions

let profile_enabled =
  try Sys.getenv "RULE_ENUM_PROFILE" = "1" with Not_found -> false

let prof_label name dt =
  if profile_enabled && dt > 0.01 then
    Printf.eprintf "    [%s] %.3fs\n%!" name dt

let run_iteration (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets) (n : int)
      (caps : Enum.caps) ~sym_cmp : 's iter_summary =
  let t_start = Sys.time () in
  let enumerated =
    Enum.enumerate_terms_caps dom.Domain.all_symbols (irreducibles rs) n caps in
  let t_enum = Sys.time () -. t_start in
  prof_label (Printf.sprintf "iter %d enum (%d)" n (List.length enumerated)) t_enum;
  let inputs = rs.inputs in
  let all_inputs = inputs @ if rs.use_smt_forced then List.map (fun vars ->
    List.map (fun (v, n) -> (v, dom.Domain.int_to_val n)) vars) rs.forced_inputs
  else [] in
  if rs.use_smt_forced then
    rs.behaviors <- List.map (fun (irr, _, _) ->
      let bv = Eval.behavior dom all_inputs irr in
      let ex = Eval.make_examples dom all_inputs irr in
      (irr, bv, ex)) rs.behaviors;
  let use_smt = rs.use_smt && inputs = [] in
  let smt_vars = if not (rs.use_smt) then [] else
    let var_names = List.init caps.max_vars Types.var_name in
    let hole_names = List.init caps.max_holes Types.hole_name in
    var_names @ hole_names
  in
  (* Partition enumerated terms: var-only first, then those with Holes. *)
  let var_only, with_holes =
    List.partition (fun t -> not (Types.has_hole t)) enumerated in
  let compiled_inputs = List.map Eval.compile all_inputs in
  let t_sp1 = Sys.time () in
  let sr1, kr1, cands1 = run_subpass dom rs
    ~all_inputs ~compiled_inputs ~use_smt ~smt_vars ~sym_cmp var_only in
  prof_label (Printf.sprintf "subpass1 var (%d)" (List.length var_only)) (Sys.time () -. t_sp1);
  let t_sp2 = Sys.time () in
  let sr2, kr2, cands2 = run_subpass dom rs
    ~all_inputs ~compiled_inputs ~use_smt ~smt_vars ~sym_cmp with_holes in
  prof_label (Printf.sprintf "subpass2 hole (%d)" (List.length with_holes)) (Sys.time () -. t_sp2);
  let new_size_rules = ref (sr1 @ sr2) in
  let new_kbo_rules = ref (kr1 @ kr2) in
  let candidates = cands1 @ cands2 in
  let t_process = Sys.time () -. t_start -. t_enum in
  let new_irreducibles = ref [] in
  let cmp = list_compare dom.Domain.compare in
  let t_dedup = Sys.time () in
  let candidates =
    let seen = Hashtbl.create (List.length candidates) in
    List.filter (fun (t, _, _) ->
      if Hashtbl.mem seen t then false
      else (Hashtbl.add seen t (); true)) candidates
  in
  prof_label (Printf.sprintf "dedup (%d cands)" (List.length candidates)) (Sys.time () -. t_dedup);
  let t_group = Sys.time () in
  let groups = group_by cmp (fun (_, bv, _) -> bv) candidates in
  prof_label (Printf.sprintf "group_by (%d groups)" (List.length groups)) (Sys.time () -. t_group);
  (* Optional SMT-driven subgrouping when SMT is enabled and inputs are empty.
     Same machinery as before; ported to the 3-element tuple. *)
  let groups = if not rs.use_smt || rs.inputs <> [] then groups else
    let cex = ref [] in
    let groups = List.concat_map (fun (bv, term_triples) ->
      let all_terms = List.map (fun (t, _, _) -> t) term_triples in
      match all_terms with [] -> [] | first :: rest ->
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
      List.map (fun g ->
        (bv, List.map (fun t ->
          let ex = make_examples dom all_inputs t in
          (t, bv, ex)) g)) !groups) groups in
    List.iter (fun assigns ->
      rs.forced_inputs <- assigns :: rs.forced_inputs) !cex;
    groups
  in
  let groups =
    if not rs.use_smt || rs.inputs <> [] then groups else
    let cex2 = ref [] in
    let rec merge_all acc = function
      | [] -> acc | (bv, tp) :: rest ->
        let rep, _, _ = List.hd tp in
        let same, diff = List.partition (fun (_, tp2) ->
          let rep2, _, _ = List.hd tp2 in
          match Smt.check_equiv dom smt_vars rep rep2 with
            Smt.Equivalent -> true
          | Smt.CounterExample assigns -> (if rs.use_smt_forced then cex2 := assigns :: !cex2); false
          | _ -> false) rest in
        let merged = List.fold_left (fun a (_, tp2) -> a @ tp2) tp same in
        merge_all ((bv, merged) :: acc) diff
    in
    List.iter (fun assigns -> rs.forced_inputs <- assigns :: rs.forced_inputs) !cex2;
    List.rev (merge_all [] groups)
  in
  let t_kbo_extract = Sys.time () in
  let new_irr_pairs = ref [] in
  List.iter (fun (_bv, term_triples) ->
    (* Cache KBO metadata (size + var_counts) once per term. *)
    let cached = List.map (fun (t, bv, ex) ->
      (Kbo.cache t, bv, ex)) term_triples in
    let all_c = List.map (fun (c, _, _) -> c) cached in
    let minimals = List.filter_map (fun ((t, _, _) as c, _, _) ->
      if not (List.exists (fun s -> Kbo.lt_cached sym_cmp s c) all_c)
      then Some t else None) cached in
    List.iter (fun m ->
      let _, bv, ex = List.find (fun (t, _, _) -> Types.term_eq sym_cmp t m) term_triples in
      new_irreducibles := m :: !new_irreducibles;
      new_irr_pairs := (m, bv, ex) :: !new_irr_pairs) minimals;
    let cached_min = List.map Kbo.cache minimals in
    List.iter (fun ((other, _, _) as oc, _, _) ->
      let is_min = List.exists (fun m -> Types.term_eq sym_cmp other m) minimals in
      if not is_min then
        match List.find_opt (fun m -> Kbo.lt_cached sym_cmp m oc) cached_min with
        | Some (target, _, _) ->
          rs.kbo_rules <- (other, target) :: rs.kbo_rules;
          new_kbo_rules := (other, target) :: !new_kbo_rules
        | None -> ()) cached)
    groups;
  prof_label (Printf.sprintf "kbo-extract (%d groups)" (List.length groups))
    (Sys.time () -. t_kbo_extract);
  let sorted = List.sort (fun (a,_,_) (b,_,_) -> Kbo.compare_total sym_cmp a b) !new_irr_pairs in
  List.iter (fun entry -> rs.behaviors <- entry :: rs.behaviors) (List.rev sorted);
  new_irreducibles := List.map (fun (t, _, _) -> t) sorted;
  { size = n; enumerated = List.length enumerated;
    new_size_rules = List.rev !new_size_rules; new_kbo_rules = List.rev !new_kbo_rules;
    new_irreducibles = List.rev !new_irreducibles;
    total_size_rules = List.length rs.size_rules; total_kbo_rules = List.length rs.kbo_rules;
    total_irreducible = List.length rs.behaviors;
    time_total = Sys.time () -. t_start; time_enum = t_enum;
    time_process = t_process; time_norm = 0.; time_eval = 0.; time_match = 0.;
    time_apply = 0.; time_group = 0.;
  }

(* Build caps from the optional inputs. Defaults: max_vars = max_holes =
   max_vcs (unified bound, the proof's setting). Either or both of
   max_vars/max_holes may be set independently; max_vcs caps their sum. *)
let make_caps ?max_vars ?max_holes ~max_vcs () : Enum.caps =
  let mv = match max_vars with Some n -> n | None -> max_vcs in
  let mh = match max_holes with Some n -> n | None -> max_vcs in
  { max_vars = mv; max_holes = mh; max_vcs }

let run ?max_size ?(forced_inputs = []) ?(on_iteration = fun _ -> ()) ?num_domains:_
      ?(use_smt = false) ?(use_smt_forced = false)
      ?max_vars ?max_holes (dom : ('s, 'a) Domain.t)
      ~num_random_inputs ~max_vcs =
  let caps = make_caps ?max_vars ?max_holes ~max_vcs () in
  let default_max = match max_size with Some m -> m | None -> 12 in
  (* Random inputs need to cover the max possible slot count (vars + holes),
     so we generate for the max of max_vars/max_holes/max_vcs. *)
  let slot_count = max caps.max_vars caps.max_holes |> max caps.max_vcs in
  let random_inputs =
    if num_random_inputs > 0
    then Eval.generate_inputs dom num_random_inputs slot_count else [] in
  let inputs = forced_inputs @ random_inputs in
  if inputs = [] && not use_smt then
    failwith "Algorithm.run: no inputs and SMT disabled";
  let rs = create ~use_smt ~use_smt_forced inputs in
  let results = ref [] in let n = ref 1 in let continue = ref true in
  while !continue && !n <= default_max do
    let summary = run_iteration dom rs !n caps
        ~sym_cmp:dom.Domain.sym_compare in
    if summary.new_size_rules = [] && summary.new_kbo_rules = [] && summary.new_irreducibles = [] then
      continue := false
    else (on_iteration summary; results := summary :: !results; incr n)
  done; (rs, List.rev !results)
