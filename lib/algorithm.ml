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

(* Per-term example cell: mutable list of (input, expected) pairs. Used
   for Tier 2 of the equivalence check. Grows whenever SMT produces a
   counterexample (the assignment + each term's value on it is appended
   to BOTH involved terms' cells), or when cross-eval extends one side
   to cover an input the other side has but it doesn't.

   Stored by reference so updates propagate to every caller holding the
   cell: candidates, post-group reps, and committed irreducibles in
   `rs.behaviors` all share the same cell type. *)
type 'a examples = ('a Eval.input * 'a) list ref
let new_examples () : 'a examples = ref []

type ('s, 'a) rule_sets = {
  mutable size_rules    : 's Types.rule list;
  mutable kbo_rules     : 's Types.rule list;
  mutable behaviors     : ('s Types.term * 'a array * 'a examples) list;
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

(* Tier 2: cross-evaluate two terms on the union of their example inputs.
   For inputs present in both cells, compare the stored values. For
   inputs present in only one cell, evaluate the missing side and append.
   Returns true iff every input in the union agrees. *)
let tier2_cross_eval (dom : ('s, 'a) Domain.t) t1 (ex1 : 'a examples) t2 (ex2 : 'a examples) =
  let lookup lst inp = List.find_map (fun (i, v) -> if i = inp then Some v else None) lst in
  let agree = ref true in
  (* For each entry in ex2: either it's also in ex1 (compare values) or
     we evaluate t1 on it and append. *)
  let to_add_1 = List.filter_map (fun (inp, v2) ->
    match lookup !ex1 inp with
    | Some v1 ->
      if not (dom.Domain.equal v1 v2) then agree := false;
      None
    | None ->
      let v1 = Eval.eval dom inp t1 in
      if not (dom.Domain.equal v1 v2) then agree := false;
      Some (inp, v1)) !ex2 in
  (* For entries in ex1 not in ex2: evaluate t2 and append. Inputs in
     both cells were already checked in the loop above. *)
  let to_add_2 = List.filter_map (fun (inp, v1) ->
    match lookup !ex2 inp with
    | Some _ -> None
    | None ->
      let v2 = Eval.eval dom inp t2 in
      if not (dom.Domain.equal v1 v2) then agree := false;
      Some (inp, v2)) !ex1 in
  ex1 := to_add_1 @ !ex1;
  ex2 := to_add_2 @ !ex2;
  !agree

(* Counters to verify Tier 2 actually saves SMT work. *)
let tier_calls = ref 0
let tier2_short_circuit = ref 0
let tier3_calls = ref 0
let tier3_cex_added = ref 0

(* Tier 3: invoke SMT after Tier 2 has confirmed cell agreement.
   On CounterExample: convert the assignment via dom.int_to_val,
   evaluate both terms on it, append (input, v) to BOTH cells. *)
let tier3_smt (dom : ('s, 'a) Domain.t) ~smt_vars t1 (ex1 : 'a examples) t2 (ex2 : 'a examples) =
  incr tier3_calls;
  match Smt.check_equiv dom smt_vars t1 t2 with
  | Smt.Equivalent -> true
  | Smt.CounterExample assigns ->
    incr tier3_cex_added;
    (* SMT may omit assignments for vars whose value doesn't matter for
       SAT. Pad missing names from smt_vars with 0 so eval never fails. *)
    let padded = List.map (fun n ->
      match List.assoc_opt n assigns with
      | Some i -> (n, dom.Domain.int_to_val i)
      | None -> (n, dom.Domain.int_to_val 0)) smt_vars in
    let v1 = Eval.eval dom padded t1 in
    let v2 = Eval.eval dom padded t2 in
    ex1 := (padded, v1) :: !ex1;
    ex2 := (padded, v2) :: !ex2;
    false
  | Smt.Unknown -> false

(* Combined Tier 2 + Tier 3 check. Returns true iff t1 ≡ t2.
   - Tier 2 (cross-eval) extends both cells and short-circuits on disagreement.
   - Tier 3 (SMT) only fires if Tier 2 agrees; counterexamples grow both cells. *)
let confirm_equiv ~use_smt ~smt_vars dom t1 ex1 t2 ex2 =
  incr tier_calls;
  if not (tier2_cross_eval dom t1 ex1 t2 ex2) then
    (incr tier2_short_circuit; false)
  else if not use_smt then true
  else tier3_smt dom ~smt_vars t1 ex1 t2 ex2

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

(* A rule decision also records the candidate triple (simplified term,
   bv, cell) and the underlying source irreducible term. SMT confirms
   `simplified ≡ irr_src`; for var-form rules this IS the rule, for the
   constP fallback the rule is (vars_to_holes simplified, vars_to_holes
   irr_src) which is logically equivalent. If SMT rejects the rule
   (random false positive), the candidate triple gets demoted to
   D_candidate-equivalent and becomes a new irreducible. *)
type ('s, 'a) term_decision =
  | D_size_rule of 's Types.rule * ('s Types.term * 'a array * 'a examples) * 's Types.term
  | D_kbo_rule  of 's Types.rule * ('s Types.term * 'a array * 'a examples) * 's Types.term
  | D_replace   of 's Types.term * 's Types.term * 'a array * 'a examples
  | D_skip
  | D_candidate of 's Types.term * 'a array * 'a examples

type match_kind = Size | Kbo | Replace | Skip

(* `equiv_irrs_anon`: like `equiv_irrs` but uses an "anon-aware" bucket
   where each entry is `(form, primary_entry)`. The same source primary
   can appear under multiple bv keys (its anon variants). We dedupe by
   primary so find_best doesn't see the same source multiple times when
   the candidate happens to share its primary bv with one of its own
   anon-variant bvs. *)
let process_term (dom : ('s, 'a) Domain.t) ~inputs:_ ~compiled_inputs_arr
      ~norm_index ~behaviors ~behaviors_by_bv
      ~use_smt ~smt_vars:_ ~sym_cmp t =
  let prefer a b = if Kbo.compare_total sym_cmp a b < 0 then a else b in
  let equiv_irrs _simplified ex bv =
    let candidates =
      if Array.length bv = 0 then
        if not use_smt then []
        else List.filter (fun (_, irr_bv, _) -> Array.length irr_bv = 0) behaviors
      else match Hashtbl.find_opt behaviors_by_bv bv with
        | Some irrs -> irrs
        | None -> []
    in
    (* Tier 2 / 3 are deferred to rule-emission time (see
       `apply_decisions` / post-group code), where we have both terms'
       cells in scope and can mutate them. *)
    let _ = ex in
    candidates
    (* Tier 3 (SMT) is deferred to rule-emission time inside
       `apply_decisions`. Calling SMT here would mean O(candidates ×
       bucket_size) calls — far more than needed. Instead we run SMT
       only when a rule is about to be committed: random's bv-match is
       a cheap prefilter, KBO determines the rule direction, then SMT
       confirms the (LHS, RHS) pair really are equivalent. *)
  in
  (* `find_best`:
     For each candidate equivalent irreducible, choose the best KBO
     decision. Var rules (general substitution) are preferred when
     KBO orders the schemas. When var-KBO returns Incomparable, fall
     back to the constP form (Var i → Hole i; the result is NoVar so
     KBO is total) and, if that orders, store the *constP version* of
     the candidate term to emit a hole rule from the var-form pair.

     Returns `Some ((decision_irr, kind), constP_pair)` where
     `constP_pair` is `Some (lhs_hole, rhs_hole)` for constP rules
     (Kbo|Replace kind, found via the constP fallback), `None`
     otherwise. *)
  let find_best simplified candidates =
    let simp_c = Kbo.cache simplified in
    let (_, t_sz, _) = simp_c in
    let simp_hole = lazy (Types.canonicalize (Types.vars_to_holes simplified)) in
    let cache_hole = lazy (Kbo.cache (Lazy.force simp_hole)) in
    let rec loop best best_const = function
      | [] -> (best, best_const)
      | (irr, _irr_bv, _irr_ex) :: rest ->
          let irr_c = Kbo.cache irr in
          let (_, irr_sz, _) = irr_c in
          match Kbo.kbo_cached sym_cmp irr_c simp_c with
          | Kbo.Equal ->
            (match best with
             | Some (_, Size) -> loop best best_const rest
             | _ -> loop (Some (irr, Skip)) best_const rest)
          | Kbo.Less when irr_sz < t_sz ->
            (match best with
             | None -> loop (Some (irr, Size)) best_const rest
             | Some (prev, Size) -> loop (Some (prefer irr prev, Size)) best_const rest
             | Some (_, (Kbo | Replace | Skip)) -> loop (Some (irr, Size)) best_const rest)
          | Kbo.Less ->
            (match best with
             | None -> loop (Some (irr, Kbo)) best_const rest
             | Some (_, Size) -> loop best best_const rest
             | Some (prev, Kbo) -> loop (Some (prefer irr prev, Kbo)) best_const rest
             | Some (_, (Replace | Skip)) -> loop (Some (irr, Kbo)) best_const rest)
          | Kbo.Greater ->
            (match best with
             | None -> loop (Some (irr, Replace)) best_const rest
             | Some (_, Size) -> loop best best_const rest
             | Some (_, Kbo) -> loop best best_const rest
             | Some (prev, Replace) -> loop (Some (prefer irr prev, Replace)) best_const rest
             | Some (_, Skip) -> loop (Some (irr, Replace)) best_const rest)
          | Kbo.Incomparable ->
            (* Var-KBO can't orient (var-count gate fails or distinct
               vars at same lex position). Try the constP version: both
               sides become NoVar, so KBO is total. *)
            let irr_hole = Types.canonicalize (Types.vars_to_holes irr) in
            let irr_hole_c = Kbo.cache irr_hole in
            (match Kbo.kbo_cached sym_cmp irr_hole_c (Lazy.force cache_hole) with
             | Kbo.Less ->
               let candidate_pair = (Lazy.force simp_hole, irr_hole) in
               let kind = if irr_sz < t_sz then Size else Kbo in
               (match best_const with
                | None -> loop best (Some (candidate_pair, kind, irr)) rest
                | Some _ -> loop best best_const rest)
             | _ -> loop best best_const rest)
    in
    let (b, bc) = loop None None candidates in
    (b, bc)
  in
  let decide ex bv simplified =
    let cand = (simplified, bv, ex) in
    match find_best simplified (equiv_irrs simplified ex bv) with
    | (None, None) -> Some (D_candidate (simplified, bv, ex))
    | (None, Some ((lhs_hole, rhs_hole), Size, irr_src)) ->
      Some (D_size_rule ((lhs_hole, rhs_hole), cand, irr_src))
    | (None, Some ((lhs_hole, rhs_hole), Kbo, irr_src)) ->
      Some (D_kbo_rule ((lhs_hole, rhs_hole), cand, irr_src))
    | (Some (irr, Size), _) -> Some (D_size_rule ((simplified, irr), cand, irr))
    | (Some (irr, Kbo), _) -> Some (D_kbo_rule ((simplified, irr), cand, irr))
    | (Some (irr, Replace), _) -> Some (D_replace (irr, simplified, bv, ex))
    | (Some (_, Skip), _) -> Some D_skip
    | (None, Some (_, (Replace | Skip), _)) ->
      Some (D_candidate (simplified, bv, ex))
  in
  (* Inputs come from enumerate_terms_caps which produces canonical
     terms. `normalize_canonical_or_skip` short-circuits as soon as it
     detects a strict size reduction (we'd skip the term anyway, so no
     need to finish rewriting/canonicalize). *)
  match Rewrite.normalize_canonical_or_skip ~sym_cmp ~index:norm_index t with
  | None -> None  (* size reduced; term filtered out *)
  | Some (simplified, _changed) ->
    let bv = Eval.behavior_compiled_arr dom compiled_inputs_arr simplified in
    let ex = new_examples () in
    decide ex bv simplified

(* Hashtbl-based seen set for rule dedup. Polymorphic structural equality
   on (term, term) pairs — fine since terms have no functions. *)
let make_rule_seen rules =
  let h = Hashtbl.create (max 16 (2 * List.length rules)) in
  List.iter (fun r -> Hashtbl.replace h r ()) rules;
  h

let apply_decisions (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets)
      ~inputs ~size_seen ~kbo_seen ~use_smt ~smt_vars
      (decisions : ('s, 'a) term_decision list) =
  let new_size_rules = ref [] in let new_kbo_rules = ref [] in let candidates = ref [] in
  (* Build a term → cell lookup so SMT counterexamples accumulate into
     the persistent irreducible's cell instead of being discarded. *)
  let cell_of_irr =
    let h = Hashtbl.create (max 16 (List.length rs.behaviors)) in
    List.iter (fun (t, _, ex) -> Hashtbl.replace h t ex) rs.behaviors;
    fun t -> match Hashtbl.find_opt h t with
      | Some ex -> ex
      | None -> new_examples ()
  in
  (* Tier 2 cross-eval + Tier 3 SMT. Both cells get updated on every
     SMT call (counterexample assignment + each term's value on it). *)
  let confirms cand_ex src_lhs src_rhs =
    let rhs_ex = cell_of_irr src_rhs in
    confirm_equiv ~use_smt ~smt_vars dom src_lhs cand_ex src_rhs rhs_ex
  in
  (* If SMT rejects a rule (random false positive), the candidate is
     demoted to a new irreducible. Without this, low-random runs would
     silently lose many genuinely-new irreducibles. *)
  let commit_size_rule cand rule src_lhs src_rhs =
    let (simplified, bv, ex) = cand in
    if Hashtbl.mem size_seen rule then ()
    else if confirms ex src_lhs src_rhs then begin
      Hashtbl.add size_seen rule ();
      rs.size_rules <- rule :: rs.size_rules;
      new_size_rules := rule :: !new_size_rules
    end else
      candidates := (simplified, bv, ex) :: !candidates
  in
  let commit_kbo_rule cand rule src_lhs src_rhs =
    let (simplified, bv, ex) = cand in
    if Hashtbl.mem kbo_seen rule then ()
    else if confirms ex src_lhs src_rhs then begin
      Hashtbl.add kbo_seen rule ();
      rs.kbo_rules <- rule :: rs.kbo_rules;
      new_kbo_rules := rule :: !new_kbo_rules
    end else
      candidates := (simplified, bv, ex) :: !candidates
  in
  List.iter (function
    | D_size_rule (rule, cand, irr_src) ->
      commit_size_rule cand rule (fst rule) irr_src
    | D_kbo_rule (rule, cand, irr_src) ->
      commit_kbo_rule cand rule (fst rule) irr_src
    | D_replace (old_irr, new_term, stored_bv, ex) ->
      let cand = (new_term, stored_bv, ex) in
      let idx = ref (-1) in
      List.iteri (fun i (term, _, _) ->
        if !idx = -1 && Types.term_eq dom.Domain.sym_compare term old_irr then idx := i)
        rs.behaviors;
      (match !idx with -1 -> () | i ->
        let current_irr, _, current_ex = List.nth rs.behaviors i in
        match Kbo.kbo dom.Domain.sym_compare new_term current_irr with
        | Kbo.Equal -> ()
        | Kbo.Greater ->
          commit_kbo_rule cand (new_term, current_irr) new_term current_irr
        | Kbo.Less ->
          let rule = (current_irr, new_term) in
          commit_kbo_rule cand rule new_term current_irr;
          let new_bv = Eval.behavior dom inputs new_term in
          (* new_term ≡ old_irr was SMT-confirmed inside commit_kbo_rule;
             safe to inherit the accumulated cell. *)
          rs.behaviors <- List.mapi (fun j (t', b, e) ->
            if j = i then (new_term, new_bv, current_ex) else (t', b, e)) rs.behaviors
        | Kbo.Incomparable ->
          let new_bv = Eval.behavior dom inputs new_term in
          rs.behaviors <- (new_term, new_bv, ex) :: rs.behaviors)
    | D_skip -> ()
    | D_candidate (t, bv, ex) -> candidates := (t, bv, ex) :: !candidates
  ) decisions;
  (!new_size_rules, !new_kbo_rules, !candidates)

let build_bv_index behaviors =
  let h = Hashtbl.create (max 16 (List.length behaviors)) in
  List.iter (fun ((_, bv, _) as entry) ->
    let bucket = try Hashtbl.find h bv with Not_found -> [] in
    Hashtbl.replace h bv (entry :: bucket)) behaviors;
  h

(* Bucket including anon variants. Each entry is `(form, source, bv, ex)`.
   For each irreducible T, we insert (T, T, bv_T, ex) — the primary —
   plus one entry per non-trivial anon variant T_S with its own bv_S.
   The `source` always points back to the var-canonical irreducible
   from which the form was derived. This lets candidates look up
   anon-variant bvs and find equivalences across DIFFERENT source IRs
   (the Cat A/C case). *)
let build_bv_index_with_anons dom inputs behaviors =
  let h = Hashtbl.create (max 16 (8 * List.length behaviors)) in
  List.iter (fun ((t, primary_bv, _ex) as primary) ->
    let bucket = try Hashtbl.find h primary_bv with Not_found -> [] in
    Hashtbl.replace h primary_bv ((t, primary) :: bucket);
    let variants = Types.anonymization_variants t in
    List.iter (fun v ->
      if not (Types.term_eq dom.Domain.sym_compare v t) then begin
        let bv_v = Eval.behavior dom inputs v in
        let bucket = try Hashtbl.find h bv_v with Not_found -> [] in
        Hashtbl.replace h bv_v ((v, primary) :: bucket)
      end) variants) behaviors;
  h

(* Subpass: normalize + decide on a list of enumerated terms.
   Returns (new_size_rules_added, new_kbo_rules_added, candidates). *)
(* Parallel `List.filter_map` for the process_term loop.

   process_term is pure on its inputs (norm_index, behaviors,
   behaviors_by_bv, compiled_inputs are all built before the loop and
   not mutated during it). Decisions are accumulated in independent
   chunks and applied serially afterwards.

   Below `parallel_threshold` items we skip the Domain.spawn overhead
   and fall back to sequential. Worker count comes from the explicit
   `~num_domains` argument; the env var `RULE_ENUM_JOBS` and the
   system's recommended-domain count are used as fallbacks when callers
   don't specify. *)
let parallel_threshold = 1500

let resolve_num_workers num_domains =
  match num_domains with
  | Some n when n > 0 -> n
  | _ ->
    match Sys.getenv_opt "RULE_ENUM_JOBS" with
    | Some s -> (try max 1 (int_of_string s) with _ -> 1)
    | None ->
      try Stdlib.Domain.recommended_domain_count () with _ -> 1

let parallel_filter_map ~num_domains f lst =
  let len = List.length lst in
  let nd = num_domains in
  if nd <= 1 || len < parallel_threshold * 2 then List.filter_map f lst
  else
    let chunk_size = (len + nd - 1) / nd in
    let rec take n acc = function
      | [] -> (List.rev acc, [])
      | rest when n <= 0 -> (List.rev acc, rest)
      | x :: xs -> take (n - 1) (x :: acc) xs in
    let rec split acc = function
      | [] -> List.rev acc
      | rest -> let ch, rem = take chunk_size [] rest in split (ch :: acc) rem in
    let chunks = split [] lst in
    let domains = List.map (fun ch ->
      Stdlib.Domain.spawn (fun () -> List.filter_map f ch)) chunks in
    List.concat (List.map Stdlib.Domain.join domains)

let run_subpass (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets)
      ~all_inputs ~compiled_inputs_arr ~size_seen ~kbo_seen
      ~num_domains ~use_smt ~smt_vars ~sym_cmp enumerated =
  let norm_index = Rewrite.index_rules (all_rules rs) in
  let behaviors_by_bv = build_bv_index rs.behaviors in
  let f = process_term dom ~inputs:all_inputs ~compiled_inputs_arr
            ~norm_index ~behaviors:rs.behaviors
            ~behaviors_by_bv ~use_smt ~smt_vars ~sym_cmp in
  let decisions = parallel_filter_map ~num_domains f enumerated in
  apply_decisions dom rs ~inputs:all_inputs ~size_seen ~kbo_seen
    ~use_smt ~smt_vars decisions

let profile_enabled =
  try Sys.getenv "RULE_ENUM_PROFILE" = "1" with Not_found -> false

let prof_label name dt =
  if profile_enabled && dt > 0.01 then
    Printf.eprintf "    [%s] %.3fs\n%!" name dt

let run_iteration (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets) (n : int)
      (caps : Enum.caps) ~num_domains ~sym_cmp : 's iter_summary =
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
      let ex = ref (Eval.make_examples dom all_inputs irr) in
      (irr, bv, ex)) rs.behaviors;
  (* SMT is the Tier-3 correctness check: random-bv is Tier 1 (cheap
     prefilter), per-term examples is Tier 2 (currently empty for the
     fast path), SMT confirms equivalence when the cheaper tiers agree.
     We run SMT *whenever* the user requested it, regardless of whether
     random inputs are also present — random just prefilters which
     candidate pairs need SMT. *)
  let use_smt = rs.use_smt in
  let smt_vars = if not use_smt then [] else
    (* Declare names for BOTH var and hole IDs up to max_vcs.
       Even with max_holes=0 in enumeration, post-group anon-form rules
       use Hole IDs; SMT must be able to encode them. *)
    let k = max caps.max_vars caps.max_holes |> max caps.max_vcs in
    let var_names = List.init k Types.var_name in
    let hole_names = List.init k Types.hole_name in
    var_names @ hole_names
  in
  let _ = inputs in
  (* Partition enumerated terms: var-only first, then those with Holes. *)
  let var_only, with_holes =
    List.partition (fun t -> not (Types.has_hole t)) enumerated in
  let compiled_inputs_arr = Array.of_list (List.map Eval.compile all_inputs) in
  let size_seen = make_rule_seen rs.size_rules in
  let kbo_seen = make_rule_seen rs.kbo_rules in
  let t_sp1 = Sys.time () in
  let sr1, kr1, cands1 = run_subpass dom rs
    ~all_inputs ~compiled_inputs_arr ~size_seen ~kbo_seen
    ~num_domains ~use_smt ~smt_vars ~sym_cmp var_only in
  prof_label (Printf.sprintf "subpass1 var (%d)" (List.length var_only)) (Sys.time () -. t_sp1);
  let t_sp2 = Sys.time () in
  let sr2, kr2, cands2 = run_subpass dom rs
    ~all_inputs ~compiled_inputs_arr ~size_seen ~kbo_seen
    ~num_domains ~use_smt ~smt_vars ~sym_cmp with_holes in
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
  (* SMT-driven subgrouping: within each bv-bucket, Tier 2 (cross-eval
     on accumulated examples) then Tier 3 (SMT) confirm which terms are
     TRULY equivalent. Random bv may produce false-positive "equivalent"
     groupings (different functions that happen to agree on the random
     sample); the combined check splits these into distinct subgroups.
     Every SMT counterexample is appended to both terms' example cells. *)
  let groups = if not rs.use_smt then groups else
    List.concat_map (fun (bv, term_triples) ->
      match term_triples with [] -> [] | first :: rest ->
      let groups = ref [[first]] in
      List.iter (fun ((t, _, t_ex) as triple) ->
        let idx = ref (-1) in
        List.iteri (fun i g ->
          if !idx = -1 then
            let (rep, _, rep_ex) = List.hd g in
            if confirm_equiv ~use_smt ~smt_vars dom t t_ex rep rep_ex then idx := i)
          !groups;
        match !idx with
        | -1 -> groups := [triple] :: !groups
        | i -> groups := List.mapi (fun j g -> if j = i then triple :: g else g) !groups)
        rest;
      List.map (fun g -> (bv, g)) !groups)
    groups
  in
  (* Cross-bv merging: cells let us cross-eval reps from different bvs
     and only invoke SMT when Tier 2 agrees. With non-empty random inputs,
     two truly equivalent terms agree on every random sample with
     overwhelming probability, so cross-bv merging adds O(g²) SMT calls
     for negligible benefit. Run only when random inputs are absent. *)
  let groups =
    if not rs.use_smt || rs.inputs <> [] then groups else
    let rec merge_all acc = function
      | [] -> acc | (bv, tp) :: rest ->
        let (rep, _, rep_ex) = List.hd tp in
        let same, diff = List.partition (fun (_, tp2) ->
          let (rep2, _, rep2_ex) = List.hd tp2 in
          confirm_equiv ~use_smt ~smt_vars dom rep rep_ex rep2 rep2_ex) rest in
        let merged = List.fold_left (fun a (_, tp2) -> a @ tp2) tp same in
        merge_all ((bv, merged) :: acc) diff
    in
    List.rev (merge_all [] groups)
  in
  let t_kbo_extract = Sys.time () in
  let new_irr_pairs = ref [] in
  List.iter (fun (_bv, term_triples) ->
    (* Compute the fully-anonymized form (vars → holes, canonicalize) of
       each term, then expand to the hole-renaming orbit. With Hole
       treated as a constP (linear-ordered, NOT renaming-equivalent),
       orbit members are distinct canonical terms. For commutative ops
       the orbit members evaluate identically (same anon-bv) — we keep
       them, so the winner-extraction emits a commutativity rule. For
       non-commutative ops the orbit members have distinct anon-bvs;
       we filter them out so no spurious rule is emitted. *)
    let with_anon = List.concat_map (fun (t, bv, ex) ->
      let anon = Types.canonicalize (Types.vars_to_holes t) in
      let anon_bv = Eval.behavior_compiled_arr dom compiled_inputs_arr anon in
      let orbit = Types.hole_permutations anon in
      List.filter_map (fun a ->
        let a_bv =
          if a == anon then anon_bv
          else Eval.behavior_compiled_arr dom compiled_inputs_arr a in
        if a_bv = anon_bv then
          let a_c = Kbo.cache a in
          Some (t, bv, ex, a, a_c)
        else None) orbit)
      term_triples
    in
    (* Pick the minimum under anon-form KBO; ties broken by compare_total
       on the SOURCE terms (var-form sorts smaller than hole-form, so we
       prefer var-canonical winners over hole-canonical when they
       represent the same anon-form). *)
    let winner = match with_anon with
      | [] -> failwith "empty group"
      | first :: rest ->
        List.fold_left (fun acc cur ->
          let (acc_t, _, _, _, acc_c) = acc in
          let (cur_t, _, _, _, cur_c) = cur in
          match Kbo.kbo_cached sym_cmp cur_c acc_c with
          | Kbo.Less -> cur
          | Kbo.Greater -> acc
          | Kbo.Equal | Kbo.Incomparable ->
            if Kbo.compare_total sym_cmp cur_t acc_t < 0 then cur else acc)
          first rest
    in
    let (w_t, w_bv, w_ex, w_anon, _) = winner in
    new_irreducibles := w_t :: !new_irreducibles;
    new_irr_pairs := (w_t, w_bv, w_ex) :: !new_irr_pairs;
    (* For each non-winner: emit a rule.
       - If var-KBO orders other → winner, emit var rule.
       - Else emit anon-form rule (other_anon → winner_anon). The anon
         form uses Holes; thanks to match_var_const accepting Var as a
         size-0 image, the rule still fires on var-containing targets
         at normalize time. *)
    let collect_holes t =
      let h = Hashtbl.create 4 in
      let rec go = function
        | Types.Hole n -> Hashtbl.replace h n ()
        | Types.Var _ -> ()
        | Types.Node (_, args) -> List.iter go args
      in go t;
      Hashtbl.fold (fun k () acc -> k :: acc) h []
    in
    let holes_subset_le rhs lhs =
      let lhs_holes = List.sort_uniq compare (collect_holes lhs) in
      let rhs_holes = List.sort_uniq compare (collect_holes rhs) in
      List.for_all (fun h -> List.mem h lhs_holes) rhs_holes
    in
    let var_set_le rhs lhs =
      let lhs_vars =
        let s = Hashtbl.create 4 in
        let rec go = function
          | Types.Var v -> Hashtbl.replace s v ()
          | Types.Hole _ -> ()
          | Types.Node (_, args) -> List.iter go args
        in go lhs;
        Hashtbl.fold (fun k () acc -> k :: acc) s []
      in
      let rhs_vars =
        let s = Hashtbl.create 4 in
        let rec go = function
          | Types.Var v -> Hashtbl.replace s v ()
          | Types.Hole _ -> ()
          | Types.Node (_, args) -> List.iter go args
        in go rhs;
        Hashtbl.fold (fun k () acc -> k :: acc) s []
      in
      List.for_all (fun v -> List.mem v lhs_vars) rhs_vars
    in
    List.iter (fun (other_t, _, other_ex, other_anon, _) ->
      (* Skip only the winner's own (source, anon) entry. Orbit expansion
         produces multiple entries with the same source `t` but different
         anon forms — those non-winning anons need rules emitted (this is
         how commutativity rules surface). *)
      let same_as_winner =
        Types.term_eq sym_cmp other_t w_t
        && Types.term_eq sym_cmp other_anon w_anon
      in
      if not same_as_winner then begin
        let other_c = Kbo.cache other_t in
        let w_c = Kbo.cache w_t in
        let rule =
          match Kbo.kbo_cached sym_cmp w_c other_c with
          | Kbo.Less when var_set_le w_t other_t && holes_subset_le w_t other_t ->
            Some (other_t, w_t)
          | _ ->
            if Types.term_eq sym_cmp other_anon w_anon then None
            else if holes_subset_le w_anon other_anon
                 && var_set_le w_anon other_anon
            then Some (other_anon, w_anon)
            else None
        in
        (* Tier 2 + Tier 3 on the source var-form pair (other_t, w_t).
           If a counterexample comes back, both cells grow. *)
        let confirms () =
          confirm_equiv ~use_smt ~smt_vars dom other_t other_ex w_t w_ex in
        match rule with
        | Some r when not (Hashtbl.mem kbo_seen r) && confirms () ->
          Hashtbl.add kbo_seen r ();
          rs.kbo_rules <- r :: rs.kbo_rules;
          new_kbo_rules := r :: !new_kbo_rules
        | _ -> ()
      end) with_anon)
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

let run ?max_size ?(forced_inputs = []) ?(on_iteration = fun _ -> ()) ?num_domains
      ?(use_smt = false) ?(use_smt_forced = false)
      ?max_vars ?max_holes (dom : ('s, 'a) Domain.t)
      ~num_random_inputs ~max_vcs =
  let caps = make_caps ?max_vars ?max_holes ~max_vcs () in
  let default_max = match max_size with Some m -> m | None -> 12 in
  let workers = resolve_num_workers num_domains in
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
        ~num_domains:workers
        ~sym_cmp:dom.Domain.sym_compare in
    if summary.new_size_rules = [] && summary.new_kbo_rules = [] && summary.new_irreducibles = [] then
      continue := false
    else (on_iteration summary; results := summary :: !results; incr n)
  done; (rs, List.rev !results)

(* Expose resolution for callers that need to print the actual job count. *)
let effective_num_workers = resolve_num_workers
