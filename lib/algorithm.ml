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
  (* Rules emitted on ASSUMED (not SMT-proven) equivalence: SMT returned
     Unknown and extra random sampling failed to refute, so we trusted
     random confidence. Tracked separately so they can be reported and,
     in safe mode, suppressed. *)
  mutable assumed_rules : 's Types.rule list;
  (* Candidate equivalences SKIPPED in safe mode: SMT returned Unknown
     and random could not refute, but `assume_unproven` is false so we
     declined to emit the rule. Logged so the user sees what was dropped. *)
  mutable skipped_rules : 's Types.rule list;
  inputs              : 'a Eval.input list;
  use_smt             : bool;
  use_smt_forced      : bool;
  (* When false (safe mode), an unproven equivalence is treated as
     not-equivalent rather than assumed: no rule is emitted from it. *)
  assume_unproven     : bool;
  (* Persistent behavior-vector index of all committed irreducibles,
     keyed by bv. Maintained incrementally (winners appended at each
     iteration's end) instead of rebuilt per subpass — O(new) rather than
     O(|behaviors|) each time. `bv_index_dirty` forces a full rebuild when
     the invariant `bv_index = build_bv_index behaviors` can be broken:
     the --smt-forced remap recomputes every bv. *)
  mutable bv_index    : ('a array, ('s Types.term * 'a array * 'a examples) list) Hashtbl.t;
  mutable bv_index_dirty : bool;
}

let create ~use_smt ~use_smt_forced ?(assume_unproven = true) inputs = {
  size_rules = []; kbo_rules = []; behaviors = []; forced_inputs = [];
  assumed_rules = []; skipped_rules = [];
  inputs; use_smt; use_smt_forced; assume_unproven;
  bv_index = Hashtbl.create 1024; bv_index_dirty = false;
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
let tier3_unknown = ref 0          (* SMT returned Unknown (e.g. timeout) *)
let tier3_unknown_refuted = ref 0  (* extra random sampling refuted equivalence *)
(* Counters for post-group orbit-cache instrumentation. *)
let anon_total = ref 0
let anon_distinct = ref 0

(* Number of extra random inputs to draw when SMT returns Unknown.
   Initialized from RULE_ENUM_SMT_UNKNOWN_INPUTS; `run` overrides it from
   its ?unknown_inputs argument (CLI --smt-unknown-inputs). *)
let unknown_extra_inputs =
  ref (match Sys.getenv_opt "RULE_ENUM_SMT_UNKNOWN_INPUTS" with
       | Some s -> (try max 0 (int_of_string s) with _ -> 1000)
       | None -> 1000)

(* Enumerated-term counts of the most recent sizes (most-recent first),
   used to estimate the next size's count for the enumeration progress
   bar. Enumeration has no known total until it finishes, and predicting
   it exactly is ~as costly as enumerating; the count grows roughly
   geometrically, so we extrapolate from the last two sizes. Reset per
   `run`. *)
let enum_history = ref []
let estimate_next_enum () =
  match !enum_history with
  | b :: a :: _ when a > 0 -> b * b / a   (* geometric: b * (b/a) *)
  | b :: _ -> b * 3                        (* one point: assume ~3x growth *)
  | [] -> 0

(* Equivalence verdict.
   - `Proven`:   SMT proved equivalence (or no-SMT mode, random is oracle).
   - `Assumed`:  SMT Unknown, extra random couldn't refute, assume_unproven
                 → treated as equivalent on random confidence.
   - `Unproven`: SMT Unknown, extra random couldn't refute, safe mode
                 → NOT treated as equivalent; the candidate rule is logged
                 as skipped.
   - `Not_equiv`: refuted by SMT counterexample or random. *)
type verdict = Not_equiv | Proven | Assumed | Unproven

(* On SMT Unknown, draw `!unknown_extra_inputs` fresh random assignments
   and evaluate both terms. If any distinguishes them they are NOT
   equivalent (record the witness in both cells so future comparisons
   short-circuit at Tier 2). If none does, the verdict depends on
   `assume_unproven`: when true, fall back to random confidence and
   return Assumed; when false (safe mode), return Unproven. *)
let tier3_unknown_fallback ~assume_unproven (dom : ('s, 'a) Domain.t) ~smt_vars t1 ex1 t2 ex2 =
  incr tier3_unknown;
  let k = max 1 (List.length smt_vars / 2) in
  let extra = dom.Domain.generate_inputs !unknown_extra_inputs k in
  let rec scan = function
    | [] -> if assume_unproven then Assumed else Unproven
    | inp :: rest ->
      let v1 = (try Eval.eval dom inp t1 with _ -> dom.Domain.int_to_val 0) in
      let v2 = (try Eval.eval dom inp t2 with _ -> dom.Domain.int_to_val 0) in
      if dom.Domain.equal v1 v2 then scan rest
      else begin
        incr tier3_unknown_refuted;
        ex1 := (inp, v1) :: !ex1;
        ex2 := (inp, v2) :: !ex2;
        Not_equiv
      end
  in scan extra

(* Exhaustive equivalence over a small finite domain. Enumerates every
   assignment of the domain's value set to the distinct leaves of t1/t2 and
   checks the two terms agree on all of them — an EXACT oracle (sound and
   complete) that needs no Z3. Returns None when unavailable (infinite
   domain) or the assignment space `|values|^|leaves|` exceeds the budget,
   in which case the caller falls back to SMT. Pure: no shared state, safe
   to run from worker domains. *)
(* Max assignment space (|values|^|leaves|) we'll exhaustively enumerate.
   Pairs whose space exceeds this return None ("too big") instead of being
   enumerated — crucial for speed: a pair spanning many distinct leaves
   (e.g. a var-term vs a hole-term that collided in a bv-bucket) would
   otherwise enumerate a huge space. Genuine equivalences span ≤ max_vcs
   leaves, so as long as |values|^max_vcs fits here they are always
   decided; None therefore implies non-equivalence (or an already-reducible
   constant), which callers treat accordingly. *)
let exhaustive_max_combos = 300_000
(* Atomic: incremented from worker domains during the parallel
   fully-exhaustive confirmation, so a plain ref would race. *)
let exhaustive_calls = Atomic.make 0

let distinct_leaf_names t1 t2 =
  let tbl = Hashtbl.create 8 in
  let rec go = function
    | Types.Var v -> Hashtbl.replace tbl (Types.var_name v) ()
    | Types.Hole n -> Hashtbl.replace tbl (Types.hole_name n) ()
    | Types.Node (_, args) -> List.iter go args
  in go t1; go t2;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

(* Exact equivalence by exhaustive evaluation over the domain's finite value
   set, applied to the union of t1/t2's distinct leaves. Short-circuits on
   the first counterexample. Returns Some true / Some false when decided,
   or None when the assignment space is too large (caller falls back).
   Pure: safe to run in workers. *)
let exhaustive_equiv (dom : ('s, 'a) Domain.t) t1 t2 : bool option =
  match dom.Domain.values with
  | None -> None
  | Some vals ->
    let names = Array.of_list (distinct_leaf_names t1 t2) in
    let vals = Array.of_list vals in
    let nv = Array.length vals and d = Array.length names in
    let rec pow acc i =
      if acc > exhaustive_max_combos then acc
      else if i = 0 then acc else pow (acc * nv) (i - 1) in
    if d > 0 && pow 1 d > exhaustive_max_combos then None
    else begin
      Atomic.incr exhaustive_calls;
      let idx = Array.make (max d 1) 0 in
      let equal = ref true and continue = ref true in
      while !continue && !equal do
        let assign =
          Array.to_list (Array.mapi (fun i name -> (name, vals.(idx.(i)))) names) in
        if not (dom.Domain.equal (Eval.eval dom assign t1) (Eval.eval dom assign t2))
        then equal := false;
        (* odometer over d digits, base nv (d=0 ⇒ single ground check) *)
        let rec inc i =
          if i < 0 then continue := false
          else if idx.(i) + 1 < nv then idx.(i) <- idx.(i) + 1
          else (idx.(i) <- 0; inc (i - 1)) in
        inc (d - 1)
      done;
      Some !equal
    end

(* Tier 3: invoke SMT after Tier 2 has confirmed cell agreement.
   On CounterExample: convert the assignment via dom.int_to_val,
   evaluate both terms on it, append (input, v) to BOTH cells. *)
let tier3_smt ~assume_unproven (dom : ('s, 'a) Domain.t) ~smt_vars t1 (ex1 : 'a examples) t2 (ex2 : 'a examples) =
  incr tier3_calls;
  match Smt.check_equiv dom smt_vars t1 t2 with
  | Smt.Equivalent -> Proven
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
    Not_equiv
  | Smt.Unknown -> tier3_unknown_fallback ~assume_unproven dom ~smt_vars t1 ex1 t2 ex2

(* Combined Tier 2 + Tier 3 check. Returns the equivalence verdict.
   - Tier 2 (cross-eval) extends both cells and short-circuits on disagreement.
   - Tier 3 (SMT) only fires if Tier 2 agrees; counterexamples grow both cells.
   Without SMT, agreement on the cells counts as Proven (random is the
   only oracle in that mode). *)
(* `smt_ok`: when false (parallel/worker context), NEVER fall through to
   Z3 — its global memory manager serializes and the OCaml binding crashes
   across domains. Workers only run the pure exhaustive oracle; an
   undecided result (None) is treated as Not_equiv, which is sound: an
   undecided pair must span more than max_vcs distinct leaves (the budget
   covers everything up to that), and a genuine equivalence never does. *)
let confirm_verdict ?(smt_ok = true) ~assume_unproven ~use_smt ~smt_vars dom t1 ex1 t2 ex2 =
  incr tier_calls;
  if not (tier2_cross_eval dom t1 ex1 t2 ex2) then
    (incr tier2_short_circuit; Not_equiv)
  else
    (* Exact exhaustive oracle when the domain is small enough (e.g. bv4):
       decisive and Z3-free. Falls back to SMT (or random, sans --smt) on
       larger domains where the assignment space is too big. *)
    match exhaustive_equiv dom t1 t2 with
    | Some true -> Proven
    | Some false -> Not_equiv
    | None ->
      if not smt_ok then Not_equiv
      else if not use_smt then Proven
      else tier3_smt ~assume_unproven dom ~smt_vars t1 ex1 t2 ex2

(* Boolean view for call sites that only need equiv/not-equiv (grouping,
   merging). Only Proven and Assumed count as equivalent; Unproven (safe
   mode) and Not_equiv keep the terms distinct. *)
let confirm_equiv ~assume_unproven ~use_smt ~smt_vars dom t1 ex1 t2 ex2 =
  match confirm_verdict ~assume_unproven ~use_smt ~smt_vars dom t1 ex1 t2 ex2 with
  | Proven | Assumed -> true
  | Unproven | Not_equiv -> false

(* O(n) group via Hashtbl keyed by the bv. The `cmp` arg is unused — we
   rely on Hashtbl's structural hashing / equality (bv keys are
   `'a list` of equality-comparable values). *)
(* Group values by an exact key. The keys here are behaviour vectors —
   arrays of length = #inputs (often hundreds). OCaml's default Hashtbl
   hashes only ~10 nodes, so vectors sharing a prefix all collide into one
   bucket and every insert pays a full O(len) structural-equality scan;
   at ~5·10^5 vectors that dominated the whole iteration (minutes). Here we
   bucket by a STRONG hash (`hash_param` over the full vector) into an
   int-keyed table, then split each bucket by exact `=`. Hashing is O(len)
   once per value; equality runs only on genuine hash collisions. *)
let group_by _cmp key_of_value values =
  let h : (int, ('k * 'v list ref) list ref) Hashtbl.t =
    Hashtbl.create (max 16 (List.length values)) in
  List.iter (fun v ->
    let k = key_of_value v in
    let hsh = Hashtbl.hash_param 2000 2000 k in
    let bucket = match Hashtbl.find_opt h hsh with
      | Some b -> b
      | None -> let b = ref [] in Hashtbl.add h hsh b; b in
    match List.find_opt (fun (k', _) -> k' = k) !bucket with
    | Some (_, items) -> items := v :: !items
    | None -> bucket := (k, ref [v]) :: !bucket) values;
  Hashtbl.fold (fun _ bucket acc ->
    List.fold_left (fun acc (k, items) -> (k, !items) :: acc) acc !bucket) h []

(* Per-term EXACT signature for a small finite (`fully_exhaustive`) domain:
   the truth-table of the term over all assignments to its own distinct
   leaves, in canonical (sorted-name) order, as an 'a array. Two terms with
   the same leaves AND the same table are provably equivalent; the array
   length encodes the leaf count. Length = |values|^(distinct leaves), which
   is ≤ |values|^max_vcs (canonical terms have ≤ max_vcs distinct leaves) and
   so within the fully_exhaustive budget. This replaces the random behaviour
   vector as a SUBGROUP key, turning the O(n·subgroups) pairwise merge into
   an O(n) hash split. *)
let exact_table (dom : ('s, 'a) Domain.t) t : 'a array =
  match dom.Domain.values with
  | None -> [||]
  | Some vals ->
    let names = Array.of_list (List.sort compare (distinct_leaf_names t t)) in
    let varr = Array.of_list vals in
    let nv = Array.length varr and d = Array.length names in
    let total = let r = ref 1 in (for _ = 1 to d do r := !r * nv done); !r in
    let out = Array.make (max 1 total) (dom.Domain.int_to_val 0) in
    let idx = Array.make (max d 1) 0 in
    let pos = ref 0 and continue = ref true in
    while !continue do
      let assign =
        Array.to_list (Array.mapi (fun i name -> (name, varr.(idx.(i)))) names) in
      out.(!pos) <- Eval.eval dom assign t; incr pos;
      if d = 0 then continue := false
      else begin
        let rec inc i =
          if i < 0 then continue := false
          else if idx.(i) + 1 < nv then idx.(i) <- idx.(i) + 1
          else (idx.(i) <- 0; inc (i - 1)) in
        inc (d - 1)
      end
    done;
    Array.sub out 0 !pos

(* Exactly split a behaviour-vector bucket into true equivalence classes by
   hashing each term's `exact_table` — O(n), no SMT, no pairwise. Two terms
   land together iff they are equal over the same leaves, which is exact for
   the common case. (Equivalents that mention DIFFERENT leaf sets — e.g. two
   distinct constant-0 forms — are not merged here, but those are reducible
   and so don't survive as candidates. A pairwise reconcile would be exact
   but is O(classes²) and quadratically slow on big buckets.) *)
let exact_subgroups (dom : ('s, 'a) Domain.t) term_triples =
  group_by () (fun (t, _, _) -> exact_table dom t) term_triples
  |> List.map snd

(* Detailed per-iteration counts surfaced in --info mode. *)
type iter_info = {
  i_var_only : int;          (* enumerated terms with no holes *)
  i_with_holes : int;        (* enumerated terms containing a hole *)
  i_reducible : int;         (* enumerated terms dropped: a rewrite applied *)
  i_size_decisions : int;    (* candidates that became size-reducing rules *)
  i_kbo_decisions : int;     (* candidates that became KBO rules *)
  i_dup_skip : int;          (* candidates equal to an existing irreducible *)
  i_candidates_raw : int;    (* fresh-candidate decisions (pre-dedup) *)
  i_candidates_dedup : int;  (* distinct candidates fed to grouping *)
  i_bv_groups : int;         (* behavior-vector groups formed *)
  (* Exact exhaustive-oracle equivalence checks (Z3-free) this iteration.
     Nonzero ⇔ the domain is small enough (bool, low-width bv) to decide
     equivalence without SMT. *)
  i_exhaustive : int;
  (* SMT / tier activity attributable to THIS iteration (deltas). *)
  i_smt_calls : int;
  i_tier2_short : int;
  i_tier3_unknown : int;
  i_tier3_refuted : int;
  i_tier3_cex : int;
}

type 's iter_summary = {
  size : int;  enumerated : int;
  new_size_rules : 's Types.rule list;
  new_kbo_rules  : 's Types.rule list;
  new_irreducibles : 's Types.term list;
  (* Rules emitted/skipped this iteration on UNPROVEN equivalence (SMT
     Unknown, passed random). `new_assumed` were added (non-safe mode);
     `new_skipped` were declined (safe mode). *)
  new_assumed : 's Types.rule list;
  new_skipped : 's Types.rule list;
  total_size_rules : int;  total_kbo_rules : int;  total_irreducible : int;
  total_assumed : int;  total_skipped : int;
  info : iter_info;
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
  | D_skip
  | D_candidate of 's Types.term * 'a array * 'a examples

type match_kind = Size | Kbo | Skip

(* `equiv_irrs_anon`: like `equiv_irrs` but uses an "anon-aware" bucket
   where each entry is `(form, primary_entry)`. The same source primary
   can appear under multiple bv keys (its anon variants). We dedupe by
   primary so find_best doesn't see the same source multiple times when
   the candidate happens to share its primary bv with one of its own
   anon-variant bvs. *)
let process_term (dom : ('s, 'a) Domain.t) ~compiled_inputs_arr
      ~norm_index ~behaviors ~behaviors_by_bv
      ~use_smt ~sym_cmp t =
  Progress.tick ();
  let prefer a b = if Kbo.compare_total sym_cmp a b < 0 then a else b in
  (* Tier 2 / 3 are deferred to rule-emission time (apply_decisions and
     post-group code), where we have both terms' cells in scope and can
     mutate them. Calling SMT here would cost O(candidates × bucket_size)
     calls; deferring lets us amortize over the committed rules only. *)
  let equiv_irrs bv =
    if Array.length bv = 0 then
      if not use_smt then []
      else List.filter (fun (_, irr_bv, _) -> Array.length irr_bv = 0) behaviors
    else match Hashtbl.find_opt behaviors_by_bv bv with
      | Some irrs -> irrs
      | None -> []
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
             | Some (_, (Kbo | Skip)) -> loop (Some (irr, Size)) best_const rest)
          | Kbo.Less ->
            (match best with
             | None -> loop (Some (irr, Kbo)) best_const rest
             | Some (_, Size) -> loop best best_const rest
             | Some (prev, Kbo) -> loop (Some (prefer irr prev, Kbo)) best_const rest
             | Some (_, Skip) -> loop (Some (irr, Kbo)) best_const rest)
          | Kbo.Greater ->
            (* Unreachable: enumeration is size-monotonic, so a prior
               irreducible (size < this term) can never be KBO-Greater
               than it. This is exactly the case D_replace used to handle;
               since it cannot occur, there is no replacement to make. *)
            assert false
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
    let _ = ex in
    match find_best simplified (equiv_irrs bv) with
    | (None, None) -> Some (D_candidate (simplified, bv, ex))
    | (None, Some ((lhs_hole, rhs_hole), Size, irr_src)) ->
      Some (D_size_rule ((lhs_hole, rhs_hole), cand, irr_src))
    | (None, Some ((lhs_hole, rhs_hole), Kbo, irr_src)) ->
      Some (D_kbo_rule ((lhs_hole, rhs_hole), cand, irr_src))
    | (Some (irr, Size), _) -> Some (D_size_rule ((simplified, irr), cand, irr))
    | (Some (irr, Kbo), _) -> Some (D_kbo_rule ((simplified, irr), cand, irr))
    | (Some (_, Skip), _) -> Some D_skip
    | (None, Some (_, Skip, _)) ->
      Some (D_candidate (simplified, bv, ex))
  in
  (* Inputs come from enumerate_terms_caps which produces canonical
     terms. `normalize_canonical_or_skip` returns None as soon as ANY
     rule applies — the term is reducible, so it is not an irreducible
     candidate, and its normal form is enumerated and processed on its
     own. Only terms with no applicable rule reach `decide`. *)
  match Rewrite.normalize_canonical_or_skip ~sym_cmp ~index:norm_index t with
  | None -> None  (* reducible; handled via its normal form *)
  | Some simplified ->
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
      ~size_seen ~kbo_seen ~use_smt ~smt_vars
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
    confirm_verdict ~assume_unproven:rs.assume_unproven ~use_smt ~smt_vars
      dom src_lhs cand_ex src_rhs rhs_ex
  in
  (* On not-equivalent, demote the candidate back to D_candidate so it
     becomes a new irreducible. Without this, runs at low random counts
     silently lose genuinely-new irreducibles when SMT rejects a rule.
     On Assumed (unproven), also record the rule for reporting. *)
  let commit_rule ~seen ~committed cand rule src_lhs src_rhs =
    let (simplified, bv, ex) = cand in
    if Hashtbl.mem seen rule then ()
    else match confirms ex src_lhs src_rhs with
      | Not_equiv -> candidates := (simplified, bv, ex) :: !candidates
      | Unproven ->
        (* Safe mode: don't emit; keep the candidate as a new irreducible
           and log the equation we declined to take. *)
        rs.skipped_rules <- rule :: rs.skipped_rules;
        candidates := (simplified, bv, ex) :: !candidates
      | (Proven | Assumed) as v ->
        Hashtbl.add seen rule ();
        committed rule;
        if v = Assumed then rs.assumed_rules <- rule :: rs.assumed_rules
  in
  let commit_size cand rule src_lhs src_rhs =
    commit_rule ~seen:size_seen ~committed:(fun r ->
      rs.size_rules <- r :: rs.size_rules;
      new_size_rules := r :: !new_size_rules)
      cand rule src_lhs src_rhs
  in
  let commit_kbo cand rule src_lhs src_rhs =
    commit_rule ~seen:kbo_seen ~committed:(fun r ->
      rs.kbo_rules <- r :: rs.kbo_rules;
      new_kbo_rules := r :: !new_kbo_rules)
      cand rule src_lhs src_rhs
  in
  List.iter (function
    | D_size_rule (rule, cand, irr_src) ->
      commit_size cand rule (fst rule) irr_src
    | D_kbo_rule (rule, cand, irr_src) ->
      commit_kbo cand rule (fst rule) irr_src
    | D_skip -> ()
    | D_candidate (t, bv, ex) -> candidates := (t, bv, ex) :: !candidates
  ) decisions;
  (!new_size_rules, !new_kbo_rules, !candidates)

(* Escape hatch: force the old rebuild-every-subpass behavior (for
   benchmarking / debugging the incremental index). *)
let force_bv_rebuild =
  try Sys.getenv "RULE_ENUM_NO_BVIDX" = "1" with Not_found -> false

let bv_index_add idx ((_, bv, _) as entry) =
  let bucket = try Hashtbl.find idx bv with Not_found -> [] in
  Hashtbl.replace idx bv (entry :: bucket)

let build_bv_index behaviors =
  let h = Hashtbl.create (max 16 (List.length behaviors)) in
  List.iter (bv_index_add h) behaviors;
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

(* OCaml's runtime caps the number of domains that can exist over the
   process lifetime (Max_domains = 128, including the main domain and any
   backup threads). Spawning a fresh batch of domains every subpass —
   ~recommended_domain_count() of them, which on a 190-core node is 190 —
   both exceeds that cap directly AND, via rapid spawn/join churn,
   exhausts the domain allocator after a few iterations ("failed to
   allocate domain"). We cap the worker count well below the limit and
   reuse a single persistent pool across all iterations. *)
let max_workers = 64

let resolve_num_workers num_domains =
  let raw = match num_domains with
    | Some n when n > 0 -> n
    | _ ->
      match Sys.getenv_opt "RULE_ENUM_JOBS" with
      | Some s -> (try max 1 (int_of_string s) with _ -> 1)
      | None ->
        try Stdlib.Domain.recommended_domain_count () with _ -> 1
  in
  max 1 (min raw max_workers)

(* Persistent worker pool: a fixed set of long-lived domains pull tasks
   from a shared queue. Created once (lazily) and reused for every
   subpass, so the process spawns at most `max_workers` domains total
   regardless of how many iterations run. *)
module Pool = struct
  type t = {
    mutex : Mutex.t;
    work_cond : Condition.t;       (* workers wait for tasks *)
    done_cond : Condition.t;       (* submitter waits for completion *)
    queue : (unit -> unit) Queue.t;
    mutable pending : int;         (* tasks submitted but not yet finished *)
    mutable shutdown : bool;
    mutable domains : unit Stdlib.Domain.t array;
  }

  let worker p () =
    (* Workers must not touch the shared hash-cons cache. *)
    Types.enter_worker_mode ();
    let running = ref true in
    while !running do
      Mutex.lock p.mutex;
      while Queue.is_empty p.queue && not p.shutdown do
        Condition.wait p.work_cond p.mutex
      done;
      if Queue.is_empty p.queue && p.shutdown then begin
        Mutex.unlock p.mutex; running := false
      end else begin
        let task = Queue.pop p.queue in
        Mutex.unlock p.mutex;
        task ();
        Mutex.lock p.mutex;
        p.pending <- p.pending - 1;
        if p.pending = 0 then Condition.broadcast p.done_cond;
        Mutex.unlock p.mutex
      end
    done

  let create n =
    let p = {
      mutex = Mutex.create ();
      work_cond = Condition.create ();
      done_cond = Condition.create ();
      queue = Queue.create ();
      pending = 0; shutdown = false; domains = [||];
    } in
    p.domains <- Array.init n (fun _ -> Stdlib.Domain.spawn (worker p));
    p

  let size p = Array.length p.domains

  (* Run each thunk on the pool, returning results in submission order.
     Blocks until all complete. The first task that raises has its
     exception re-raised after the batch finishes. *)
  let run p (thunks : (unit -> 'a) array) : 'a array =
    let n = Array.length thunks in
    if n = 0 then [||] else begin
      let results = Array.make n None in
      let exn = ref None in
      Mutex.lock p.mutex;
      p.pending <- n;
      Array.iteri (fun i th ->
        Queue.push (fun () ->
          match th () with
          | v -> results.(i) <- Some v
          | exception e ->
            Mutex.lock p.mutex;
            if !exn = None then exn := Some e;
            Mutex.unlock p.mutex) p.queue) thunks;
      Condition.broadcast p.work_cond;
      while p.pending > 0 do Condition.wait p.done_cond p.mutex done;
      Mutex.unlock p.mutex;
      match !exn with
      | Some e -> raise e
      | None -> Array.map (function Some x -> x | None -> assert false) results
    end
end

(* Global pool, (re)built when the requested worker count changes. *)
let pool_ref : Pool.t option ref = ref None
let get_pool n =
  match !pool_ref with
  | Some p when Pool.size p = n -> p
  | _ ->
    let p = Pool.create n in
    pool_ref := Some p;
    p

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
    let chunks = Array.of_list (split [] lst) in
    let pool = get_pool nd in
    let thunks = Array.map (fun ch () -> List.filter_map f ch) chunks in
    List.concat (Array.to_list (Pool.run pool thunks))

(* Order-preserving parallel map over the pool. Used to parallelize rule
   confirmation when the equivalence oracle is the pure exhaustive one
   (`fully_exhaustive`): unlike Z3 (which serializes on its global memory
   manager), exhaustive eval has no shared state, so this scales. `f` must
   be pure apart from benign stat-counter increments. *)
let parallel_map ~num_domains f lst =
  let len = List.length lst in
  let nd = num_domains in
  if nd <= 1 || len < 256 then List.map f lst
  else
    let chunk_size = (len + nd - 1) / nd in
    let rec take n acc = function
      | [] -> (List.rev acc, [])
      | rest when n <= 0 -> (List.rev acc, rest)
      | x :: xs -> take (n - 1) (x :: acc) xs in
    let rec split acc = function
      | [] -> List.rev acc
      | rest -> let ch, rem = take chunk_size [] rest in split (ch :: acc) rem in
    let chunks = Array.of_list (split [] lst) in
    let pool = get_pool nd in
    let thunks = Array.map (fun ch () -> List.map f ch) chunks in
    List.concat (Array.to_list (Pool.run pool thunks))

(* Per-subpass decision tally for --info: (reducible, size, kbo,
   dup_skip, candidate). `reducible` = enumerated terms that a rule
   rewrote (so process_term dropped them); the rest are the decision
   kinds returned. *)
type subpass_counts = {
  c_reducible : int; c_size : int; c_kbo : int;
  c_dup_skip : int; c_candidate : int;
}

let count_decisions ~enumerated decisions =
  let sz = ref 0 and kb = ref 0 and sk = ref 0 and cd = ref 0 in
  List.iter (function
    | D_size_rule _ -> incr sz
    | D_kbo_rule _ -> incr kb
    | D_skip -> incr sk
    | D_candidate _ -> incr cd) decisions;
  { c_reducible = enumerated - List.length decisions;
    c_size = !sz; c_kbo = !kb;
    c_dup_skip = !sk; c_candidate = !cd }

let run_subpass (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets)
      ~compiled_inputs_arr ~size_seen ~kbo_seen
      ~num_domains ~use_smt ~smt_vars ~sym_cmp enumerated =
  let norm_index = Rewrite.index_rules (all_rules rs) in
  (* Reuse the persistent bv-index; only rebuild if the --smt-forced
     remap invalidated it. Behaviors don't change between the two
     subpasses of an iteration, so this rebuilds at most once. *)
  if rs.bv_index_dirty || force_bv_rebuild then begin
    rs.bv_index <- build_bv_index rs.behaviors;
    rs.bv_index_dirty <- false
  end;
  let behaviors_by_bv = rs.bv_index in
  let f = process_term dom ~compiled_inputs_arr
            ~norm_index ~behaviors:rs.behaviors
            ~behaviors_by_bv ~use_smt ~sym_cmp in
  let decisions = parallel_filter_map ~num_domains f enumerated in
  let counts = count_decisions ~enumerated:(List.length enumerated) decisions in
  let (sr, kr, cands) = apply_decisions dom rs
    ~size_seen ~kbo_seen ~use_smt ~smt_vars decisions in
  (sr, kr, cands, counts)

let profile_enabled =
  try Sys.getenv "RULE_ENUM_PROFILE" = "1" with Not_found -> false

let prof_label name dt =
  if profile_enabled && dt > 0.01 then
    Printf.eprintf "    [%s] %.3fs\n%!" name dt

let mem_label name =
  if profile_enabled then begin
    let s = Gc.quick_stat () in
    let mb words = float_of_int words *. 8.0 /. 1024.0 /. 1024.0 in
    Printf.eprintf "    [mem %s] heap=%.1fMB\n%!" name (mb s.heap_words)
  end

let run_iteration (dom : ('s, 'a) Domain.t) (rs : ('s, 'a) rule_sets) (n : int)
      (caps : Enum.caps) ~num_domains ~sym_cmp : 's iter_summary =
  let t_start = Sys.time () in
  (* Snapshot the unproven-equivalence logs so we can report what THIS
     iteration added/skipped, not just the cumulative totals. *)
  let assumed_before = rs.assumed_rules in
  let skipped_before = rs.skipped_rules in
  (* Snapshot global tier counters for per-iteration deltas (--info). *)
  let smt0 = !tier3_calls and t2sc0 = !tier2_short_circuit in
  let unk0 = !tier3_unknown and ref0 = !tier3_unknown_refuted and cex0 = !tier3_cex_added in
  let exh0 = Atomic.get exhaustive_calls in
  let suffix_added prev cur =
    (* cur is `new @ prev` (rules prepended); return just the new prefix. *)
    let extra = List.length cur - List.length prev in
    if extra <= 0 then [] else
      let rec take n = function x :: xs when n > 0 -> x :: take (n-1) xs | _ -> [] in
      take extra cur
  in
  (* Enumeration phase: indeterminate progress (Enum ticks per term),
     with an approximate % from the size-growth estimate. *)
  Progress.start ~est:(estimate_next_enum ())
    ~label:(Printf.sprintf "size %d enumerating" n) ~total:0 ();
  let enumerated =
    Enum.enumerate_terms_caps dom.Domain.all_symbols (irreducibles rs) n caps in
  let n_enum = List.length enumerated in
  enum_history := n_enum :: !enum_history;
  let t_enum = Sys.time () -. t_start in
  prof_label (Printf.sprintf "iter %d enum (%d)" n n_enum) t_enum;
  (* Processing phase: exact bar over the now-known term count. *)
  Progress.start ~label:(Printf.sprintf "size %d processing" n) ~total:n_enum ();
  let inputs = rs.inputs in
  let all_inputs = inputs @ if rs.use_smt_forced then List.map (fun vars ->
    List.map (fun (v, n) -> (v, dom.Domain.int_to_val n)) vars) rs.forced_inputs
  else [] in
  if rs.use_smt_forced then begin
    rs.behaviors <- List.map (fun (irr, _, _) ->
      let bv = Eval.behavior dom all_inputs irr in
      let ex = ref (Eval.make_examples dom all_inputs irr) in
      (irr, bv, ex)) rs.behaviors;
    (* New forced inputs change every irreducible's bv → index stale. *)
    rs.bv_index_dirty <- true
  end;
  (* SMT is the Tier-3 correctness check: random-bv is Tier 1 (cheap
     prefilter), per-term examples is Tier 2 (currently empty for the
     fast path), SMT confirms equivalence when the cheaper tiers agree.
     We run SMT *whenever* the user requested it, regardless of whether
     random inputs are also present — random just prefilters which
     candidate pairs need SMT. *)
  let use_smt = rs.use_smt in
  (* True when the pure exhaustive oracle can DECIDE every genuine
     equivalence, so confirmation never needs Z3 and can be parallelized.
     A genuine equivalence spans ≤ max_vcs distinct leaves, hence
     |values|^max_vcs evaluations — if that fits the budget, equivalent
     pairs are always fully decided; non-equivalent pairs short-circuit or
     (rarely) come back undecided, which the parallel path treats as
     Not_equiv. So workers never touch Z3. *)
  let fully_exhaustive =
    match dom.Domain.values with
    | None -> false
    | Some vals ->
      let nv = List.length vals in
      let rec pow acc i =
        if acc > exhaustive_max_combos then acc
        else if i = 0 then acc else pow (acc * nv) (i - 1) in
      pow 1 caps.max_vcs <= exhaustive_max_combos
  in
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
  let sr1, kr1, cands1, ct1 = run_subpass dom rs
    ~compiled_inputs_arr ~size_seen ~kbo_seen
    ~num_domains ~use_smt ~smt_vars ~sym_cmp var_only in
  prof_label (Printf.sprintf "subpass1 var (%d)" (List.length var_only)) (Sys.time () -. t_sp1);
  let t_sp2 = Sys.time () in
  let sr2, kr2, cands2, ct2 = run_subpass dom rs
    ~compiled_inputs_arr ~size_seen ~kbo_seen
    ~num_domains ~use_smt ~smt_vars ~sym_cmp with_holes in
  prof_label (Printf.sprintf "subpass2 hole (%d)" (List.length with_holes)) (Sys.time () -. t_sp2);
  let new_size_rules = ref (sr1 @ sr2) in
  let new_kbo_rules = ref (kr1 @ kr2) in
  let candidates = cands1 @ cands2 in
  let candidates_raw_count = List.length candidates in
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
  let candidates_dedup_count = List.length candidates in
  prof_label (Printf.sprintf "dedup (%d cands)" candidates_dedup_count) (Sys.time () -. t_dedup);
  let t_group = Sys.time () in
  let groups = group_by cmp (fun (_, bv, _) -> bv) candidates in
  let n_bv_groups = List.length groups in
  prof_label (Printf.sprintf "group_by (%d groups)" n_bv_groups) (Sys.time () -. t_group);
  (* SMT-driven subgrouping: within each bv-bucket, Tier 2 (cross-eval
     on accumulated examples) then Tier 3 (SMT) confirm which terms are
     TRULY equivalent. Random bv may produce false-positive "equivalent"
     groupings (different functions that happen to agree on the random
     sample); the combined check splits these into distinct subgroups.
     Every SMT counterexample is appended to both terms' example cells. *)
  let groups = if not rs.use_smt then groups else begin
    Progress.start
      ~label:(Printf.sprintf "size %d subgrouping%s" n
                (if fully_exhaustive then "" else " (smt)"))
      ~total:(List.length groups) ();
    let subgroup_one (bv, term_triples) =
      Progress.tick ();
      match term_triples with [] -> [] | first :: rest ->
      let groups = ref [[first]] in
      List.iter (fun ((t, _, t_ex) as triple) ->
        let idx = ref (-1) in
        let unproven_rep = ref None in
        List.iteri (fun i g ->
          if !idx = -1 then
            let (rep, _, rep_ex) = List.hd g in
            (* smt_ok=false ⇔ run on the worker pool (fully_exhaustive):
               must not reach Z3. Harmless when sequential (None won't
               occur for in-budget genuine equivalences). *)
            match confirm_verdict ~smt_ok:(not fully_exhaustive)
                    ~assume_unproven:rs.assume_unproven ~use_smt ~smt_vars
                    dom t t_ex rep rep_ex with
            | Proven | Assumed -> idx := i
            | Unproven -> if !unproven_rep = None then unproven_rep := Some rep
            | Not_equiv -> ())
          !groups;
        match !idx with
        | i when i >= 0 -> groups := List.mapi (fun j g -> if j = i then triple :: g else g) !groups
        | _ ->
          (* t stays in its own subgroup. If it was kept separate from a
             random-equivalent rep only because SMT couldn't decide
             (Unproven, safe mode), log the declined equation (oriented by
             KBO) so it surfaces alongside the assumed rules. *)
          (match !unproven_rep with
           | Some rep ->
             let r = if Kbo.compare_total sym_cmp rep t <= 0 then (t, rep) else (rep, t) in
             rs.skipped_rules <- r :: rs.skipped_rules
           | None -> ());
          groups := [triple] :: !groups)
        rest;
      List.map (fun g -> (bv, g)) !groups
    in
    (* On a small finite domain, split each bv-bucket EXACTLY via per-term
       truth-tables: O(n) hash + cheap rep reconcile, instead of the
       O(n·subgroups) pairwise merge (which dominated bv4 at size 8 because
       the 200-input random vector over-groups, producing huge buckets that
       fan out into many subgroups). Pure ⇒ parallelizable. *)
    let subgroup_exact (bv, term_triples) =
      Progress.tick ();
      List.map (fun g -> (bv, g)) (exact_subgroups dom term_triples)
    in
    (* Groups are independent; with the pure exhaustive oracle (no Z3, no
       Unproven ⇒ no shared skipped_rules write, and each candidate's cell
       lives in exactly one bv-group) the per-group work parallelizes. With
       SMT it must stay sequential. Order is preserved either way. *)
    let f = if fully_exhaustive then subgroup_exact else subgroup_one in
    List.concat
      (if fully_exhaustive then parallel_map ~num_domains f groups
       else List.map f groups)
  end
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
          confirm_equiv ~assume_unproven:rs.assume_unproven ~use_smt ~smt_vars
            dom rep rep_ex rep2 rep2_ex) rest in
        let merged = List.fold_left (fun a (_, tp2) -> a @ tp2) tp same in
        merge_all ((bv, merged) :: acc) diff
    in
    List.rev (merge_all [] groups)
  in
  let t_kbo_extract = Sys.time () in
  let new_irr_pairs = ref [] in
  (* Candidate KBO rules collected during the (pure) orbit/winner pass.
     Confirmation is deferred so each distinct (lhs, rhs) pair is verified
     exactly once: the orbit expansion proposes the same pair from many
     groups/orbit members (~half are duplicates), and unconfirmed duplicates
     would otherwise pile up in skipped_rules / re-run SMT. *)
  let candidate_rules = ref [] in
  (* Per-iteration cache: anon → list of (orbit_member, kbo_cache).
     Different source terms in the same bv-group often share an anon
     (e.g. var-form `a+b` and hole-form `A+B` both reduce to the same
     anon `A+B`), and hole_permutations + bv-filter + Kbo.cache are pure
     functions of anon. Caching at iteration scope amortizes them. *)
  let use_cache =
    try Sys.getenv "RULE_ENUM_NO_ANON_CACHE" <> "1" with Not_found -> true
  in
  let anon_cache = Hashtbl.create 64 in
  let compute_orbit anon =
    let anon_bv = Eval.behavior_compiled_arr dom compiled_inputs_arr anon in
    let orbit = Types.hole_permutations anon in
    List.filter_map (fun a ->
      let a_bv =
        if a == anon then anon_bv
        else Eval.behavior_compiled_arr dom compiled_inputs_arr a in
      if a_bv = anon_bv then Some (a, Kbo.cache a) else None) orbit
  in
  let orbit_of_anon anon =
    if not use_cache then compute_orbit anon
    else match Hashtbl.find_opt anon_cache anon with
      | Some v -> v
      | None ->
        incr anon_distinct;
        let filtered = compute_orbit anon in
        Hashtbl.add anon_cache anon filtered;
        filtered
  in
  Progress.start ~label:(Printf.sprintf "size %d extracting rules" n)
    ~total:(List.length groups) ();
  List.iter (fun (_bv, term_triples) ->
    Progress.tick ();
    (* The anonymized form `vars_to_holes ∘ canonicalize` plus its hole-
       renaming orbit determines the winner per bv-group. Orbit members
       with anon-bv equal to the canonical anon are the universally-
       equivalent forms; the rest get filtered. *)
    let with_anon = List.concat_map (fun (t, bv, ex) ->
      incr anon_total;
      let anon = Types.canonicalize (Types.vars_to_holes t) in
      List.map (fun (a, a_c) -> (t, bv, ex, a, a_c)) (orbit_of_anon anon))
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
    let w_c = Kbo.cache w_t in   (* constant per group — hoisted out of the inner loop below *)
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
        (* Confirm the ACTUAL rule pair (lhs, rhs), not the source terms.
           Orbit members share a source (`other_t = w_t`), so checking the
           sources is a trivially-true self-comparison; the emitted rule
           may relate different hole orientations (e.g. the bogus
           Shl(B,A) -> Shl(A,B), whose sides agree only on a degenerate
           all-zero random sample). Verifying lhs ≡ rhs directly lets SMT
           reject such non-equivalences. *)
        let _ = other_ex in let _ = w_ex in
        (match rule with Some r -> candidate_rules := r :: !candidate_rules | None -> ())
      end) with_anon)
    groups;
  prof_label (Printf.sprintf "kbo-extract orbit (%d groups)" (List.length groups))
    (Sys.time () -. t_kbo_extract);
  (* Deduplicate candidate pairs: confirm each distinct (lhs, rhs) once,
     dropping pairs already committed in a prior subpass/iteration
     (`kbo_seen`). `List.rev` restores left-to-right encounter order. *)
  let t_confirm = Sys.time () in
  let cand_rules =
    let seen = Hashtbl.create (max 16 (List.length !candidate_rules)) in
    List.filter (fun r ->
      if Hashtbl.mem kbo_seen r || Hashtbl.mem seen r then false
      else (Hashtbl.add seen r (); true))
      (List.rev !candidate_rules)
  in
  Progress.start
    ~label:(Printf.sprintf "size %d verifying rules%s" n
              (if fully_exhaustive then "" else " (smt)"))
    ~total:(List.length cand_rules) ();
  let confirm_one (lhs, rhs as r) =
    Progress.tick ();
    (r, confirm_verdict ~smt_ok:(not fully_exhaustive)
          ~assume_unproven:rs.assume_unproven
          ~use_smt ~smt_vars dom lhs (new_examples ()) rhs (new_examples ()))
  in
  (* When `fully_exhaustive`, confirmation is pure (no Z3) and parallelizes
     cleanly; otherwise SMT can be reached, which must stay single-threaded
     (Z3's global memory manager serializes and the binding misbehaves
     across domains). Verdicts merge SEQUENTIALLY in candidate order, so the
     committed rule set is independent of worker scheduling. *)
  let verdicts =
    if fully_exhaustive then parallel_map ~num_domains confirm_one cand_rules
    else List.map confirm_one cand_rules
  in
  List.iter (fun (r, v) ->
    match v with
    | Not_equiv -> ()
    | Unproven -> rs.skipped_rules <- r :: rs.skipped_rules
    | (Proven | Assumed) as v ->
      Hashtbl.add kbo_seen r ();
      rs.kbo_rules <- r :: rs.kbo_rules;
      new_kbo_rules := r :: !new_kbo_rules;
      if v = Assumed then rs.assumed_rules <- r :: rs.assumed_rules)
    verdicts;
  prof_label (Printf.sprintf "kbo-extract confirm (%d unique rules)" (List.length cand_rules))
    (Sys.time () -. t_confirm);
  Progress.finish ();
  let sorted = List.sort (fun (a,_,_) (b,_,_) -> Kbo.compare_total sym_cmp a b) !new_irr_pairs in
  List.iter (fun entry ->
    rs.behaviors <- entry :: rs.behaviors;
    (* Keep the persistent bv-index in lock-step with behaviors. If it was
       invalidated this iteration it will be rebuilt wholesale next subpass,
       so the incremental add is only needed while clean. *)
    if not rs.bv_index_dirty then bv_index_add rs.bv_index entry)
    (List.rev sorted);
  new_irreducibles := List.map (fun (t, _, _) -> t) sorted;
  { size = n; enumerated = List.length enumerated;
    new_size_rules = List.rev !new_size_rules; new_kbo_rules = List.rev !new_kbo_rules;
    new_irreducibles = List.rev !new_irreducibles;
    new_assumed = List.rev (suffix_added assumed_before rs.assumed_rules);
    new_skipped = List.rev (suffix_added skipped_before rs.skipped_rules);
    total_size_rules = List.length rs.size_rules; total_kbo_rules = List.length rs.kbo_rules;
    total_irreducible = List.length rs.behaviors;
    total_assumed = List.length rs.assumed_rules;
    total_skipped = List.length rs.skipped_rules;
    info = {
      i_var_only = List.length var_only;
      i_with_holes = List.length with_holes;
      i_reducible = ct1.c_reducible + ct2.c_reducible;
      i_size_decisions = ct1.c_size + ct2.c_size;
      i_kbo_decisions = ct1.c_kbo + ct2.c_kbo;
      i_dup_skip = ct1.c_dup_skip + ct2.c_dup_skip;
      i_candidates_raw = candidates_raw_count;
      i_candidates_dedup = candidates_dedup_count;
      i_bv_groups = n_bv_groups;
      i_exhaustive = Atomic.get exhaustive_calls - exh0;
      i_smt_calls = !tier3_calls - smt0;
      i_tier2_short = !tier2_short_circuit - t2sc0;
      i_tier3_unknown = !tier3_unknown - unk0;
      i_tier3_refuted = !tier3_unknown_refuted - ref0;
      i_tier3_cex = !tier3_cex_added - cex0;
    };
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

let run ?max_size ?(forced_inputs = []) ?(on_iteration = fun _ _ -> ()) ?num_domains
      ?(use_smt = false) ?(use_smt_forced = false) ?(assume_unproven = true)
      ?unknown_inputs ?(progress = false)
      ?max_vars ?max_holes (dom : ('s, 'a) Domain.t)
      ~num_random_inputs ~max_vcs =
  Types.clear_cons_cache ();
  Progress.enabled := progress;
  enum_history := [];
  (match unknown_inputs with Some n -> unknown_extra_inputs := max 0 n | None -> ());
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
  let rs = create ~use_smt ~use_smt_forced ~assume_unproven inputs in
  let results = ref [] in let n = ref 1 in let continue = ref true in
  while !continue && !n <= default_max do
    let summary = run_iteration dom rs !n caps
        ~num_domains:workers
        ~sym_cmp:dom.Domain.sym_compare in
    if summary.new_size_rules = [] && summary.new_kbo_rules = [] && summary.new_irreducibles = [] then
      continue := false
    else (on_iteration rs summary; results := summary :: !results; incr n)
  done; (rs, List.rev !results)

(* Expose resolution for callers that need to print the actual job count. *)
let effective_num_workers = resolve_num_workers
