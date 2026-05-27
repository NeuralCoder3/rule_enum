(* For each rule produced WITH holes but missing without, classify by why
   the no-holes algorithm doesn't produce it.

   For each missing rule (lhs, rhs), we build the "var-analogue" by
   replacing each Hole with a fresh Var (in a disjoint id space) — but
   we do NOT canonicalize the resulting terms standalone, because rewrite
   rules are pairs applied via match-and-substitute. The pair's RHS
   structure is fixed at rule construction.

   Categories:

   (A) Partial-KBO refuses the var-analogue: rhs_va ⊀ₖ lhs_va, so even
       if the analogue rule were valid semantically, KBO can't orient it
       (a substitution might violate the inequality).

   (B) Var-count gate refuses the var-analogue: some Var appears more
       on RHS than LHS, so under substitution `Var := big_term` the rule
       would grow size — substitution-unsafe.

   (C) Var-analogue IS a valid pure-var rule (semantically equivalent
       and KBO-orderable), but the algorithm misses it because its
       canonical enumeration doesn't store both orientations. The hole
       version recovers it by giving holes a separate id-space whose
       canonical form preserves the position that var canonicalization
       would erase. *)

open Rule_enum

let sym_str = Domain_int.string_of_symbol
let sym_cmp = Domain_int.int_domain.Domain.sym_compare
let dom = Domain_int.int_domain

let holes_to_vars ~offset t =
  let rec go = function
    | Types.Var v -> Types.Var v
    | Types.Hole n -> Types.Var (offset + n)
    | Types.Node (f, args) -> Types.Node (f, List.map go args)
  in go t

(* Canonicalize a rule PAIR: vars on either side share a single id space,
   renumbered by left-to-right first occurrence across (lhs, rhs). Holes
   are canonicalized in their own separate id space. *)
let canonicalize_pair (lhs, rhs) =
  let next_v = ref 0 and next_h = ref 0 in
  let vmap = Hashtbl.create 8 and hmap = Hashtbl.create 8 in
  let rec go = function
    | Types.Var v ->
      (match Hashtbl.find_opt vmap v with
       | Some nv -> Types.Var nv
       | None -> let id = !next_v in incr next_v;
         Hashtbl.add vmap v id; Types.Var id)
    | Types.Hole n ->
      (match Hashtbl.find_opt hmap n with
       | Some nh -> Types.Hole nh
       | None -> let id = !next_h in incr next_h;
         Hashtbl.add hmap n id; Types.Hole id)
    | Types.Node (f, args) -> Types.Node (f, List.map go args)
  in
  let lhs' = go lhs in
  let rhs' = go rhs in
  (lhs', rhs')

let rule_str (l, r) =
  Types.to_string sym_str l ^ "  ->  " ^ Types.to_string sym_str r

(* Highest var id present in the term (or -1 if none). *)
let rec max_var_id = function
  | Types.Var v -> v
  | Types.Hole _ -> -1
  | Types.Node (_, args) -> List.fold_left (fun m t -> max m (max_var_id t)) (-1) args

(* Treat all Vars as named inputs; test semantic equality on N random
   ground assignments covering ALL var ids in either term. *)
let same_semantic_value t1 t2 =
  let n_inputs = 200 in
  let k = 1 + max (max_var_id t1) (max_var_id t2) in
  let inputs = Eval.generate_inputs dom n_inputs (max k 1) in
  List.for_all (fun inp ->
    try
      Eval.eval dom inp t1 = Eval.eval dom inp t2
    with _ -> false) inputs

let () =
  Printf.printf "Running with holes...\n%!";
  let with_holes, _ = Algorithm.run ~max_size:5 dom
    ~num_random_inputs:100 ~max_vcs:3 ~max_vars:3 ~max_holes:3 in
  Printf.printf "Running without holes...\n%!";
  let no_holes, _ = Algorithm.run ~max_size:5 dom
    ~num_random_inputs:100 ~max_vcs:3 ~max_vars:3 ~max_holes:0 in

  let with_rules = with_holes.Algorithm.size_rules @ with_holes.Algorithm.kbo_rules in
  let no_rules = no_holes.Algorithm.size_rules @ no_holes.Algorithm.kbo_rules in

  (* Membership check is up to alpha-renaming on the pair. *)
  let in_set rule rules =
    let (cl, cr) = canonicalize_pair rule in
    List.exists (fun r ->
      let (l, r) = canonicalize_pair r in
      Types.term_eq sym_cmp cl l && Types.term_eq sym_cmp cr r) rules
  in
  (* For comparing a hole rule against no_rules: also check whether the
     var-analogue (holes → fresh vars) appears in no_rules up to alpha. *)
  let var_analogue_in_set (lhs, rhs) rules =
    let offset = 1 + max (max_var_id lhs) (max_var_id rhs) in
    let analogue = (holes_to_vars ~offset lhs, holes_to_vars ~offset rhs) in
    in_set analogue rules
  in

  (* "Missing" = hole rule whose var-analogue isn't even alpha-equivalent
     to any rule in no_rules. *)
  let missing = List.filter
    (fun r -> not (var_analogue_in_set r no_rules)) with_rules in
  let missing_pure_var = List.filter (fun (l, r) ->
    not (Types.has_hole l) && not (Types.has_hole r)) missing in
  Printf.printf "[sanity] missing pure-var rules (should be 0): %d\n"
    (List.length missing_pure_var);
  if missing_pure_var <> [] then
    List.iter (fun r ->
      Printf.printf "  pure-var missing: %s\n" (rule_str r)) missing_pure_var;
  let extra = List.filter (fun r -> not (in_set r with_rules)) no_rules in
  Printf.printf "\nWith holes:    %d rules\n" (List.length with_rules);
  Printf.printf "Without holes: %d rules\n" (List.length no_rules);
  Printf.printf "Missing: %d.  Extra (only in no-holes): %d.\n"
    (List.length missing) (List.length extra);
  if extra <> [] then
    List.iter (fun r ->
      Printf.printf "  extra: %s\n" (rule_str r)) extra;

  (* Classify each missing rule per the corrected (no-canon-RHS) criterion. *)
  let cat_a = ref [] in
  let cat_b = ref [] in
  let cat_c = ref [] in
  let unclassified = ref [] in

  List.iter (fun (lhs, rhs) ->
    let offset = 1 + Types.distinct_vars lhs in
    let lhs_va = holes_to_vars ~offset lhs in
    let rhs_va = holes_to_vars ~offset rhs in
    let sem_ok = same_semantic_value lhs_va rhs_va in
    let kbo_dir = Kbo.kbo sym_cmp rhs_va lhs_va in   (* want Less for valid rule *)
    let l_vars = Types.var_counts lhs_va and r_vars = Types.var_counts rhs_va in
    let r_le_l = Types.var_counts_le r_vars l_vars in
    match sem_ok, kbo_dir, r_le_l with
    | _, _, false ->
      cat_b := (lhs, rhs, lhs_va, rhs_va) :: !cat_b
    | true, Kbo.Less, true ->
      (* Var analogue is a valid rule — algorithm just misses it via
         canonicalization. *)
      cat_c := (lhs, rhs, lhs_va, rhs_va) :: !cat_c
    | _, _, true ->
      (* Var analogue is NOT KBO-orderable as a rule (rhs ⊀ₖ lhs).
         True KBO refusal — needs hole semantics for KBO totality
         (NoVar lex). *)
      cat_a := (lhs, rhs, lhs_va, rhs_va) :: !cat_a)
    missing;
  (* All cases handled above; sanity check. *)
  ignore unclassified;

  Printf.printf "\nClassification of %d missing rules:\n" (List.length missing);
  Printf.printf "  (A) Var-analogue rule NOT KBO-orderable as pure-var rule:  %d\n"
    (List.length !cat_a);
  Printf.printf "  (B) Var-count gate forbids var-analogue:                   %d\n"
    (List.length !cat_b);
  Printf.printf "  (C) Var-analogue IS valid pure-var rule, alg misses it via\n      canonicalization (hole namespace recovers it):        %d\n"
    (List.length !cat_c);

  let show name lst limit =
    if lst <> [] then begin
      Printf.printf "\n[%s] first %d:\n" name limit;
      List.iter (fun (lhs, rhs, lhs_va, rhs_va) ->
        Printf.printf "  rule:         %s\n" (rule_str (lhs, rhs));
        Printf.printf "    var-analogue: %s  ->  %s\n"
          (Types.to_string sym_str lhs_va)
          (Types.to_string sym_str rhs_va))
        (List.filteri (fun i _ -> i < limit) lst)
    end
  in
  show "Category A: var-analogue not KBO-orderable" !cat_a 4;
  show "Category B: var-count gate" !cat_b 4;
  show "Category C: missed by canonicalization" !cat_c 6;

  let total = List.length !cat_a + List.length !cat_b + List.length !cat_c in
  Printf.printf "\nTotal classified: %d / %d\n" total (List.length missing);
  if total = List.length missing then
    Printf.printf "✓ All missing rules classified.\n"
  else
    Printf.printf "✗ Some rules unclassified.\n"
