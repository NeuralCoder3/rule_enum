open Rule_enum

let v c = Types.Var (Char.code c - Char.code 'a')
let h n = Types.Hole n

let int_dom = Domain_int.int_domain
let bool_dom = Domain_bool.bool_domain
let int_sym_cmp = Domain_int.compare_symbol

let i_node s args = Types.Node (s, args)
let i_plus = Domain_int.Plus
let i_minus = Domain_int.Minus
let _i_uminus = Domain_int.UMinus
let i_times = Domain_int.Times

let b_node s args = Types.Node (s, args)
let b_and = Domain_bool.And
let b_or = Domain_bool.Or
let b_xor = Domain_bool.Xor
let _b_not = Domain_bool.Not

let test_canonicalize () =
  let s = Types.to_string Domain_int.string_of_symbol in
  let c1 = Types.canonicalize (v 'b') in
  assert (Types.to_string (fun _ -> "") c1 = "a");
  (* Var/Hole live in independent id namespaces post-canonicalize. *)
  let mixed = i_node i_plus [v 'b'; h 5] in
  assert (s (Types.canonicalize mixed) = "(a+A)");
  (* Vars are renaming-equivalent — renumbered by first occurrence. *)
  assert (s (Types.canonicalize (i_node i_plus [v 'c'; v 'a'])) = "(a+b)");
  (* Holes are constPs with a linear order — canonicalize MUST preserve
     their relative orientation. `Hole 0 + Hole 1` and `Hole 1 + Hole 0`
     are two distinct canonical forms. Both use the smallest ids 0..k-1
     (renumbered by sorted-id rank). *)
  let h01 = Types.canonicalize (i_node i_plus [h 0; h 1]) in
  let h10 = Types.canonicalize (i_node i_plus [h 1; h 0]) in
  assert (s h01 = "(A+B)");
  assert (s h10 = "(B+A)");
  assert (not (Types.term_eq int_sym_cmp h01 h10));
  (* Sorted-rank renumbering: Hole 5 + Hole 3 → Hole 1 + Hole 0 (3 has
     rank 0, 5 has rank 1; the orientation `larger-id on left` is preserved). *)
  assert (s (Types.canonicalize (i_node i_plus [h 5; h 3])) = "(B+A)");
  assert (s (Types.canonicalize (i_node i_plus [h 3; h 5])) = "(A+B)");
  (* Single hole: any id collapses to Hole 0. *)
  assert (s (Types.canonicalize (h 7)) = "A");
  Printf.printf "  canonicalize: OK\n"

let test_distinct_vcs () =
  assert (Types.distinct_vars (v 'a') = 1);
  assert (Types.distinct_holes (h 0) = 1);
  assert (Types.num_distinct_vcs (i_node i_plus [v 'a'; h 0]) = 2);
  Printf.printf "  distinct vc counts: OK\n"

let test_size () =
  assert (Types.size (v 'a') = 1);
  assert (Types.size (h 0) = 1);
  Printf.printf "  size: OK\n"

let test_has_hole () =
  assert (not (Types.has_hole (v 'a')));
  assert (Types.has_hole (h 0));
  assert (Types.has_hole (i_node i_plus [v 'a'; h 0]));
  assert (not (Types.has_hole (i_node i_plus [v 'a'; v 'b'])));
  Printf.printf "  has_hole: OK\n"

let test_kbo () =
  let a = v 'a' and b = v 'b' in
  assert (Kbo.lt int_sym_cmp a (i_node i_plus [a; b]));
  (* Holes are totally ordered by id (NoVar = KBO-total). *)
  assert (Kbo.lt int_sym_cmp (h 0) (h 1));
  assert (Kbo.kbo int_sym_cmp (h 0) (h 0) = Kbo.Equal);
  Printf.printf "  kbo: OK\n"

let test_match_subst () =
  let lhs = i_node i_plus [v 'a'; v 'b'] in
  let target1 = i_node i_plus [i_node i_times [v 'x'; v 'y']; v 'z'] in
  assert (Option.is_some (Types.match_subst lhs target1));
  let target2 = i_node i_plus [v 'x'; v 'x'] in
  assert (Option.is_some (Types.match_subst lhs target2));
  let lhs2 = i_node i_plus [v 'a'; v 'a'] in
  assert (Option.is_some (Types.match_subst lhs2 target2));
  assert (Option.is_none (Types.match_subst lhs2 (i_node i_plus [v 'x'; v 'y'])));
  Printf.printf "  match_subst: OK\n"

let test_match_var_const () =
  (* The order constraint enforces `img(Hole i) < img(Hole j)` iff `i < j`
     — i.e., by hole id, NOT by encounter order. This is what makes
     `Hole 0 + Hole 1` (canonical) and `Hole 1 + Hole 0` (non-canonical)
     match DIFFERENT target orientations, the asymmetry that supports
     commutativity rewriting. *)
  let lhs01 = i_node i_plus [h 0; h 1] in
  let lhs10 = i_node i_plus [h 1; h 0] in
  let canon_target    = i_node i_plus [h 2; h 3] in  (* left < right *)
  let reverse_target  = i_node i_plus [h 3; h 2] in  (* left > right *)
  (* Canonical LHS matches canonical target, NOT reverse. *)
  assert (Option.is_some (Types.match_var_const int_sym_cmp lhs01 canon_target));
  assert (Types.match_var_const int_sym_cmp lhs01 reverse_target = None);
  (* Reverse LHS does the OPPOSITE: matches reverse target, not canonical. *)
  assert (Types.match_var_const int_sym_cmp lhs10 reverse_target |> Option.is_some);
  assert (Types.match_var_const int_sym_cmp lhs10 canon_target = None);
  (* TERMINATION GUARD: distinct holes require STRICTLY ordered images, so
     a commutativity rule (Hole1+Hole0 -> Hole0+Hole1) must NOT match a
     target with EQUAL images like c+c — otherwise it would rewrite c+c to
     c+c forever. Equal images fail the strict `<` order for both LHS
     orientations. *)
  let equal_target = i_node i_plus [h 7; h 7] in
  assert (Types.match_var_const int_sym_cmp lhs01 equal_target = None);
  assert (Types.match_var_const int_sym_cmp lhs10 equal_target = None);
  let equal_var_target = i_node i_plus [v 'a'; v 'a'] in
  assert (Types.match_var_const int_sym_cmp lhs01 equal_var_target = None);
  assert (Types.match_var_const int_sym_cmp lhs10 equal_var_target = None);
  (* Hole pattern accepts Var as image (with order-preservation). *)
  let var_canon   = i_node i_plus [v 'a'; v 'b'] in
  let var_reverse = i_node i_plus [v 'b'; v 'a'] in
  assert (Option.is_some (Types.match_var_const int_sym_cmp lhs01 var_canon));
  assert (Types.match_var_const int_sym_cmp lhs01 var_reverse = None);
  assert (Option.is_some (Types.match_var_const int_sym_cmp lhs10 var_reverse));
  assert (Types.match_var_const int_sym_cmp lhs10 var_canon = None);
  (* Mixed Var+Hole LHS: x + ?A. Var part = general subst, Hole part = size-0. *)
  let mixed_lhs = i_node i_plus [v 'a'; h 0] in
  let mixed_target = i_node i_plus [i_node i_times [v 'x'; v 'y']; h 5] in
  (match Types.match_var_const int_sym_cmp mixed_lhs mixed_target with
   | Some (_vm, _hm) -> ()
   | None -> assert false);
  (* Hole consistency: ?A + ?A only matches when both target leaves equal. *)
  let lhs_eq = i_node i_plus [h 0; h 0] in
  let same = i_node i_plus [h 4; h 4] in
  let diff = i_node i_plus [h 4; h 5] in
  assert (Option.is_some (Types.match_var_const int_sym_cmp lhs_eq same));
  assert (Types.match_var_const int_sym_cmp lhs_eq diff = None);
  (* Three holes: order constraint applies pairwise by hole id. Pattern
     `(Hole 0 + Hole 2) - Hole 1` requires img(0) < img(1) < img(2). *)
  let lhs3 = i_node i_minus [i_node i_plus [h 0; h 2]; h 1] in
  assert (Option.is_some (Types.match_var_const int_sym_cmp lhs3
            (i_node i_minus [i_node i_plus [h 0; h 2]; h 1])));
  (* If img(2) and img(1) get swapped in the target, no match. *)
  assert (Types.match_var_const int_sym_cmp lhs3
            (i_node i_minus [i_node i_plus [h 0; h 1]; h 2]) = None);
  Printf.printf "  match_var_const: OK\n"

(* Termination: a commutativity rule applied by the rewrite engine must
   reach a fixpoint, never loop. With a non-strict order it would rewrite
   c+c -> c+c indefinitely. We normalize equal-, reverse-, and
   canonical-argument targets and assert each terminates with the right
   result. (If the strict-order guard regressed, this test would hang.) *)
let test_commutativity_terminates () =
  let cmp = int_sym_cmp in
  let s = Domain_int.string_of_symbol in
  let comm = (i_node i_plus [h 1; h 0], i_node i_plus [h 0; h 1]) in (* B+A -> A+B *)
  let norm t = fst (Rewrite.normalize_with_index ~sym_cmp:cmp [comm] t) in
  (* equal images: must be a fixpoint (no rewrite). *)
  let cc = norm (i_node i_plus [h 5; h 5]) in
  assert (Types.to_string s cc = "(A+A)");
  (* reverse order: rewritten once to canonical, then fixpoint. *)
  let dc = norm (i_node i_plus [h 6; h 5]) in
  assert (Types.to_string s dc = "(A+B)");
  (* canonical: untouched. *)
  let cd = norm (i_node i_plus [h 5; h 6]) in
  assert (Types.to_string s cd = "(A+B)");
  Printf.printf "  commutativity terminates (no c+c loop): OK\n"

let test_kbo_var_count () =
  let cmp = int_sym_cmp in
  let aplusa = i_node i_plus [v 'a'; v 'a'] in
  assert (Kbo.kbo cmp aplusa (v 'a') <> Kbo.Less);
  assert (Kbo.lt cmp (v 'a') aplusa);
  let f_a = i_node i_plus [v 'a'; v 'a'] in
  let f_b = i_node i_plus [v 'b'; v 'b'] in
  assert (Kbo.kbo cmp f_a f_b = Kbo.Incomparable);
  Printf.printf "  kbo var-count: OK\n"

let test_rewrite () =
  let sym_str = Domain_int.string_of_symbol in
  let rules = [(i_node i_plus [v 'a'; v 'a'], v 'a')] in
  let r1, sr1 = Rewrite.normalize_with_index ~sym_cmp:int_sym_cmp rules
    (i_node i_plus [v 'b'; v 'b']) in
  assert (Types.to_string sym_str r1 = "a"); assert sr1;
  let r2, sr2 = Rewrite.normalize_with_index ~sym_cmp:int_sym_cmp rules
    (i_node i_plus [v 'b'; v 'c']) in
  assert (Types.to_string sym_str r2 = "(a+b)"); assert (not sr2);
  Printf.printf "  rewrite: OK\n"

let test_rewrite_hole_priority () =
  (* When both a hole rule and a var rule can fire, the hole rule wins. *)
  let sym_str = Domain_int.string_of_symbol in
  let hole_rule = (i_node i_plus [h 0; h 1], h 0) in   (* ?A + ?B -> ?A *)
  let var_rule  = (i_node i_plus [v 'a'; v 'b'], v 'b') in  (* x + y -> y *)
  let rules = [var_rule; hole_rule] in
  let target = i_node i_plus [h 2; h 3] in  (* ?C + ?D *)
  let r, _ = Rewrite.normalize_with_index ~sym_cmp:int_sym_cmp rules target in
  (* Hole rule canonicalizes ?A's image, so output is canonicalized to "A". *)
  let s = Types.to_string sym_str r in
  assert (s = "A");
  Printf.printf "  rewrite hole priority: OK (got %s)\n" s

let test_eval_int () =
  let inputs = [("a", 5); ("b", 3)] in
  assert (Eval.eval int_dom inputs (i_node i_plus [v 'a'; v 'b']) = 8);
  assert (Eval.eval int_dom inputs (i_node i_minus [v 'a'; v 'b']) = 2);
  assert (Eval.eval int_dom inputs (i_node i_times [v 'a'; v 'b']) = 15);
  Printf.printf "  eval (int): OK\n"

let test_eval_bool () =
  let inputs = [("a", true); ("b", false)] in
  assert (Eval.eval bool_dom inputs (b_node b_and [v 'a'; v 'b']) = false);
  assert (Eval.eval bool_dom inputs (b_node b_or [v 'a'; v 'b']) = true);
  assert (Eval.eval bool_dom inputs (b_node b_xor [v 'a'; v 'b']) = true);
  Printf.printf "  eval (bool): OK\n"

let test_eval_hole () =
  let inputs = [("a", 5); ("A", 7)] in
  assert (Eval.eval int_dom inputs (i_node i_plus [v 'a'; h 0]) = 12);
  Printf.printf "  eval hole: OK\n"

let test_enum_with_holes () =
  let irr = [v 'a'; h 0] in
  let terms = Enum.enumerate_terms [("+", 2, Domain_int.Plus)] irr 3 2 in
  let has_hole_terms = List.filter Types.has_hole terms in
  let pure_var_terms = List.filter (fun t -> not (Types.has_hole t)) terms in
  Printf.printf "  enum (size 3, k=2): %d total, %d pure-var, %d with-hole\n"
    (List.length terms) (List.length pure_var_terms) (List.length has_hole_terms);
  assert (List.length has_hole_terms > 0);
  List.iter (fun t -> assert (Types.num_distinct_vcs t <= 2)) terms;
  Printf.printf "  enum with holes: OK\n"

let test_enum_size1 () =
  let terms = Enum.enumerate_terms [("+", 2, Domain_int.Plus)] [] 1 2 in
  (* Should produce both Var 0 and Hole 0 at size 1. *)
  assert (List.length terms = 2);
  Printf.printf "  enum size 1: OK\n"

let test_enum_caps_separated () =
  (* max_holes=0 disables hole leaves at size 1 (only Var). *)
  let no_holes : Enum.caps = { max_vars = 3; max_holes = 0; max_vcs = 3 } in
  let t1 = Enum.enumerate_terms_caps [("+", 2, Domain_int.Plus)] [] 1 no_holes in
  assert (List.length t1 = 1);
  (* max_vars=0 disables var leaves (only Hole). *)
  let no_vars : Enum.caps = { max_vars = 0; max_holes = 3; max_vcs = 3 } in
  let t2 = Enum.enumerate_terms_caps [("+", 2, Domain_int.Plus)] [] 1 no_vars in
  assert (List.length t2 = 1);
  (* max_vcs caps the sum even when individual caps are loose. *)
  let cap_sum : Enum.caps = { max_vars = 3; max_holes = 3; max_vcs = 2 } in
  let t3 = Enum.enumerate_terms_caps [("+", 2, Domain_int.Plus)]
    [v 'a'; h 0] 3 cap_sum in
  List.iter (fun t -> assert (Types.num_distinct_vcs t <= 2)) t3;
  Printf.printf "  enum caps separated: OK\n"

(* Disabling holes (max_holes=0) drops rules that require constant
   placeholders. Every missing rule falls into one of three categories,
   verified by examining the rule's "var-analogue" (substitute fresh
   Vars for Holes; don't canonicalize standalone, treat as a rule pair):

   (A) Var-analogue is NOT KBO-orderable as a pure-var rule (partial KBO
       refuses orientation under substitution-monotonicity).
   (B) Var-count gate forbids the var-analogue (RHS Var-count exceeds
       LHS, so substitution `Var := big_term` would grow the term).
   (C) Var-analogue IS a valid pure-var rule, but the algorithm misses
       it because its canonical enumeration collapses orientations the
       hole feature distinguishes (separate id space). *)
let test_holes_required_for_completeness () =
  let dom = int_dom in
  let with_holes, _ = Algorithm.run ~max_size:5 dom
    ~num_random_inputs:100 ~max_vcs:3 ~max_vars:3 ~max_holes:3 in
  let no_holes, _ = Algorithm.run ~max_size:5 dom
    ~num_random_inputs:100 ~max_vcs:3 ~max_vars:3 ~max_holes:0 in
  let sym_cmp = dom.Domain.sym_compare in
  let with_rules = with_holes.Algorithm.size_rules @ with_holes.Algorithm.kbo_rules in
  let no_rules = no_holes.Algorithm.size_rules @ no_holes.Algorithm.kbo_rules in
  let rec max_var_id = function
    | Types.Var v -> v
    | Types.Hole _ -> -1
    | Types.Node (_, args) -> List.fold_left (fun m t -> max m (max_var_id t)) (-1) args
  in
  let holes_to_vars ~offset t =
    let rec go = function
      | Types.Var v -> Types.Var v
      | Types.Hole n -> Types.Var (offset + n)
      | Types.Node (f, args) -> Types.Node (f, List.map go args)
    in go t
  in
  let canonicalize_pair (lhs, rhs) =
    let next_v = ref 0 and next_h = ref 0 in
    let vm = Hashtbl.create 8 and hm = Hashtbl.create 8 in
    let rec go = function
      | Types.Var v ->
        (match Hashtbl.find_opt vm v with Some nv -> Types.Var nv
         | None -> let i = !next_v in incr next_v;
           Hashtbl.add vm v i; Types.Var i)
      | Types.Hole n ->
        (match Hashtbl.find_opt hm n with Some nh -> Types.Hole nh
         | None -> let i = !next_h in incr next_h;
           Hashtbl.add hm n i; Types.Hole i)
      | Types.Node (f, args) -> Types.Node (f, List.map go args)
    in (go lhs, go rhs)
  in
  let alpha_eq r1 r2 =
    let (l1, r1) = canonicalize_pair r1 and (l2, r2) = canonicalize_pair r2 in
    Types.term_eq sym_cmp l1 l2 && Types.term_eq sym_cmp r1 r2
  in
  let var_analogue_in_set (lhs, rhs) rules =
    let off = 1 + max (max_var_id lhs) (max_var_id rhs) in
    let analogue = (holes_to_vars ~offset:off lhs, holes_to_vars ~offset:off rhs) in
    List.exists (fun r -> alpha_eq analogue r) rules
  in
  let missing = List.filter (fun r -> not (var_analogue_in_set r no_rules)) with_rules in
  assert (List.length missing > 0);
  (* Sanity: every missing rule must contain a hole. *)
  List.iter (fun (l, r) -> assert (Types.has_hole l || Types.has_hole r)) missing;

  let same_sem t1 t2 =
    let k = 1 + max (max_var_id t1) (max_var_id t2) in
    let inputs = Eval.generate_inputs dom 200 (max k 1) in
    List.for_all (fun inp ->
      try Eval.eval dom inp t1 = Eval.eval dom inp t2 with _ -> false) inputs
  in
  let cat_a = ref 0 and cat_b = ref 0 and cat_c = ref 0 in
  List.iter (fun (lhs, rhs) ->
    let off = 1 + max (max_var_id lhs) (max_var_id rhs) in
    let lhs_va = holes_to_vars ~offset:off lhs in
    let rhs_va = holes_to_vars ~offset:off rhs in
    let kbo_dir = Kbo.kbo sym_cmp rhs_va lhs_va in
    let l_vars = Types.var_counts lhs_va and r_vars = Types.var_counts rhs_va in
    let r_le_l = Types.var_counts_le r_vars l_vars in
    if not r_le_l then incr cat_b
    else if same_sem lhs_va rhs_va && kbo_dir = Kbo.Less then incr cat_c
    else incr cat_a)
    missing;
  let total = !cat_a + !cat_b + !cat_c in
  assert (total = List.length missing);
  Printf.printf "  completeness scheme: OK (%d missing; A=%d B=%d C=%d)\n"
    (List.length missing) !cat_a !cat_b !cat_c

let test_hole_permutations () =
  let s = Types.to_string Domain_int.string_of_symbol in
  let to_set ts = List.sort_uniq compare (List.map s ts) in
  (* 0 distinct holes: singleton orbit. *)
  assert (to_set (Types.hole_permutations (v 'a')) = ["a"]);
  (* 1 distinct hole: singleton orbit. *)
  assert (to_set (Types.hole_permutations (h 0)) = ["A"]);
  (* 2 distinct holes: both orientations. *)
  assert (to_set (Types.hole_permutations (i_node i_plus [h 0; h 1]))
            = ["(A+B)"; "(B+A)"]);
  (* Structural symmetry: identical-hole term has only one orbit member. *)
  assert (to_set (Types.hole_permutations (i_node i_plus [h 0; h 0])) = ["(A+A)"]);
  (* 3 distinct holes: 6 permutations all distinct under new canonicalize. *)
  let t = i_node i_plus [h 0; i_node i_times [h 1; h 2]] in
  let orbit = Types.hole_permutations t in
  assert (List.length orbit = 6);
  assert (List.length (to_set orbit) = 6);
  Printf.printf "  hole_permutations: OK\n"

let test_commutativity_rule () =
  (* End-to-end: running the algorithm on the int domain should produce
     explicit commutativity rules for `+` and `*`, but NOT for `-`. *)
  let rs, _ = Algorithm.run ~max_size:3 int_dom ~num_random_inputs:100 ~max_vcs:3 in
  let sym_str = Domain_int.string_of_symbol in
  let rule_strs = List.map (fun (l, r) ->
    Types.to_string sym_str l ^ " -> " ^ Types.to_string sym_str r)
    (rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules) in
  let has s = List.mem s rule_strs in
  assert (has "(B+A) -> (A+B)");
  assert (has "(B*A) -> (A*B)");
  (* No analogous rule for `-` (non-commutative). Both directions must be absent. *)
  assert (not (has "(B-A) -> (A-B)"));
  assert (not (has "(A-B) -> (B-A)"));
  Printf.printf "  commutativity rule generation: OK\n"

(* Regression for the same-size orientation concern: when two same-size
   equivalent terms are enumerated (e.g. B+A and A+B), the KBO-larger one
   must NOT survive as an irreducible — a rule orienting it to the
   KBO-smaller rep must be emitted, regardless of enumeration order.
   Concretely: no two distinct irreducibles may be behavior-equivalent
   (that would mean a missing orienting rule), and each irreducible must
   be the KBO-minimum of its own behavior class. *)
let test_no_equivalent_irreducibles () =
  Random.init 42;
  let rs, _ = Algorithm.run ~max_size:6 int_dom ~num_domains:1
    ~num_random_inputs:200 ~max_vcs:3 ~max_holes:3 in
  let sym_cmp = int_sym_cmp in
  let inputs = Eval.generate_inputs int_dom 200 3 in
  let compiled = Array.of_list (List.map Eval.compile inputs) in
  let irrs = List.map (fun (t, _, _) -> t) rs.Algorithm.behaviors in
  (* Group irreducibles by behavior vector; any bucket with >1 member is
     a missing-orientation bug. *)
  let by_bv = Hashtbl.create 256 in
  List.iter (fun t ->
    let bv = Eval.behavior_compiled_arr int_dom compiled t in
    let prev = try Hashtbl.find by_bv bv with Not_found -> [] in
    Hashtbl.replace by_bv bv (t :: prev)) irrs;
  let collisions = Hashtbl.fold (fun _ ts acc ->
    if List.length ts > 1 then ts :: acc else acc) by_bv [] in
  if collisions <> [] then begin
    Printf.eprintf "  equivalent irreducibles (missing orientation rule):\n";
    List.iter (fun ts ->
      Printf.eprintf "    { %s }\n"
        (String.concat ", " (List.map (Types.to_string Domain_int.string_of_symbol) ts)))
      collisions;
    assert false
  end;
  Printf.printf "  no equivalent irreducibles: OK (%d irreducibles, all distinct bv)\n"
    (List.length irrs);
  ignore sym_cmp

(* End-to-end semantic check: the synthesized rule set should normalize
   any two universally-equivalent terms to the same normal form. We
   sample pairs of terms and verify the rewrite engine converges. *)
let test_rule_set_semantic_closure () =
  let rs, _ = Algorithm.run ~max_size:5 int_dom ~num_random_inputs:100 ~max_vcs:3 in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let normalize t =
    let r, _ = Rewrite.normalize_with_index ~sym_cmp:int_sym_cmp rules t in
    Types.canonicalize r
  in
  (* Commutativity application: B+A and A+B should normalize to the same form. *)
  let n1 = normalize (i_node i_plus [h 1; h 0]) in
  let n2 = normalize (i_node i_plus [h 0; h 1]) in
  assert (Types.term_eq int_sym_cmp n1 n2);
  let n3 = normalize (i_node i_times [h 1; h 0]) in
  let n4 = normalize (i_node i_times [h 0; h 1]) in
  assert (Types.term_eq int_sym_cmp n3 n4);
  (* Non-commutativity stays separated: B-A and A-B normalize to DIFFERENT forms. *)
  let m1 = normalize (i_node i_minus [h 1; h 0]) in
  let m2 = normalize (i_node i_minus [h 0; h 1]) in
  assert (not (Types.term_eq int_sym_cmp m1 m2));
  Printf.printf "  rule-set semantic closure: OK\n"

(* Regression for the user-reported bug: nested commutative products with
   a REPEATED hole (e.g. `B*(A*B)`) failed to normalize. Every syntactic
   orientation of a commutative/associative product must normalize to ONE
   form. This specifically guards the case the size-3 closure test above
   misses (nesting + repeated leaves) and the case the all-terms confluence
   test would only catch in hole-enabled mode. Times and Plus are both AC;
   we check the A.B^2, A^2.B, and A.B.C orientation families. *)
let mul a b = i_node i_times [a; b]
let pl  a b = i_node i_plus  [a; b]

let test_constp_product_confluence () =
  Random.init 42;
  let rs, _ = Algorithm.run ~max_size:5 int_dom ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:3 ~max_holes:3 ~use_smt:true in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let cmp = int_sym_cmp in
  let str = Types.to_string Domain_int.string_of_symbol in
  let nf t = Types.canonicalize (fst (Rewrite.normalize_with_index ~sym_cmp:cmp rules t)) in
  (* Every term in `terms` is universally equal; assert one shared normal form. *)
  let assert_confluent label terms =
    match terms with
    | [] -> ()
    | t0 :: _ ->
      let nf0 = nf t0 in
      List.iter (fun t ->
        let n = nf t in
        if not (Types.term_eq cmp n nf0) then begin
          Printf.eprintf
            "  NON-CONFLUENT (%s): %s -> %s   vs   %s -> %s\n"
            label (str t0) (str nf0) (str t) (str n);
          assert false
        end) terms
  in
  let families op =       (* op = mul or pl, both AC *)
    let a = h 0 and b = h 1 and c = h 2 in
    (* A.B^2 (= one single, one doubled) — all 6 orientations *)
    [ "A.B^2", [ op b (op a b); op a (op b b); op b (op b a);
                 op (op a b) b; op (op b a) b; op (op b b) a ];
    (* A^2.B *)
      "A^2.B", [ op a (op a b); op a (op b a); op b (op a a);
                 op (op a a) b; op (op a b) a; op (op b a) a ];
    (* A.B.C — all distinct, the 6 right-nested perms + 6 left-nested *)
      "A.B.C", [ op a (op b c); op a (op c b); op b (op a c); op b (op c a);
                 op c (op a b); op c (op b a);
                 op (op a b) c; op (op a c) b; op (op b a) c; op (op b c) a;
                 op (op c a) b; op (op c b) a ] ]
  in
  List.iter (fun (lbl, ts) -> assert_confluent ("* " ^ lbl) ts) (families mul);
  List.iter (fun (lbl, ts) -> assert_confluent ("+ " ^ lbl) ts) (families pl);
  Printf.printf "  constP product confluence: OK\n"

(* Pins the documented incompleteness of var-only mode (--max-holes 0):
   `B*(A*B)` does NOT reach the same normal form as `A*(B*B)`, because
   vars_to_holes of a first-occurrence-canonical var term can never produce
   the LHS `*(H1,*(H0,H1))`. If this ever starts converging, var-only mode
   became complete — update the CLI warning and docs. The COMPLETE mode
   (above) must always converge. *)
let test_var_only_mode_incompleteness () =
  Random.init 42;
  let rs, _ = Algorithm.run ~max_size:5 int_dom ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:3 ~max_holes:0 ~use_smt:true in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let cmp = int_sym_cmp in
  let nf t = Types.canonicalize (fst (Rewrite.normalize_with_index ~sym_cmp:cmp rules t)) in
  let bab = mul (h 1) (mul (h 0) (h 1)) in   (* B*(A*B) *)
  let abb = mul (h 0) (mul (h 1) (h 1)) in   (* A*(B*B) *)
  let converged = Types.term_eq cmp (nf bab) (nf abb) in
  if converged then
    Printf.printf "  var-only incompleteness: NOTE — B*(A*B) now converges (var-only became complete?)\n"
  else
    Printf.printf "  var-only incompleteness: confirmed (B*(A*B) stuck; use --max-holes>0)\n"

(* Regression: a NON-commutative operator must never get a commutativity
   rule, even when its random behavior is degenerate. For 32-bit shifts,
   full-range random shift amounts are almost always >= width, so B<<A and
   A<<B are both 0 on every random input and share a behavior vector — the
   hole-orbit machinery would emit `<<(B,A) -> <<(A,B)` unless the actual
   rule pair is SMT-verified. SMT refutes it (e.g. A=2^32-1, B=0), so the
   rule must be absent; genuine commutativity (+, *, &, |) must remain. *)
let test_no_shift_commutativity () =
  Random.init 7;
  let rs, _ = Algorithm.run ~max_size:4 Domain_bv.bv_domain ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:3 ~use_smt:true in
  let s = Domain_bv.string_of_symbol in
  let strs = List.map (fun (l, r) ->
    Types.to_string s l ^ " -> " ^ Types.to_string s r)
    (rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules) in
  let has x = List.mem x strs in
  assert (not (has "<<(B,A) -> <<(A,B)"));
  assert (not (has ">>(B,A) -> >>(A,B)"));
  (* Genuine commutativity survives. *)
  assert (has "(B+A) -> (A+B)");
  assert (has "(B*A) -> (A*B)");
  (* And every emitted rule is genuinely sound (SMT-equivalent). *)
  let smt_vars = List.init 3 Types.var_name @ List.init 3 Types.hole_name in
  List.iter (fun (l, r) ->
    match Smt.check_equiv Domain_bv.bv_domain smt_vars l r with
    | Smt.Equivalent -> ()
    | _ -> Printf.eprintf "  UNSOUND bv rule: %s -> %s\n"
             (Types.to_string s l) (Types.to_string s r); assert false)
    (rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules);
  Printf.printf "  no shift commutativity (bv 32-bit): OK\n"

(* Soundness gate: with SMT enabled, EVERY rule the algorithm emits must
   be a genuine equivalence (SMT proves LHS ≡ RHS). Runs the synthesizer
   across all domains and a range of caps — including the configs that
   previously produced unsound rules (bv 32-bit shifts, mixed var/hole) —
   and checks each emitted rule with the SMT solver.

   Scope note: this is asserted for `--smt` runs only. Random-only mode
   has no soundness guarantee — e.g. for 32-bit bv shifts, B<<A and A<<B
   are both 0 on every full-range random input, so random alone cannot
   tell them apart and CAN emit unsound rules. `test_random_only_bv_unsound`
   pins that as a known limitation. *)
let smt_check_all_rules (type s) ~name (dom : (s, _) Domain.t) ~max_size
      ~max_vcs ~max_holes ~seed =
  Random.init seed;
  let rs, _ = Algorithm.run ~max_size dom ~num_domains:1
    ~num_random_inputs:100 ~max_vcs ~max_holes ~use_smt:true in
  let k = max max_vcs max_holes in
  let smt_vars = List.init k Types.var_name @ List.init k Types.hole_name in
  let sym = dom.Domain.sym_to_string in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let bad = List.filter (fun (l, r) ->
    match Smt.check_equiv dom smt_vars l r with
    | Smt.Equivalent -> false
    | Smt.CounterExample _ | Smt.Unknown -> true) rules in
  (* `Unknown` would be a false positive (a sound rule SMT can't prove in
     time); configs are small enough that all rules resolve, and any
     Unknown is reported and fails so it is never silently ignored. *)
  if bad <> [] then begin
    Printf.eprintf "  UNSOUND/unprovable rules in %s:\n" name;
    List.iter (fun (l, r) -> Printf.eprintf "    %s -> %s\n"
      (Types.to_string sym l) (Types.to_string sym r)) bad;
    assert false
  end;
  List.length rules

let test_all_rules_smt_sound () =
  let n =
    smt_check_all_rules ~name:"int (max_holes=0)" int_dom
      ~max_size:6 ~max_vcs:3 ~max_holes:0 ~seed:1
    + smt_check_all_rules ~name:"int (max_holes=3)" int_dom
        ~max_size:6 ~max_vcs:3 ~max_holes:3 ~seed:1
    + smt_check_all_rules ~name:"bool" bool_dom
        ~max_size:5 ~max_vcs:2 ~max_holes:2 ~seed:1
    + smt_check_all_rules ~name:"bv (32-bit, max_holes=3)"
        Domain_bv.bv_domain ~max_size:4 ~max_vcs:3 ~max_holes:3 ~seed:1
    + smt_check_all_rules ~name:"bv (8-bit)"
        Domain_bv.bv_domain ~max_size:4 ~max_vcs:3 ~max_holes:0 ~seed:1
  in
  Printf.printf "  all emitted rules SMT-sound (--smt): OK (%d rules across 5 configs)\n" n

(* Pins the known limitation: random-only verification on 32-bit bv is
   NOT sound, because full-range random shift amounts are almost always
   >= width, making B<<A and A<<B both 0 on every sample. If a future
   change (e.g. shift-aware random sampling, or always-on SMT) makes
   random-only bv sound, this test trips and should be updated. *)
let test_random_only_bv_unsound () =
  Random.init 7;
  let rs, _ = Algorithm.run ~max_size:4 Domain_bv.bv_domain ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:3 ~max_holes:0 ~use_smt:false in
  let smt_vars = List.init 3 Types.var_name @ List.init 3 Types.hole_name in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let n_unsound = List.length (List.filter (fun (l, r) ->
    Smt.check_equiv Domain_bv.bv_domain smt_vars l r <> Smt.Equivalent) rules) in
  assert (n_unsound > 0);
  Printf.printf "  random-only bv unsound (known limitation): OK (%d/%d rules unsound)\n"
    n_unsound (List.length rules)

(* Regression: a Hole and a Var are distinct concepts. Tests that lump
   them together (e.g., "canonicalize renumbers by first occurrence")
   would let the conflation bug return silently. *)
let test_hole_var_distinct_semantics () =
  let s = Types.to_string Domain_int.string_of_symbol in
  (* Distinct var-only terms `a+b` and `b+a` are renaming-equivalent —
     SAME canonical form. *)
  let v_ab = Types.canonicalize (i_node i_plus [v 'a'; v 'b']) in
  let v_ba = Types.canonicalize (i_node i_plus [v 'b'; v 'a']) in
  assert (Types.term_eq int_sym_cmp v_ab v_ba);
  assert (s v_ab = "(a+b)");
  (* Distinct hole-only terms `A+B` and `B+A` are NOT renaming-equivalent
     — DIFFERENT canonical forms. *)
  let h_ab = Types.canonicalize (i_node i_plus [h 0; h 1]) in
  let h_ba = Types.canonicalize (i_node i_plus [h 1; h 0]) in
  assert (not (Types.term_eq int_sym_cmp h_ab h_ba));
  (* KBO orders the two hole-orientations. *)
  assert (Kbo.lt int_sym_cmp h_ab h_ba);
  Printf.printf "  hole/var distinct semantics: OK\n"

(* Manual checks we made during development that warrant regression tests. *)

(* Tier 2 cross-eval as a standalone helper: when cells contain shared
   inputs with disagreeing values, the helper must return false. The
   previous version silently agreed on inputs already in BOTH cells —
   it only checked new evaluations on inputs present in one cell only. *)
let test_tier2_cross_eval_helper () =
  let open Rule_enum in
  let dom = int_dom in
  (* Two non-equivalent terms. *)
  let t1 = i_node i_plus [h 0; h 1] in
  let t2 = i_node i_plus [h 0; h 0] in
  (* Seed both cells with the SAME input but DIFFERENT stored values
     (mimicking a prior SMT counterexample for the two terms). *)
  let input = [("A", 1); ("B", 2)] in
  let ex1 = ref [(input, 3)] in  (* claimed value of t1 *)
  let ex2 = ref [(input, 2)] in  (* claimed value of t2; differs *)
  assert (not (Algorithm.tier2_cross_eval dom t1 ex1 t2 ex2));
  (* Conversely: agreeing cells return true and extend with unseen inputs. *)
  let t3 = i_node i_plus [h 0; h 1] in
  let t4 = i_node i_plus [h 1; h 0] in  (* commutative: same value on all inputs *)
  let in_a = [("A", 5); ("B", 7)] in
  let in_b = [("A", 11); ("B", 13)] in
  let ex3 = ref [(in_a, 12)] in
  let ex4 = ref [(in_b, 24)] in
  let ok = Algorithm.tier2_cross_eval dom t3 ex3 t4 ex4 in
  assert ok;
  (* Cells extended to cover the union: ex3 now has in_b, ex4 now has in_a. *)
  assert (List.length !ex3 = 2);
  assert (List.length !ex4 = 2);
  Printf.printf "  tier2 cross-eval helper: OK\n"

(* At the recommended config (--random 100, max_holes=0), --random and
   --random --smt produce identical rule sets — SMT confirmation neither
   adds nor rejects rules at this density. *)
let test_smt_random_equivalence () =
  let run ?(use_smt = false) ?(max_holes = 0) () =
    Random.init 42;
    Algorithm.run ~max_size:6 int_dom ~num_domains:1
      ~num_random_inputs:100 ~max_vcs:3 ~max_holes ~use_smt |> fst
  in
  let rs_rand = run () in
  let rs_smt  = run ~use_smt:true () in
  let r1 = List.sort compare (rs_rand.Algorithm.size_rules @ rs_rand.Algorithm.kbo_rules) in
  let r2 = List.sort compare (rs_smt.Algorithm.size_rules @ rs_smt.Algorithm.kbo_rules) in
  let i1 = List.sort compare (List.map (fun (t, _, _) -> t) rs_rand.Algorithm.behaviors) in
  let i2 = List.sort compare (List.map (fun (t, _, _) -> t) rs_smt.Algorithm.behaviors) in
  assert (r1 = r2);
  assert (i1 = i2);
  (* The int domain never makes Z3 return Unknown, so every emitted rule
     is SMT-proven: nothing should be logged as an assumed equivalence. *)
  assert (rs_smt.Algorithm.assumed_rules = []);
  Printf.printf "  smt/random equivalence: OK (rules=%d irrs=%d, assumed=%d)\n"
    (List.length r1) (List.length i1) (List.length rs_smt.Algorithm.assumed_rules)

(* Safe mode must never assume unproven equivalences: assumed_rules stays
   empty, and a safe run is a subset (rule-wise) of the default run. For
   the int domain (no Unknown) the two coincide; this pins the invariant
   that assumed_rules is empty under safe mode regardless of domain. *)
let test_safe_mode_no_assumed () =
  let run ~safe () =
    Random.init 42;
    Algorithm.run ~max_size:5 int_dom ~num_domains:1
      ~num_random_inputs:100 ~max_vcs:3 ~use_smt:true
      ~assume_unproven:(not safe) |> fst
  in
  let rs_default = run ~safe:false () in
  let rs_safe = run ~safe:true () in
  assert (rs_safe.Algorithm.assumed_rules = []);
  (* The int domain never hits Unknown, so safe mode skips nothing. *)
  assert (rs_safe.Algorithm.skipped_rules = []);
  assert (rs_default.Algorithm.skipped_rules = []);
  (* Safe-mode rules are a subset of default-mode rules (safe never adds). *)
  let to_set rs = List.sort_uniq compare
    (rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules) in
  let d = to_set rs_default and s = to_set rs_safe in
  assert (List.for_all (fun r -> List.mem r d) s);
  Printf.printf "  safe mode (no assumed): OK (default=%d safe=%d)\n"
    (List.length d) (List.length s)

(* Semantic-closure check for what the algorithm DOES guarantee: pairs
   of universally-equivalent terms that differ only by leaf-level
   commutativity (swapping size-0 hole/constP positions of a commutative
   op). The rewrite engine normalizes both to the same canonical form. *)
let test_semantic_closure_leaf_level () =
  Random.init 42;
  let rs, _ = Algorithm.run ~max_size:6 int_dom ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:3 ~max_holes:3 ~use_smt:true in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let normalize t =
    let r, _ = Rewrite.normalize_with_index ~sym_cmp:int_sym_cmp rules t in
    Types.canonicalize r
  in
  let pairs = [
    "comm-+", i_node i_plus [h 1; h 0], i_node i_plus [h 0; h 1];
    "comm-*", i_node i_times [h 1; h 0], i_node i_times [h 0; h 1];
    (* Commutativity inside a unary context (negation): leaf-level. *)
    "-(B+A) ≡ -(A+B)",
      i_node Domain_int.UMinus [i_node i_plus [h 1; h 0]],
      i_node Domain_int.UMinus [i_node i_plus [h 0; h 1]];
  ] in
  List.iter (fun (name, lhs, rhs) ->
    let nl = normalize lhs in
    let nr = normalize rhs in
    if not (Types.term_eq int_sym_cmp nl nr) then begin
      Printf.eprintf "  leaf-level closure FAIL on %s: %s ~> %s vs %s ~> %s\n" name
        (Types.to_string Domain_int.string_of_symbol lhs)
        (Types.to_string Domain_int.string_of_symbol nl)
        (Types.to_string Domain_int.string_of_symbol rhs)
        (Types.to_string Domain_int.string_of_symbol nr);
      assert false
    end) pairs;
  Printf.printf "  semantic closure (leaf-level): OK (%d pairs)\n" (List.length pairs)

(* Operator-level commutativity on COMPOUND args.

   These pairs are universally equivalent but related by swapping args
   of a commutative operator when those args are compound terms (not
   just size-0 leaves). The basic comm rule `(B*A) → (A*B)` cannot fire
   because `match_var_const` requires hole images to be size-0.

   These close because the enumerator now generates every hole-id
   labeling of each partition shape (e.g. `B*(A*B)` alongside `A*(B*A)`).
   The non-canonical labelings land in their own bv-buckets where
   bv-bucketing groups them with semantically-equivalent canonical
   forms, and the post-group winner-extraction emits the rule. *)
let test_operator_level_closure () =
  Random.init 42;
  let rs, _ = Algorithm.run ~max_size:6 int_dom ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:3 ~max_holes:3 ~use_smt:true in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let normalize t =
    let r, _ = Rewrite.normalize_with_index ~sym_cmp:int_sym_cmp rules t in
    Types.canonicalize r
  in
  let pairs = [
    "B*(A*B) ≡ A*(B*B)",
      i_node i_times [h 1; i_node i_times [h 0; h 1]],
      i_node i_times [h 0; i_node i_times [h 1; h 1]];
    "(B*B)*A ≡ A*(B*B)",
      i_node i_times [i_node i_times [h 1; h 1]; h 0],
      i_node i_times [h 0; i_node i_times [h 1; h 1]];
    (* Negated counterparts (the original "extra SMT rules"). *)
    "-(B*(A*B)) ≡ -(A*(B*B))",
      i_node Domain_int.UMinus [i_node i_times [h 1; i_node i_times [h 0; h 1]]],
      i_node Domain_int.UMinus [i_node i_times [h 0; i_node i_times [h 1; h 1]]];
  ] in
  List.iter (fun (name, lhs, rhs) ->
    let nl = normalize lhs and nr = normalize rhs in
    if not (Types.term_eq int_sym_cmp nl nr) then begin
      Printf.eprintf "  operator-level closure FAIL on %s: %s ~> %s vs %s ~> %s\n" name
        (Types.to_string Domain_int.string_of_symbol lhs)
        (Types.to_string Domain_int.string_of_symbol nl)
        (Types.to_string Domain_int.string_of_symbol rhs)
        (Types.to_string Domain_int.string_of_symbol nr);
      assert false
    end) pairs;
  Printf.printf "  semantic closure (operator-level): OK (%d pairs)\n" (List.length pairs)

(* Same seed → identical rule set; different seed → same rule set for
   this domain at this size (stable across seeds). Calls `Random.init`
   explicitly to surface any seed-dependent nondeterminism that
   `Random.self_init` would have masked. *)
let test_cross_seed_determinism () =
  let run_with_seed seed =
    Random.init seed;
    let rs, _ = Algorithm.run ~max_size:5 int_dom ~num_domains:1
      ~num_random_inputs:100 ~max_vcs:3 in
    (List.sort compare (rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules),
     List.sort compare (List.map (fun (t,_,_) -> t) rs.Algorithm.behaviors))
  in
  let a1, i1 = run_with_seed 42 in
  let a2, i2 = run_with_seed 42 in
  assert (a1 = a2);
  assert (i1 = i2);
  (* Different seed: rule SET (not list order) is the same modulo random
     bv collisions, but for this domain at size 5 the rule SET is stable
     across seeds. *)
  let b1, j1 = run_with_seed 1 in
  let b2, _ = run_with_seed 999 in
  assert (b1 = a1);
  assert (b2 = a1);
  assert (j1 = i1);
  Printf.printf "  cross-seed determinism: OK\n"

(* Regression: --random-inputs 1 --smt formerly crashed with "Unbound
   name: A" when SMT counterexamples omitted hole-slot assignments and
   downstream Eval saw missing names. tier3_smt now pads missing names
   with 0; the test just runs the path without exception. *)
let test_low_random_smt_no_unbound () =
  Random.init 42;
  let rs, _ = Algorithm.run ~max_size:4 int_dom ~num_domains:1
    ~num_random_inputs:1 ~max_vcs:3 ~use_smt:true in
  assert (List.length rs.Algorithm.behaviors > 0);
  Printf.printf "  low-random smt (no Unbound crash): OK (%d irrs)\n"
    (List.length rs.Algorithm.behaviors)

(* Round-trips every term the pretty-printer emits through the
   load-able file format used by --rule-output / --eval. *)
let test_parse_roundtrip () =
  let to_str = int_dom.Domain.term_to_string in
  let of_str = int_dom.Domain.term_of_string in
  let terms = [
    v 'a';
    h 0;
    i_node i_plus [v 'a'; v 'b'];
    i_node i_minus [v 'a'; h 0];
    i_node Domain_int.UMinus [i_node i_plus [v 'a'; h 1]];
    i_node i_times [i_node Domain_int.UMinus [v 'a']; h 0];
    i_node i_plus [i_node i_times [v 'a'; v 'b']; i_node i_minus [v 'c'; h 0]];
    (* Non-canonical hole orientation, must round-trip exactly. *)
    i_node i_times [h 1; i_node i_times [h 0; h 1]];
  ] in
  List.iter (fun t ->
    let s = to_str t in
    let t' = of_str s in
    if not (Types.term_eq int_sym_cmp t t') then begin
      Printf.eprintf "  parse roundtrip FAIL: %s\n" s;
      Printf.eprintf "    reparsed as: %s\n" (to_str t');
      assert false
    end) terms;
  (* The bv domain prints multi-char operators (<<, >>) in prefix form
     `<<(a,b)`; the domain's term_of_string must read those back. *)
  let bv = Domain_bv.bv_domain in
  let n s a = Types.Node (s, a) in
  let bv_terms = [
    n Domain_bv.Shl [v 'a'; v 'b'];                              (* <<(a,b) *)
    n Domain_bv.Shr [h 1; h 0];                                  (* >>(B,A) *)
    n Domain_bv.Not [n Domain_bv.Shl [v 'a'; h 0]];              (* (~<<(a,A)) *)
    n Domain_bv.Plus [v 'a'; n Domain_bv.Shr [v 'b'; v 'c']];    (* (a+>>(b,c)) *)
    n Domain_bv.Or [n Domain_bv.Shl [v 'a'; v 'b']; v 'c'];      (* (<<(a,b)|c) *)
  ] in
  List.iter (fun t ->
    let s = bv.Domain.term_to_string t in
    let t' = bv.Domain.term_of_string s in
    if not (Types.term_eq Domain_bv.compare_symbol t t') then begin
      Printf.eprintf "  bv parse roundtrip FAIL: %s -> %s\n" s (bv.Domain.term_to_string t');
      assert false
    end) bv_terms;
  Printf.printf "  parse roundtrip: OK (%d int + %d bv terms)\n"
    (List.length terms) (List.length bv_terms)

(* End-to-end: save rules, reload them, normalize the same term with
   both rule sets, get identical results. Locks in the file format and
   the parser/printer pair. *)
let test_save_load_normalize () =
  Random.init 42;
  let rs, _ = Algorithm.run ~max_size:5 int_dom ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:3 in
  let rules_mem = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let path = Filename.temp_file "rule_enum_test" ".rules" in
  Parse.save_rules int_dom.Domain.term_to_string path rules_mem;
  let rules_loaded = Parse.load_rules int_dom.Domain.term_of_string path in
  assert (List.length rules_mem = List.length rules_loaded);
  (* Normalize a battery of terms with each rule set. *)
  let probe_terms = [
    i_node i_plus [v 'b'; v 'a'];
    i_node i_times [v 'c'; v 'a'];
    i_node Domain_int.UMinus [i_node Domain_int.UMinus [v 'a']];
    i_node i_minus [i_node i_plus [v 'a'; v 'b']; v 'b'];
  ] in
  List.iter (fun t ->
    let nm, _ = Rewrite.normalize_with_index ~sym_cmp:int_sym_cmp rules_mem t in
    let nl, _ = Rewrite.normalize_with_index ~sym_cmp:int_sym_cmp rules_loaded t in
    assert (Types.term_eq int_sym_cmp
              (Types.canonicalize nm) (Types.canonicalize nl))) probe_terms;
  Sys.remove path;
  Printf.printf "  save+load+normalize: OK (%d rules round-tripped)\n"
    (List.length rules_mem)

(* Full validation in three parts.

   Part 1 — RULE SOUNDNESS. Every emitted rule (lhs, rhs) must satisfy
   `lhs ≡ rhs` under SMT. A counterexample means the synthesis produced
   an unsound rule (a real correctness bug).

   Part 2 — REWRITE SOUNDNESS. Enumerate every canonical hole-only term
   of size ≤ max_size. For each, the rewrite engine's normalization must
   be SMT-equivalent to the input. Catches mistakes where the rewrite
   engine changes a term's semantic value.

   Part 3 — CONFLUENCE. Within the same SMT class of enumerated terms,
   all members must normalize to the same syntactic form. This is the
   confluence + completeness check.

   We restrict to hole-only terms because they are the schema-level
   analogue of ground user inputs (no schema vars, only constPs). The
   normalization helper deliberately does NOT alpha-rename hole ids at
   the end — that would map `(-B)` to `(-A)` and break semantic
   equivalence between input and output. *)
(* Generate every well-formed term of size n over the given symbols,
   using `k` hole leaves. No canonicalization, no orientation filter —
   this is the full unrestricted Herbrand universe restricted to the
   specified leaves and symbol set. Used by the all-possible-terms test
   to validate the rewrite engine on inputs the synthesis enumerator
   would never produce as canonical candidates. *)
let rec all_possible_terms_of_size all_symbols ~k size =
  let leaves = List.init k (fun i -> Types.Hole i) in
  let zero_arity = List.filter_map (fun (_, ar, s) ->
    if ar = 0 then Some (Types.Node (s, [])) else None) all_symbols in
  if size <= 0 then []
  else if size = 1 then leaves @ zero_arity
  else
    let result = ref [] in
    List.iter (fun (_, arity, sym) ->
      if arity > 0 then
        let arg_size_parts = Enum.partitions (size - 1) arity in
        List.iter (fun arg_sizes ->
          let per_arg = List.map
            (fun s -> all_possible_terms_of_size all_symbols ~k s) arg_sizes in
          if List.for_all (fun l -> l <> []) per_arg then
            List.iter (fun args ->
              result := Types.Node (sym, args) :: !result)
              (Enum.product per_arg)) arg_size_parts) all_symbols;
    !result

let test_full_soundness_and_completeness () =
  Random.init 42;
  let max_size = 5 in
  let k = 3 in
  let rs, _ = Algorithm.run ~max_size int_dom ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:k ~max_holes:k ~use_smt:true in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let sym_cmp = int_sym_cmp in
  let sym_str = Domain_int.string_of_symbol in
  let smt_vars =
    List.init k Types.var_name @ List.init k Types.hole_name
  in

  (* Part 1. *)
  let n_invalid = ref 0 in
  List.iter (fun (lhs, rhs) ->
    match Smt.check_equiv int_dom smt_vars lhs rhs with
    | Smt.Equivalent -> ()
    | Smt.CounterExample cex ->
      incr n_invalid;
      Printf.eprintf "  INVALID rule: %s -> %s\n    counterexample: %s\n"
        (Types.to_string sym_str lhs) (Types.to_string sym_str rhs)
        (String.concat " " (List.map (fun (n, v) -> Printf.sprintf "%s=%d" n v) cex))
    | Smt.Unknown ->
      incr n_invalid;
      Printf.eprintf "  UNKNOWN (could not prove) rule: %s -> %s\n"
        (Types.to_string sym_str lhs) (Types.to_string sym_str rhs)) rules;
  assert (!n_invalid = 0);

  (* Enumerate every canonical hole-only term of size ≤ max_size. *)
  let caps : Enum.caps = { max_vars = 0; max_holes = k; max_vcs = k } in
  let acc = ref [] in
  for sz = 1 to max_size do
    let ts = Enum.enumerate_terms_caps int_dom.Domain.all_symbols !acc sz caps in
    acc := ts @ !acc
  done;
  let all_terms = !acc in

  (* Normalize by rewriting alone — no canonicalize at the end, since
     canonicalize alpha-renames hole ids and breaks semantic preservation
     for rewrite outputs. *)
  let index = Rewrite.index_rules rules in
  let normalize t = Rewrite.norm_bottom ~sym_cmp ~index t in
  let norm = List.map (fun t -> (t, normalize t)) all_terms in

  (* Part 2: rewrite soundness. *)
  let n_unsound = ref 0 in
  List.iter (fun (t, t_norm) ->
    if not (Types.term_eq sym_cmp t t_norm) then
      match Smt.check_equiv int_dom smt_vars t t_norm with
      | Smt.Equivalent -> ()
      | Smt.CounterExample cex ->
        incr n_unsound;
        Printf.eprintf "  REWRITE UNSOUND: %s normalized to %s, not equivalent (cex: %s)\n"
          (Types.to_string sym_str t) (Types.to_string sym_str t_norm)
          (String.concat " " (List.map (fun (n, v) -> Printf.sprintf "%s=%d" n v) cex))
      | Smt.Unknown -> ()) norm;
  assert (!n_unsound = 0);

  (* Part 3: confluence. Pre-partition by behavior vector, then SMT
     pairwise within each bv-group to build the true equivalence classes.
     All members of one class must normalize to the same term. *)
  let inputs = Eval.generate_inputs int_dom 50 k in
  let compiled = Array.of_list (List.map Eval.compile inputs) in
  let bv t = Eval.behavior_compiled_arr int_dom compiled t in
  let bv_groups = Hashtbl.create 64 in
  List.iter (fun (t, t_norm) ->
    let b = bv t in
    let cur = try Hashtbl.find bv_groups b with Not_found -> [] in
    Hashtbl.replace bv_groups b ((t, t_norm) :: cur)) norm;

  let n_classes = ref 0 in
  let n_smt_calls = ref 0 in
  Hashtbl.iter (fun _ members ->
    let n = List.length members in
    let arr = Array.of_list members in
    let parent = Array.init n (fun i -> i) in
    let rec find i = if parent.(i) = i then i
      else (let p = find parent.(i) in parent.(i) <- p; p) in
    let union i j = let ri = find i and rj = find j in
      if ri <> rj then parent.(ri) <- rj in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        if find i <> find j then begin
          let (ti, _) = arr.(i) and (tj, _) = arr.(j) in
          incr n_smt_calls;
          match Smt.check_equiv int_dom smt_vars ti tj with
          | Smt.Equivalent -> union i j
          | _ -> ()
        end
      done
    done;
    let classes = Hashtbl.create 8 in
    for i = 0 to n - 1 do
      let r = find i in
      let prev = try Hashtbl.find classes r with Not_found -> [] in
      Hashtbl.replace classes r (i :: prev)
    done;
    Hashtbl.iter (fun _r idxs ->
      incr n_classes;
      let normalized_forms = List.map (fun i -> let (_, n) = arr.(i) in n) idxs in
      match normalized_forms with
      | [] | [_] -> ()
      | first :: rest ->
        List.iter (fun other ->
          if not (Types.term_eq sym_cmp first other) then begin
            Printf.eprintf "  CONFLUENCE FAIL within an SMT class:\n";
            List.iter (fun j ->
              let (t, n) = arr.(j) in
              Printf.eprintf "    %s -> %s\n"
                (Types.to_string sym_str t) (Types.to_string sym_str n)) idxs;
            assert false
          end) rest) classes) bv_groups;
  Printf.printf
    "  full soundness+confluence (hole-only, size≤%d): OK (%d rules valid, %d terms, %d classes, %d SMT calls)\n"
    max_size (List.length rules) (List.length all_terms) !n_classes !n_smt_calls

(* The HARDEST test: enumerate every well-formed term of size ≤ max_size,
   without any canonicalization or orientation filter. This includes
   terms like `(C+A)`, `(B-A)`, `Hole 1 + Hole 0` and other hole-id
   permutations that the synthesis enumerator filters out as "same orbit".

   For each such term we check:
     - normalize(t) ≡ t under SMT (rewrite soundness on arbitrary input).
     - terms in the same SMT class normalize to the same form modulo
       alpha-renaming (confluence).

   This is the strongest correctness property the algorithm can provide
   for ground user inputs: any term someone might pass in gets reduced
   consistently to a single normal form.

   Cost grows ~quadratically with the number of terms in each bv-group.
   At max_size=6 with k=3 we get ~3500 terms and ~6500 SMT calls (~40s).
   max_size=5 is ~5s if a faster test cycle is needed. *)
let test_all_possible_terms_soundness_and_confluence () =
  Random.init 42;
  let max_size = 6 in
  let k = 3 in
  (* Generate a rule set up to a LARGER size than max_size so that rules
     for normalizing the small-size targets are all present. *)
  let rs, _ = Algorithm.run ~max_size:7 int_dom ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:k ~max_holes:k ~use_smt:true in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let sym_cmp = int_sym_cmp in
  let sym_str = Domain_int.string_of_symbol in
  let smt_vars = List.init k Types.var_name @ List.init k Types.hole_name in

  (* Enumerate every possible term of size 1..max_size — full Herbrand. *)
  let all_terms =
    let acc = ref [] in
    for sz = 1 to max_size do
      let ts = all_possible_terms_of_size int_dom.Domain.all_symbols ~k sz in
      acc := ts @ !acc
    done;
    !acc
  in

  let index = Rewrite.index_rules rules in
  let normalize t = Rewrite.norm_bottom ~sym_cmp ~index t in
  let norm = List.map (fun t -> (t, normalize t)) all_terms in

  (* Soundness: normalize(t) ≡ t under SMT for every t. To keep this
     tractable we only SMT-check when t and t_norm differ syntactically. *)
  let n_unsound = ref 0 in
  let n_sound_calls = ref 0 in
  List.iter (fun (t, t_norm) ->
    if not (Types.term_eq sym_cmp t t_norm) then begin
      incr n_sound_calls;
      match Smt.check_equiv int_dom smt_vars t t_norm with
      | Smt.Equivalent -> ()
      | Smt.CounterExample cex ->
        incr n_unsound;
        Printf.eprintf "  REWRITE UNSOUND on full input: %s normalized to %s (cex: %s)\n"
          (Types.to_string sym_str t) (Types.to_string sym_str t_norm)
          (String.concat " " (List.map (fun (n, v) -> Printf.sprintf "%s=%d" n v) cex))
      | Smt.Unknown -> ()
    end) norm;
  assert (!n_unsound = 0);

  (* Confluence: pre-partition by bv (different bv ⇒ provably non-equiv),
     then pairwise SMT-confirm within each bv group. Every member of an
     SMT-equivalence class must normalize to the same syntactic term. *)
  let inputs = Eval.generate_inputs int_dom 50 k in
  let compiled = Array.of_list (List.map Eval.compile inputs) in
  let bv t = Eval.behavior_compiled_arr int_dom compiled t in
  let bv_groups = Hashtbl.create 64 in
  List.iter (fun (t, t_norm) ->
    let b = bv t in
    let cur = try Hashtbl.find bv_groups b with Not_found -> [] in
    Hashtbl.replace bv_groups b ((t, t_norm) :: cur)) norm;

  let n_classes = ref 0 in
  let n_pairwise_calls = ref 0 in
  Hashtbl.iter (fun _ members ->
    let n = List.length members in
    let arr = Array.of_list members in
    let parent = Array.init n (fun i -> i) in
    let rec find i = if parent.(i) = i then i
      else (let p = find parent.(i) in parent.(i) <- p; p) in
    let union i j = let ri = find i and rj = find j in
      if ri <> rj then parent.(ri) <- rj in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        if find i <> find j then begin
          let (ti, _) = arr.(i) and (tj, _) = arr.(j) in
          incr n_pairwise_calls;
          match Smt.check_equiv int_dom smt_vars ti tj with
          | Smt.Equivalent -> union i j
          | _ -> ()
        end
      done
    done;
    let classes = Hashtbl.create 8 in
    for i = 0 to n - 1 do
      let r = find i in
      let prev = try Hashtbl.find classes r with Not_found -> [] in
      Hashtbl.replace classes r (i :: prev)
    done;
    Hashtbl.iter (fun _r idxs ->
      incr n_classes;
      (* Confluence modulo alpha-renaming: terms like `(A-A)` and `(B-B)`
         are SMT-equivalent (both = 0) but live in different hole-id
         namespaces. The algorithm cannot bridge them without producing
         free-hole rewrite rules (LHS `(A-A)`, RHS `(B-B)` would have a
         free Hole 1 in the RHS). The rewrite engine correctly keeps them
         as separate normal forms; comparing modulo alpha-renaming
         (canonicalize) collapses them. *)
      let normalized_forms = List.map (fun i ->
        let (_, n) = arr.(i) in Types.canonicalize n) idxs in
      match normalized_forms with
      | [] | [_] -> ()
      | first :: rest ->
        List.iter (fun other ->
          if not (Types.term_eq sym_cmp first other) then begin
            Printf.eprintf "  ALL-TERMS CONFLUENCE FAIL (after alpha-rename):\n";
            List.iter (fun j ->
              let (t, n) = arr.(j) in
              Printf.eprintf "    %s --> %s  (alpha: %s)\n"
                (Types.to_string sym_str t)
                (Types.to_string sym_str n)
                (Types.to_string sym_str (Types.canonicalize n))) idxs;
            assert false
          end) rest) classes) bv_groups;
  Printf.printf
    "  all-terms soundness+confluence (size≤%d): OK (%d total terms, %d classes, %d+%d SMT calls)\n"
    max_size (List.length all_terms) !n_classes !n_sound_calls !n_pairwise_calls

(* Regression: dead-hole (cancelling constP) reduction.

   A constP that cancels out — e.g. B in `A-(A+B)` ≡ -B, or B in
   `B-(A+(B+C))` ≡ -(A+C) — gives the term a behavior vector that skips
   the dead slot, so the ordinary bv-lookup never connects it to its
   smaller true equivalent. Before the fix these were wrongly kept as
   irreducibles, leaving a ground-confluence gap (two equivalent ground
   terms reaching different normal forms). Each witness must now reduce to
   a strictly smaller, SMT-equivalent normal form and must NOT be listed
   as an irreducible. *)
let test_dead_hole_reduction () =
  Random.init 42;
  let k = 3 in
  (* max_size 7 so every witness is within enumeration scope (the largest,
     A-(A+(B+C)), is size 7). A dead-hole term reduces once its smaller
     hole-compacted equivalent is a listed irreducible, which holds for any
     term of size <= max_size. *)
  let rs, _ = Algorithm.run ~max_size:7 int_dom ~num_domains:1
    ~num_random_inputs:100 ~max_vcs:k ~max_holes:k ~use_smt:true in
  let rules = rs.Algorithm.size_rules @ rs.Algorithm.kbo_rules in
  let sym_cmp = int_sym_cmp in
  let sym_str = Domain_int.string_of_symbol in
  let smt_vars = List.init k Types.var_name @ List.init k Types.hole_name in
  let index = Rewrite.index_rules rules in
  let h i = Types.mk_hole i in
  let minus a b = Types.mk_node Domain_int.Minus [a; b] in
  let plus a b = Types.mk_node Domain_int.Plus [a; b] in
  let witnesses =
    [ minus (h 0) (plus (h 0) (h 1));                       (* A-(A+B)     = -B      *)
      minus (h 0) (minus (h 0) (h 1));                      (* A-(A-B)     =  B      *)
      minus (h 0) (plus (h 0) (plus (h 1) (h 2))) ] in      (* A-(A+(B+C)) = -(B+C)  *)
  List.iter (fun t ->
    let n = Rewrite.norm_bottom ~sym_cmp ~index t in
    if Types.size n >= Types.size t then begin
      Printf.eprintf "  DEAD-HOLE NOT REDUCED: %s --> %s\n"
        (Types.to_string sym_str t) (Types.to_string sym_str n); assert false
    end;
    (match Smt.check_equiv int_dom smt_vars t n with
     | Smt.Equivalent -> ()
     | _ ->
       Printf.eprintf "  DEAD-HOLE REWRITE UNSOUND: %s --> %s\n"
         (Types.to_string sym_str t) (Types.to_string sym_str n); assert false);
    assert (not (List.exists (fun (x, _, _) -> Types.term_eq sym_cmp x t)
                   rs.Algorithm.behaviors)))
    witnesses;
  Printf.printf "  dead-hole reduction (cancelling constP): OK (%d witnesses reduced)\n"
    (List.length witnesses)

(* Tier 2 must short-circuit some SMT calls when cells are populated.
   At rand=1 size 5 we observe ~50% short-circuit; this asserts the
   cells-accumulate-then-cross-eval path actually fires. *)
let test_tier2_accumulates_and_short_circuits () =
  let snap () =
    (!Algorithm.tier_calls, !Algorithm.tier2_short_circuit,
     !Algorithm.tier3_calls, !Algorithm.tier3_cex_added)
  in
  let (tb, sb, mb, cb) = snap () in
  Random.init 42;
  let _ = Algorithm.run ~max_size:5 int_dom ~num_domains:1
    ~num_random_inputs:1 ~max_vcs:3 ~use_smt:true in
  let (t1, s1, m1, c1) = snap () in
  let dt = t1 - tb and ds = s1 - sb and dm = m1 - mb and dc = c1 - cb in
  (* At rand=1 with SMT and max_size=5, the algorithm runs Tier checks.
     We assert: at least one tier call happens, AT LEAST ONE SMT
     counterexample fires (which proves the cell-population path works),
     and Tier 2 short-circuits at least once (which proves the cross-eval
     path correctly catches the populated cells on subsequent compares). *)
  assert (dt > 0);
  assert (dc > 0);
  assert (ds > 0);
  assert (dm > 0);
  Printf.printf
    "  tier2 accumulation + short-circuit: OK (calls=%d sc=%d smt=%d cex=%d)\n"
    dt ds dm dc

let test_algorithm_int () =
  let sig' = [("-", 1, Domain_int.UMinus); ("+", 2, Domain_int.Plus); ("-", 2, Domain_int.Minus)] in
  let dom = { int_dom with Domain.all_symbols = sig' } in
  let rs, _ = Algorithm.run ~max_size:5 dom ~num_random_inputs:100 ~max_vcs:3 in
  let sym_str = Domain_int.string_of_symbol in
  let rule_strs = List.map (fun (l, r) ->
    Types.to_string sym_str l ^ " -> " ^ Types.to_string sym_str r) rs.Algorithm.size_rules in
  assert (List.mem "((a-b)+b) -> a" rule_strs);
  Printf.printf "  algorithm (int): OK\n"

let test_algorithm_bool () =
  let rs, _ = Algorithm.run ~max_size:5 bool_dom ~num_random_inputs:100 ~max_vcs:2 in
  assert (List.length rs.Algorithm.behaviors > 0);
  Printf.printf "  algorithm (bool): OK (irreducibles: %d, rules: %d)\n"
    (List.length rs.Algorithm.behaviors)
    (List.length rs.Algorithm.size_rules + List.length rs.Algorithm.kbo_rules)

let test_size_progression () =
  let rs5, _ = Algorithm.run ~max_size:5 int_dom ~num_random_inputs:100 ~max_vcs:3 in
  let rs6, _ = Algorithm.run ~max_size:6 int_dom ~num_random_inputs:100 ~max_vcs:3 in
  let c rs = List.length rs.Algorithm.behaviors in
  assert (c rs6 >= c rs5);
  Printf.printf "  size progression: OK (5:%d, 6:%d)\n" (c rs5) (c rs6)

let test_forced_inputs () =
  let forced = [[("a", true); ("b", true)]; [("a", true); ("b", false)];
                [("a", false); ("b", true)]; [("a", false); ("b", false)]] in
  (* When using forced_inputs, we still need keys for hole names since the
     algorithm may produce Hole-containing terms. Provide values for A, B too. *)
  let forced = List.map (fun env ->
    env @ [("A", false); ("B", true)]) forced in
  let rs, _ = Algorithm.run ~max_size:3 bool_dom ~num_random_inputs:0 ~max_vcs:2
      ~forced_inputs:forced in
  assert (List.length rs.Algorithm.behaviors > 0);
  Printf.printf "  forced inputs (bool): OK (irreducibles: %d)\n"
    (List.length rs.Algorithm.behaviors)

let test_all_bool_inputs () =
  let forced = Domain_bool.all_inputs 2 in
  (* 2 vars + 2 holes = 4 slots → 2^4 = 16 inputs *)
  assert (List.length forced = 16);
  let rs, _ = Algorithm.run ~max_size:3 bool_dom ~num_random_inputs:0 ~max_vcs:2
      ~forced_inputs:forced in
  assert (List.length rs.Algorithm.behaviors > 0);
  Printf.printf "  all_bool_inputs: OK (%d irreducibles)\n"
    (List.length rs.Algorithm.behaviors)

let () = Printf.printf "Running tests...\n";
  test_canonicalize (); test_distinct_vcs (); test_size (); test_has_hole ();
  test_kbo (); test_match_subst (); test_match_var_const ();
  test_commutativity_terminates ();
  test_kbo_var_count (); test_rewrite (); test_rewrite_hole_priority ();
  test_eval_int (); test_eval_bool (); test_eval_hole ();
  test_enum_size1 (); test_enum_with_holes (); test_enum_caps_separated ();
  test_hole_permutations ();
  test_hole_var_distinct_semantics ();
  test_commutativity_rule ();
  test_no_equivalent_irreducibles ();
  test_rule_set_semantic_closure ();
  test_constp_product_confluence ();
  test_var_only_mode_incompleteness ();
  test_no_shift_commutativity ();
  test_all_rules_smt_sound ();
  test_random_only_bv_unsound ();
  test_tier2_cross_eval_helper ();
  test_smt_random_equivalence ();
  test_safe_mode_no_assumed ();
  test_semantic_closure_leaf_level ();
  test_operator_level_closure ();
  test_cross_seed_determinism ();
  test_low_random_smt_no_unbound ();
  test_parse_roundtrip ();
  test_save_load_normalize ();
  test_full_soundness_and_completeness ();
  test_all_possible_terms_soundness_and_confluence ();
  test_dead_hole_reduction ();
  test_tier2_accumulates_and_short_circuits ();
  test_algorithm_int (); test_algorithm_bool (); test_size_progression ();
  test_forced_inputs (); test_all_bool_inputs ();
  test_holes_required_for_completeness ();
  Printf.printf "All tests passed!\n"
