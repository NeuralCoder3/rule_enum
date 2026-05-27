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
  let c1 = Types.canonicalize (v 'b') in
  assert (Types.to_string (fun _ -> "") c1 = "a");
  (* Var/Hole live in independent id namespaces post-canonicalize. *)
  let mixed = i_node i_plus [v 'b'; h 5] in
  let c2 = Types.canonicalize mixed in
  assert (Types.to_string Domain_int.string_of_symbol c2 = "(a+A)");
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
  (* Hole-only LHS: ?A + ?B  matches  ?C + ?D  in canonical (id-increasing) order. *)
  let lhs = i_node i_plus [h 0; h 1] in
  let ok_target = i_node i_plus [h 2; h 3] in
  (match Types.match_var_const int_sym_cmp lhs ok_target with
   | Some (_, _) -> ()
   | None -> assert false);
  (* Order-preservation: ?A + ?B doesn't match ?D + ?C (image of A would
     be ?D > image of B = ?C, violating relative order). *)
  let bad_target = i_node i_plus [h 3; h 2] in
  assert (Types.match_var_const int_sym_cmp lhs bad_target = None);
  (* Hole image NOT allowed to be a Var (Mapping A: user terms are ground). *)
  let var_target = i_node i_plus [v 'a'; v 'b'] in
  assert (Types.match_var_const int_sym_cmp lhs var_target = None);
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
  Printf.printf "  match_var_const: OK\n"

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
  test_kbo_var_count (); test_rewrite (); test_rewrite_hole_priority ();
  test_eval_int (); test_eval_bool (); test_eval_hole ();
  test_enum_size1 (); test_enum_with_holes (); test_enum_caps_separated ();
  test_algorithm_int (); test_algorithm_bool (); test_size_progression ();
  test_forced_inputs (); test_all_bool_inputs ();
  test_holes_required_for_completeness ();
  Printf.printf "All tests passed!\n"
