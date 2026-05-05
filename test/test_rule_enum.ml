open Rule_enum

let v c = Types.Var (Char.code c - Char.code 'a')

let int_dom = Domain_int.int_domain
let bool_dom = Domain_bool.bool_domain

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
  let c1 = Types.canonicalize (v 'b') in assert (Types.to_string (fun _ -> "") c1 = "a");
  Printf.printf "  canonicalize: OK\n"

let test_distinct_vars () =
  assert (Types.distinct_vars (v 'a') = 1);
  Printf.printf "  distinct_vars: OK\n"

let test_size () =
  assert (Types.size (v 'a') = 1);
  Printf.printf "  size: OK\n"

let test_kbo () =
  let a = v 'a' and b = v 'b' in
  assert (Kbo.lt Domain_int.compare_symbol a (i_node i_plus [a; b]));
  Printf.printf "  kbo: OK\n"

let test_match_renaming () =
  let lhs = i_node i_plus [v 'a'; v 'b'] in
  assert (Option.is_some (Types.match_renaming lhs (i_node i_plus [v 'x'; v 'y'])));
  assert (Option.is_none (Types.match_renaming lhs (i_node i_plus [v 'x'; v 'x'])));
  let lhs2 = i_node i_plus [v 'a'; v 'a'] in
  assert (Option.is_some (Types.match_renaming lhs2 (i_node i_plus [v 'x'; v 'x'])));
  assert (Option.is_none (Types.match_renaming lhs2 (i_node i_plus [v 'x'; v 'y'])));
  Printf.printf "  match_renaming: OK\n"

let test_rewrite () =
  let sym_str = Domain_int.string_of_symbol in
  let rules = [(i_node i_plus [v 'a'; v 'a'], v 'a')] in
  let r1, sr1 = Rewrite.normalize_with_index rules (i_node i_plus [v 'b'; v 'b']) in
  assert (Types.to_string sym_str r1 = "a"); assert sr1;
  let r2, sr2 = Rewrite.normalize_with_index rules (i_node i_plus [v 'b'; v 'c']) in
  assert (Types.to_string sym_str r2 = "(a+b)"); assert (not sr2);
  Printf.printf "  rewrite: OK\n"

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

let test_enum_max_vars () =
  let irr = [v 'a'] in
  let terms = Enum.enumerate_terms [("+", 2, Domain_int.Plus)] irr 3 1 in
  List.iter (fun t -> assert (Types.distinct_vars t <= 1)) terms;
  Printf.printf "  enum max_vars: OK\n"

let test_algorithm_int () =
  let sig' = [("-", 1, Domain_int.UMinus); ("+", 2, Domain_int.Plus); ("-", 2, Domain_int.Minus)] in
  let dom = { int_dom with Domain.all_symbols = sig' } in
  let rs, _ = Algorithm.run ~max_size:5 dom ~num_random_inputs:100 ~max_vars:3 in
  let sym_str = Domain_int.string_of_symbol in
  let rule_strs = List.map (fun (l, r) ->
    Types.to_string sym_str l ^ " -> " ^ Types.to_string sym_str r) rs.Algorithm.size_rules in
  assert (List.mem "((a-b)+b) -> a" rule_strs);
  Printf.printf "  algorithm (int): OK\n"

let test_algorithm_bool () =
  let rs, _ = Algorithm.run ~max_size:5 bool_dom ~num_random_inputs:100 ~max_vars:2 in
  assert (List.length rs.Algorithm.behaviors > 0);
  Printf.printf "  algorithm (bool): OK (irreducibles: %d, rules: %d)\n"
    (List.length rs.Algorithm.behaviors)
    (List.length rs.Algorithm.size_rules + List.length rs.Algorithm.kbo_rules)

let test_size_progression () =
  let rs5, _ = Algorithm.run ~max_size:5 int_dom ~num_random_inputs:100 ~max_vars:3 in
  let rs6, _ = Algorithm.run ~max_size:6 int_dom ~num_random_inputs:100 ~max_vars:3 in
  let rs7, _ = Algorithm.run ~max_size:7 int_dom ~num_random_inputs:100 ~max_vars:3 in
  let c rs = List.length rs.Algorithm.behaviors in
  assert (c rs6 > c rs5); assert (c rs7 > c rs6);
  Printf.printf "  size progression: OK (5:%d, 6:%d, 7:%d)\n" (c rs5) (c rs6) (c rs7)

let test_forced_inputs () =
  let forced = [[("a", true); ("b", true)]; [("a", true); ("b", false)];
                [("a", false); ("b", true)]; [("a", false); ("b", false)]] in
  let rs, _ = Algorithm.run ~max_size:3 bool_dom ~num_random_inputs:0 ~max_vars:2
      ~forced_inputs:forced in
  assert (List.length rs.Algorithm.behaviors > 0);
  Printf.printf "  forced inputs (bool, exhaustive): OK (irreducibles: %d)\n"
    (List.length rs.Algorithm.behaviors)

let test_all_bool_inputs () =
  let forced = Domain_bool.all_inputs 2 in
  assert (List.length forced = 4);
  let rs, _ = Algorithm.run ~max_size:3 bool_dom ~num_random_inputs:0 ~max_vars:2
      ~forced_inputs:forced in
  let rs_rand, _ = Algorithm.run ~max_size:3 bool_dom ~num_random_inputs:100 ~max_vars:2 in
  assert (List.length rs.Algorithm.behaviors = List.length rs_rand.Algorithm.behaviors);
  Printf.printf "  all_bool_inputs: OK\n"

let () = Printf.printf "Running tests...\n";
  test_canonicalize (); test_distinct_vars (); test_size ();
  test_kbo (); test_match_renaming (); test_rewrite ();
  test_eval_int (); test_eval_bool (); test_enum_max_vars ();
  test_algorithm_int (); test_algorithm_bool (); test_size_progression ();
  test_forced_inputs (); test_all_bool_inputs ();
  Printf.printf "All tests passed!\n"
