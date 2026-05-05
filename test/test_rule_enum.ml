open Rule_enum

let v c = Types.Var (Char.code c - Char.code 'a')
let n s args = Types.Node (Types.Sym.of_string s, args)

let int_dom = Domain_int.int_domain
let bool_dom = Domain_bool.bool_domain

let test_canonicalize () =
  let c1 = Types.canonicalize (v 'b') in assert (Types.to_string c1 = "a");
  let c2 = Types.canonicalize (n "+" [v 'b'; v 'a']) in assert (Types.to_string c2 = "(a+b)");
  let c3 = Types.canonicalize (n "+" [v 'x'; v 'x']) in assert (Types.to_string c3 = "(a+a)");
  let c4 = Types.canonicalize (n "-" [v 'y'; v 'x']) in assert (Types.to_string c4 = "(a-b)");
  Printf.printf "  canonicalize: OK\n"

let test_distinct_vars () =
  assert (Types.distinct_vars (v 'a') = 1);
  assert (Types.distinct_vars (n "+" [v 'a'; v 'b']) = 2);
  assert (Types.distinct_vars (n "+" [v 'a'; v 'a']) = 1);
  Printf.printf "  distinct_vars: OK\n"

let test_size () =
  let a = v 'a' in assert (Types.size a = 1);
  let ab = n "+" [a; v 'b'] in assert (Types.size ab = 3);
  assert (Types.size (n "+" [ab; v 'c']) = 5);
  Printf.printf "  size: OK\n"

let test_kbo () =
  let a = v 'a' and b = v 'b' in
  let apb = n "+" [a; b] in assert (Kbo.lt a apb);
  let apbpc = n "+" [apb; v 'c'] in
  let apbpc2 = n "+" [a; n "+" [b; v 'c']] in
  assert (Kbo.lt apbpc2 apbpc);
  Printf.printf "  kbo: OK\n"

let test_match_renaming () =
  let lhs = n "+" [v 'a'; v 'b'] in
  assert (Option.is_some (Types.match_renaming lhs (n "+" [v 'x'; v 'y'])));
  assert (Option.is_none (Types.match_renaming lhs (n "+" [v 'x'; v 'x'])));
  let lhs2 = n "+" [v 'a'; v 'a'] in
  assert (Option.is_some (Types.match_renaming lhs2 (n "+" [v 'x'; v 'x'])));
  assert (Option.is_none (Types.match_renaming lhs2 (n "+" [v 'x'; v 'y'])));
  Printf.printf "  match_renaming: OK\n"

let test_rewrite () =
  let rules = [(n "+" [v 'a'; v 'a'], v 'a')] in
  let r1, sr1 = Rewrite.normalize_with_index rules (n "+" [v 'b'; v 'b']) in
  assert (Types.to_string r1 = "a"); assert sr1;
  let r2, sr2 = Rewrite.normalize_with_index rules (n "+" [v 'b'; v 'c']) in
  assert (Types.to_string r2 = "(a+b)"); assert (not sr2);
  Printf.printf "  rewrite: OK\n"

let test_eval_int () =
  let inputs = [("a", 5); ("b", 3)] in
  assert (Eval.eval int_dom inputs (n "+" [v 'a'; v 'b']) = 8);
  assert (Eval.eval int_dom inputs (n "-" [v 'a'; v 'b']) = 2);
  assert (Eval.eval int_dom inputs (n "*" [v 'a'; v 'b']) = 15);
  assert (Eval.eval int_dom inputs (n "+" [n "-" [v 'a'; v 'b']; v 'b']) = 5);
  Printf.printf "  eval (int): OK\n"

let test_eval_bool () =
  let inputs = [("a", true); ("b", false)] in
  assert (Eval.eval bool_dom inputs (n "&" [v 'a'; v 'b']) = false);
  assert (Eval.eval bool_dom inputs (n "|" [v 'a'; v 'b']) = true);
  assert (Eval.eval bool_dom inputs (n "^" [v 'a'; v 'b']) = true);
  assert (Eval.eval bool_dom inputs (n "^" [v 'a'; v 'a']) = false);
  Printf.printf "  eval (bool): OK\n"

let test_enum_max_vars () =
  let irr = [v 'a'] in
  let terms = Enum.enumerate_terms [("+", 2)] irr 3 1 in
  List.iter (fun t -> assert (Types.distinct_vars t <= 1)) terms;
  let strs = List.map Types.to_string terms |> List.sort String.compare in
  assert (List.mem "(a+a)" strs); assert (not (List.mem "(a+b)" strs));
  Printf.printf "  enum max_vars: OK\n"

let test_algorithm_int () =
  let sig' = [("-", 1); ("+", 2); ("-", 2)] in
  let int_dom_small = { int_dom with Domain.signature = sig' } in
  let rs, _iters = Algorithm.run ~max_size:5 int_dom_small ~num_random_inputs:100 ~max_vars:3 in

  let rule_strs = List.map (fun (l, r) ->
    Types.to_string l ^ " -> " ^ Types.to_string r
  ) rs.size_rules in

  assert (List.mem "((a-b)+b) -> a" rule_strs);
  assert (List.mem "(a+(b-b)) -> a" rule_strs);
  assert (List.mem "((a+a)-a) -> a" rule_strs);

  Printf.printf "  algorithm (int): OK\n"

let test_algorithm_bool () =
  let rs, _iters = Algorithm.run ~max_size:5 bool_dom ~num_random_inputs:100 ~max_vars:2 in

  assert (List.length rs.Algorithm.behaviors > 0);
  Printf.printf "  algorithm (bool): OK (irreducibles: %d, rules: %d)\n"
    (List.length rs.Algorithm.behaviors)
    (List.length rs.Algorithm.size_rules + List.length rs.Algorithm.kbo_rules)

let test_size_progression () =
  let rs5, _ = Algorithm.run ~max_size:5 int_dom ~num_random_inputs:100 ~max_vars:3 in
  let rs6, _ = Algorithm.run ~max_size:6 int_dom ~num_random_inputs:100 ~max_vars:3 in
  let rs7, _ = Algorithm.run ~max_size:7 int_dom ~num_random_inputs:100 ~max_vars:3 in

  let count rs = List.length rs.Algorithm.behaviors in
  assert (count rs6 > count rs5);
  assert (count rs7 > count rs6);
  Printf.printf "  size progression: OK (5:%d, 6:%d, 7:%d)\n"
    (count rs5) (count rs6) (count rs7)

let test_forced_inputs () =
  let forced = [
    [("a", true); ("b", true)];
    [("a", true); ("b", false)];
    [("a", false); ("b", true)];
    [("a", false); ("b", false)];
  ] in
  let rs, _ = Algorithm.run ~max_size:3 bool_dom
      ~num_random_inputs:0 ~max_vars:2 ~forced_inputs:forced in
  assert (List.length rs.Algorithm.behaviors > 0);
  Printf.printf "  forced inputs (bool, exhaustive): OK (irreducibles: %d)\n"
    (List.length rs.Algorithm.behaviors)

let test_all_bool_inputs () =
  let forced = Domain_bool.all_inputs 2 in
  assert (List.length forced = 4);
  let rs, _ = Algorithm.run ~max_size:3 bool_dom
      ~num_random_inputs:0 ~max_vars:2 ~forced_inputs:forced in
  let rs_rand, _ = Algorithm.run ~max_size:3 bool_dom
      ~num_random_inputs:100 ~max_vars:2 in
  assert (List.length rs.Algorithm.behaviors = List.length rs_rand.Algorithm.behaviors);
  Printf.printf "  all_bool_inputs: OK (exhaustive matches random at size 3)\n"

let () =
  Printf.printf "Running tests...\n";
  test_canonicalize ();
  test_distinct_vars ();
  test_size ();
  test_kbo ();
  test_match_renaming ();
  test_rewrite ();
  test_eval_int ();
  test_eval_bool ();
  test_enum_max_vars ();
  test_algorithm_int ();
  test_algorithm_bool ();
  test_size_progression ();
  test_forced_inputs ();
  test_all_bool_inputs ();
  Printf.printf "All tests passed!\n"
