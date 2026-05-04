open Rule_enum

let int_dom = Domain_int.int_domain
let bool_dom = Domain_bool.bool_domain

let test_canonicalize () =
  let t1 = Types.Var "b" in
  let c1 = Types.canonicalize t1 in
  assert (Types.to_string c1 = "a");

  let t2 = Types.Node ("+", [Types.Var "b"; Types.Var "a"]) in
  let c2 = Types.canonicalize t2 in
  assert (Types.to_string c2 = "(a+b)");

  let t3 = Types.Node ("+", [Types.Var "x"; Types.Var "x"]) in
  let c3 = Types.canonicalize t3 in
  assert (Types.to_string c3 = "(a+a)");

  let t4 = Types.Node ("-", [Types.Var "y"; Types.Var "x"]) in
  let c4 = Types.canonicalize t4 in
  assert (Types.to_string c4 = "(a-b)");

  Printf.printf "  canonicalize: OK\n"

let test_distinct_vars () =
  let t1 = Types.Var "a" in
  assert (Types.distinct_vars t1 = 1);
  let t2 = Types.Node ("+", [Types.Var "a"; Types.Var "b"]) in
  assert (Types.distinct_vars t2 = 2);
  let t3 = Types.Node ("+", [Types.Var "a"; Types.Var "a"]) in
  assert (Types.distinct_vars t3 = 1);
  Printf.printf "  distinct_vars: OK\n"

let test_size () =
  let a = Types.Var "a" in
  assert (Types.size a = 1);
  let ab = Types.Node ("+", [a; Types.Var "b"]) in
  assert (Types.size ab = 3);
  let abc = Types.Node ("+", [ab; Types.Var "c"]) in
  assert (Types.size abc = 5);
  Printf.printf "  size: OK\n"

let test_kbo () =
  let a = Types.Var "a" in
  let b = Types.Var "b" in
  let apb = Types.Node ("+", [a; b]) in
  assert (Kbo.lt a apb);

  let apbpc = Types.Node ("+", [apb; Types.Var "c"]) in
  let apbpc2 = Types.Node ("+", [a; Types.Node ("+", [b; Types.Var "c"])]) in
  assert (Kbo.lt apbpc2 apbpc);

  Printf.printf "  kbo: OK\n"

let test_match_renaming () =
  let lhs = Types.Node ("+", [Types.Var "a"; Types.Var "b"]) in
  let t1 = Types.Node ("+", [Types.Var "x"; Types.Var "y"]) in
  let m1 = Types.match_renaming lhs t1 in
  assert (Option.is_some m1);

  let t2 = Types.Node ("+", [Types.Var "x"; Types.Var "x"]) in
  let m2 = Types.match_renaming lhs t2 in
  assert (Option.is_none m2);

  let lhs2 = Types.Node ("+", [Types.Var "a"; Types.Var "a"]) in
  let t3 = Types.Node ("+", [Types.Var "x"; Types.Var "x"]) in
  let m3 = Types.match_renaming lhs2 t3 in
  assert (Option.is_some m3);

  let t4 = Types.Node ("+", [Types.Var "x"; Types.Var "y"]) in
  let m4 = Types.match_renaming lhs2 t4 in
  assert (Option.is_none m4);

  Printf.printf "  match_renaming: OK\n"

let test_rewrite () =
  let rule_lhs = Types.Node ("+", [Types.Var "a"; Types.Var "a"]) in
  let rule_rhs = Types.Var "a" in
  let rules = [(rule_lhs, rule_rhs)] in

  let t1 = Types.Node ("+", [Types.Var "b"; Types.Var "b"]) in
  let result1, size_red1 = Rewrite.normalize rules t1 in
  assert (Types.to_string result1 = "a");
  assert size_red1;

  let t2 = Types.Node ("+", [Types.Var "b"; Types.Var "c"]) in
  let result2, size_red2 = Rewrite.normalize rules t2 in
  assert (Types.to_string result2 = "(a+b)");
  assert (not size_red2);

  Printf.printf "  rewrite: OK\n"

let test_eval_int () =
  let inputs = [("a", 5); ("b", 3)] in
  let t1 = Types.Node ("+", [Types.Var "a"; Types.Var "b"]) in
  assert (Eval.eval int_dom inputs t1 = 8);

  let t2 = Types.Node ("-", [Types.Var "a"; Types.Var "b"]) in
  assert (Eval.eval int_dom inputs t2 = 2);

  let t3 = Types.Node ("*", [Types.Var "a"; Types.Var "b"]) in
  assert (Eval.eval int_dom inputs t3 = 15);

  let t4 = Types.Node ("+", [Types.Node ("-", [Types.Var "a"; Types.Var "b"]);
                              Types.Var "b"]) in
  assert (Eval.eval int_dom inputs t4 = 5);

  Printf.printf "  eval (int): OK\n"

let test_eval_bool () =
  let inputs = [("a", true); ("b", false)] in
  let t1 = Types.Node ("&", [Types.Var "a"; Types.Var "b"]) in
  assert (Eval.eval bool_dom inputs t1 = false);

  let t2 = Types.Node ("|", [Types.Var "a"; Types.Var "b"]) in
  assert (Eval.eval bool_dom inputs t2 = true);

  let t3 = Types.Node ("^", [Types.Var "a"; Types.Var "b"]) in
  assert (Eval.eval bool_dom inputs t3 = true);

  let t4 = Types.Node ("^", [Types.Var "a"; Types.Var "a"]) in
  assert (Eval.eval bool_dom inputs t4 = false);

  Printf.printf "  eval (bool): OK\n"

let test_enum_max_vars () =
  let irr = [Types.Var "a"] in
  let terms = Enum.enumerate_terms [("+", 2)] irr 3 1 in
  List.iter (fun t ->
    assert (Types.distinct_vars t <= 1)
  ) terms;
  let strs = List.map Types.to_string terms |> List.sort String.compare in
  assert (List.mem "(a+a)" strs);
  assert (not (List.mem "(a+b)" strs));
  Printf.printf "  enum max_vars: OK\n"

let test_algorithm_int () =
  let sig' = [("-", 1); ("+", 2); ("-", 2)] in
  let int_dom_small = { int_dom with Domain.signature = sig' } in
  let rs = Algorithm.run int_dom_small 5 100 3 in

  let rule_strs = List.map (fun (l, r) ->
    Types.to_string l ^ " -> " ^ Types.to_string r
  ) rs.size_rules in

  assert (List.mem "((a-b)+b) -> a" rule_strs);
  assert (List.mem "(a+(b-b)) -> a" rule_strs);
  assert (List.mem "((a+a)-a) -> a" rule_strs);

  Printf.printf "  algorithm (int): OK\n"

let test_algorithm_bool () =
  let rs = Algorithm.run bool_dom 5 100 2 in

  assert (List.length rs.irreducible > 0);
  Printf.printf "  algorithm (bool): OK (irreducibles: %d, rules: %d)\n"
    (List.length rs.irreducible)
    (List.length rs.size_rules + List.length rs.kbo_rules)

let test_size_progression () =
  let rs5 = Algorithm.run int_dom 5 100 3 in
  let rs6 = Algorithm.run int_dom 6 100 3 in
  let rs7 = Algorithm.run int_dom 7 100 3 in

  let count irr = List.length irr in
  assert (count rs6.irreducible > count rs5.irreducible);
  assert (count rs7.irreducible > count rs6.irreducible);
  Printf.printf "  size progression: OK (5:%d, 6:%d, 7:%d)\n"
    (count rs5.irreducible) (count rs6.irreducible) (count rs7.irreducible)

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
  Printf.printf "All tests passed!\n"
