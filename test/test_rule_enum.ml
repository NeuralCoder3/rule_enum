open Rule_enum

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
  assert (Eval.eval inputs t1 = 5 + 3);
  assert (Eval.eval inputs t1 = 8);

  let t2 = Types.Node ("-", [Types.Var "a"; Types.Var "b"]) in
  assert (Eval.eval inputs t2 = 5 - 3);

  let t3 = Types.Node ("*", [Types.Var "a"; Types.Var "b"]) in
  assert (Eval.eval inputs t3 = 5 * 3);

  let t4 = Types.Node ("+", [Types.Node ("-", [Types.Var "a"; Types.Var "b"]);
                              Types.Var "b"]) in
  assert (Eval.eval inputs t4 = 5);

  Printf.printf "  eval (int): OK\n"

let test_enum () =
  let terms = Enum.enumerate_terms [("+", 2)] [] 1 3 in
  assert (List.length terms = 1);
  assert (Types.to_string (List.hd terms) = "a");

  let irr = [Types.Var "a"] in
  let terms3 = Enum.enumerate_terms [("+", 2)] irr 3 3 in
  let strs = List.map Types.to_string terms3 |> List.sort String.compare in
  assert (List.length strs >= 2);
  assert (List.mem "(a+a)" strs);
  assert (List.mem "(a+b)" strs);
  Printf.printf "  enum: OK\n"

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
  let sig' = [("+", 2); ("-", 2)] in
  let rs = Algorithm.run sig' 5 100 3 in

  let rule_strs = List.map (fun (l, r) ->
    Types.to_string l ^ " -> " ^ Types.to_string r
  ) rs.size_rules in

  assert (List.mem "((a-b)+b) -> a" rule_strs);
  assert (List.mem "(a+(b-b)) -> a" rule_strs);
  assert (List.mem "((a+a)-a) -> a" rule_strs);

  Printf.printf "  algorithm (int): OK\n"

let () =
  Printf.printf "Running tests...\n";
  test_canonicalize ();
  test_distinct_vars ();
  test_size ();
  test_kbo ();
  test_match_renaming ();
  test_rewrite ();
  test_eval_int ();
  test_enum ();
  test_enum_max_vars ();
  test_algorithm_int ();
  Printf.printf "All tests passed!\n"
