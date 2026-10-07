open Rule_enum

let failures = ref 0

let check name ok =
  Printf.printf "  %-62s %s\n%!" name (if ok then "ok" else "FAILED");
  if not ok then incr failures

let caps ~vars ~ucs = { Enumerate.vars; ucs; leaves = 3 }
let has_z3 = Sys.command "z3 -version > /dev/null 2>&1" = 0

module Suite (T : Theory.THEORY) = struct
  module Sem = Semantics.Make (T)
  module Syn = Synth.Make (T)

  let syms = Syn.syms
  let mode = if List.exists (fun (f, _) -> T.ac f) T.symbols then "ac" else "plain"
  let parse = Syntax.parse ~symbols:T.symbols ~name:T.name
  let show = Syntax.to_string T.name
  let ( >> ) s t = Kbo.greater syms s t

  let prover () =
    if has_z3 then Option.map (fun e -> let p = Smt.prover e in fun s t -> p s t = Smt.Equivalent) T.smt
    else None

  let inputs ?(samples = 0) ~vars ~ucs seed = Sem.inputs (Random.State.make [| seed |]) ~samples ~vars ~ucs
  let exact s t = Option.get (Sem.exhaustively_equal ~limit:max_int s t)
  let tested inputs s t = Array.for_all (fun i -> Sem.eval i s = Sem.eval i t) inputs

  let synthesize ?prove caps max_size =
    Syn.run caps ~max_size (Sem.oracle ?prove (inputs ~samples:200 ~vars:caps.Enumerate.vars ~ucs:caps.ucs 42))

  let constants = List.filter_map (fun (f, k) -> if k = 0 then Some (Term.App (f, [])) else None) T.symbols

  let rec ground_terms ~ucs n =
    if n = 1 then List.init ucs (fun i -> Term.Uc i) @ constants
    else
      List.concat_map
        (fun (f, k) ->
          Combi.compositions (n - 1) k
          |> Seq.flat_map (fun sizes ->
                 Combi.product (List.map (fun s -> List.to_seq (ground_terms ~ucs s)) sizes))
          |> Seq.map (fun args -> Term.App (f, args))
          |> List.of_seq)
        (List.filter (fun (_, k) -> k > 0) T.symbols)

  (* Every ground term up to [max_size] reaches a smallest equivalent, and
     equivalent terms reach the same one up to renaming of constants they
     do not depend on. *)
  let ground_complete ~equivalent ~inputs ~ucs ~max_size st =
    let index = Rewrite.of_list syms (Synth.rules st) in
    let classes = Hashtbl.create 1024 in
    let add t =
      let key = Array.map (fun i -> Sem.eval i t) inputs in
      Hashtbl.replace classes key (t :: Option.value (Hashtbl.find_opt classes key) ~default:[])
    in
    List.iter add (List.concat_map (ground_terms ~ucs) (List.init max_size succ));
    Hashtbl.to_seq_values classes
    |> Seq.for_all (fun members ->
           let normal = List.map (fun t -> (t, Rewrite.normalize syms index t)) members in
           let smallest = List.fold_left (fun m t -> min m (Term.size t)) max_int members in
           let ok =
             List.for_all (fun (t, n) -> equivalent t n && Term.size n = smallest) normal
             && List.length (List.sort_uniq compare (List.map (fun (_, n) -> Term.canonical n) normal)) = 1
           in
           if not ok then List.iter (fun (t, n) -> Printf.printf "    %s ~> %s\n" (show t) (show n)) normal;
           ok)

  let rules_admissible ~equivalent st =
    List.for_all
      (fun (l, r) -> l >> r && Term.regular ~lhs:l r && Term.normalize syms l = l && equivalent l r)
      (Synth.rules st)

  let normal_forms_irreducible st =
    let index = Rewrite.of_list syms (Synth.rules st) in
    List.for_all (fun t -> Rewrite.normalize syms index t = t) (Synth.normal_forms st)

  let round_trip st =
    let parse_rule = Syntax.parse_rule ~symbols:T.symbols ~name:T.name in
    List.for_all (fun t -> parse (show t) = t) (Synth.normal_forms st)
    && List.for_all (fun r -> parse_rule (Syntax.rule_to_string T.name r) = r) (Synth.rules st)

  let counts st =
    let rules = Synth.rules st in
    let sr = List.length (List.filter Synth.shrinking rules) in
    (sr, List.length rules - sr, List.length (Synth.normal_forms st))

  let properties ~equivalent ~complete name st =
    let name = Printf.sprintf "%s %s" mode name in
    check (name ^ ": rules admissible") (rules_admissible ~equivalent st);
    check (name ^ ": normal forms irreducible") (normal_forms_irreducible st);
    check (name ^ ": ground complete") (complete st);
    check (name ^ ": syntax round trip") (round_trip st)

  (* Equivalence is proved with z3 when available, tested otherwise. *)
  let infinite_theory name ~max_size ~complete_up_to =
    let prove = prover () in
    let many = fst (inputs ~samples:2000 ~vars:3 ~ucs:3 7) in
    let equivalent = Option.value prove ~default:(tested many) in
    properties ~equivalent
      ~complete:(ground_complete ~equivalent ~inputs:many ~ucs:3 ~max_size:complete_up_to)
      (Printf.sprintf "%s, size %d" name complete_up_to)
      (synthesize ?prove (caps ~vars:3 ~ucs:3) max_size)
end

module Bool_tests (T : Theory.THEORY with type sym = Boolean.sym and type value = bool) = struct
  module S = Suite (T)

  let run ~counts =
    let inputs = fst (S.inputs ~vars:0 ~ucs:3 0) in
    let complete = S.ground_complete ~equivalent:S.exact ~inputs ~ucs:3 ~max_size:8 in
    let constants = S.synthesize (caps ~vars:0 ~ucs:3) 10 in
    check (Printf.sprintf "%s bool constants, size 10: SR/KR/IR" S.mode) (S.counts constants = counts);
    S.properties ~equivalent:S.exact ~complete "bool constants, size 8" constants;
    S.properties ~equivalent:S.exact ~complete "bool variables and constants, size 8"
      (S.synthesize (caps ~vars:3 ~ucs:3) 8);
    check (S.mode ^ " bool variables only: rules admissible")
      (S.rules_admissible ~equivalent:S.exact (S.synthesize (caps ~vars:3 ~ucs:0) 7));
    let saturated = S.synthesize (caps ~vars:0 ~ucs:2) 50 in
    check (S.mode ^ " bool two constants: saturates with 14 normal forms")
      (S.Syn.exhausted saturated 51 && List.length (Synth.normal_forms saturated) = 14)
end

module Boolean_plain = struct include Boolean let ac _ = false end
module B = Suite (Boolean_plain)
module Bac = Suite (Boolean)

let test_basics () =
  print_endline "basics";
  let p = B.parse in
  let m pat t = Option.is_some (Rewrite.matches B.syms (p pat) (p t)) in
  check "canonical renames variables by first occurrence" (Term.canonical (p "(C&(B|C))") = p "(A&(B|A))");
  check "canonical compresses constants order-preservingly" (Term.canonical (p "(c&(b|c))") = p "(b&(a|b))");
  check "instances of a canonical term" (Seq.length (Term.instances ~vars:3 ~ucs:3 (p "(A&(a|b))")) = 9);
  check "kbo orients idempotence" B.(p "(A&A)" >> p "A");
  check "kbo leaves variable commutation unordered" (Kbo.compare B.syms (p "(A&B)") (p "(B&A)") = Kbo.Unordered);
  check "kbo orders constant commutation" B.(p "(b&a)" >> p "(a&b)");
  check "kbo respects variable occurrences" B.(p "(A&(B|B))" >> p "(B&B)" && not (p "(A&(B|C))" >> p "(B&B)"));
  check "constants match monotonically" (m "(b&a)" "(c&a)" && not (m "(b&a)" "(a&c)"));
  check "variables match subterms consistently" (m "(A&A)" "((a|b)&(a|b))" && not (m "(A&A)" "(a&b)"));
  check "constants never match variables" (not (m "(a&a)" "(A&A)"));
  check "processing order: patterns before their instances"
    (List.sort B.Syn.processing_order (List.map p [ "(b&a)"; "(A&a)"; "(A&B)"; "(A&A)"; "(a&b)" ])
    = List.map p [ "(A&B)"; "(A&A)"; "(A&a)"; "(a&b)"; "(b&a)" ])

let test_ac () =
  print_endline "ac";
  let p = Bac.parse and n t = Term.normalize Bac.syms (Bac.parse t) in
  let rewrite rules t = Rewrite.normalize Bac.syms (Rewrite.of_list Bac.syms (List.map (fun (l, r) -> (n l, n r)) rules)) (n t) in
  check "normal form flattens, sorts and nests chains" (n "((c&a)&(b&a))" = p "(a&(a&(b&c)))");
  check "normal form puts variables before constants" (n "(a&A)" = n "(A&a)" && n "(a&A)" = p "(A&a)");
  check "ac-kbo is monotone where the structural order is not"
    (Bac.(n "(a&b)" >> n "(~(~c))") && Bac.(n "(b&(a&b))" >> n "(b&(~(~c)))")
    && Term.compare B.syms (n "(b&(a&b))") (n "(b&(~(~c)))") < 0);
  check "ac-kbo orders ground chains, not open ones"
    (Bac.(n "(a&(~b))" >> n "(b&(~a))")
    && Kbo.compare Bac.syms (n "(A&(B|C))") (n "(A&(B^C))") = Kbo.Unordered);
  check "extension: absorption inside a wider disjunction" (rewrite [ ("(A|(A&B))", "A") ] "(b|(a|(a&b)))" = n "(a|b)");
  check "extension: cancellation inside a wider xor" (rewrite [ ("(A^A)", "(A^(~A))") ] "(b^(a^b))" = n "(a^(b^(~b)))");
  check "constants still match monotonically" (rewrite [ ("(a^(~b))", "(~(a^b))") ] "((~a)^b)" = n "((~a)^b)");
  check "constants still never match variables" (rewrite [ ("(a&a)", "a") ] "(A&A)" = n "(A&A)")

let test_enumeration () =
  print_endline "enumeration";
  let st = B.synthesize (caps ~vars:3 ~ucs:3) 6 in
  let index = Rewrite.of_list B.syms (Synth.rules st) in
  let terms =
    List.of_seq (Enumerate.terms B.syms (caps ~vars:3 ~ucs:3) Boolean.symbols (fun s -> Synth.find s st.normal_forms) 7)
  in
  let arguments_normal = function
    | Term.App (_, ts) -> List.for_all (fun t -> Rewrite.normalize B.syms index t = t) ts
    | _ -> true
  in
  check "generated terms are canonical" (List.for_all (fun t -> Term.canonical t = t) terms);
  check "generated terms are distinct" (List.length (List.sort_uniq compare terms) = List.length terms);
  check "arguments are normal forms" (List.for_all arguments_normal terms)

module Bool_plain_tests = Bool_tests (Boolean_plain)
module Bool_ac_tests = Bool_tests (Boolean)

let test_bool () =
  print_endline "bool";
  Bool_plain_tests.run ~counts:(18782, 1013, 232);
  Bool_ac_tests.run ~counts:(5675, 387, 232)

module Bv4 = Bitvector.Make (struct let width = 4 end)
module Bv4_plain = struct include Bv4 let ac _ = false end

module Bv4_tests (T : Theory.THEORY with type sym = Bv4.sym and type value = int) = struct
  module S = Suite (T)

  let run () =
    let inputs = fst (S.Sem.inputs ~exhaustive_limit:max_int (Random.State.make [| 0 |]) ~samples:0 ~vars:0 ~ucs:2) in
    S.properties ~equivalent:S.exact
      ~complete:(S.ground_complete ~equivalent:S.exact ~inputs ~ucs:2 ~max_size:4)
      "bv4, size 4" (S.synthesize (caps ~vars:2 ~ucs:2) 5)
end

module Bv4_plain_tests = Bv4_tests (Bv4_plain)
module Bv4_ac_tests = Bv4_tests (Bv4)

let test_bv4 () =
  print_endline "bv4";
  Bv4_plain_tests.run ();
  Bv4_ac_tests.run ()

module I = Suite (struct include Integer let ac _ = false end)
module Iac = Suite (Integer)
module D = Suite (struct include Demo let ac _ = false end)
module Dac = Suite (Demo)

let test_infinite () =
  print_endline (if has_z3 then "int / demo (z3)" else "int / demo (random testing, z3 not found)");
  I.infinite_theory "int" ~max_size:6 ~complete_up_to:5;
  Iac.infinite_theory "int" ~max_size:6 ~complete_up_to:5;
  D.infinite_theory "demo" ~max_size:7 ~complete_up_to:7;
  Dac.infinite_theory "demo" ~max_size:7 ~complete_up_to:7

let () =
  test_basics ();
  test_ac ();
  test_enumeration ();
  test_bool ();
  test_bv4 ();
  test_infinite ();
  if !failures > 0 then (
    Printf.printf "%d check(s) failed\n" !failures;
    exit 1)
