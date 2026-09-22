module RE = Rule_enum
let d = RE.Domain_bool.bool_domain
let cmp = d.RE.Domain.sym_compare
let ac = d.RE.Domain.is_ac
let p = d.RE.Domain.term_of_string
let s = d.RE.Domain.term_to_string
let acn t = RE.Types.ac_normalize ac cmp (p t)
let eq a b = RE.Types.term_eq cmp (acn a) (acn b)
let () =
  (* ac_normalize *)
  assert (eq "(B&A)" "(A&B)");
  assert (eq "((A&B)&C)" "(A&(B&C))");
  assert (eq "(A&(C&B))" "((A&B)&C)");
  assert (eq "(C^(A^B))" "(A^(B^C))");
  assert (not (eq "(A&A)" "A"));            (* idempotence not AC *)
  assert (not (eq "(A&B)" "(A|B)"));        (* different ops *)
  Printf.printf "ac_normalize: OK  e.g. (B&A)->%s  ((A&B)&C)->%s\n"
    (s (acn "(B&A)")) (s (acn "((A&B)&C)"));
  (* ac_match_root: absorption a|(a&b) -> a *)
  let lhs = p "(a|(a&b))" and rhs = p "a" in
  let test tgt exp =
    let t = acn tgt in
    match RE.Types.ac_match_root ac cmp lhs t with
    | Some (st, leftover) ->
      let r = if leftover=[] then RE.Types.apply_ac_subst st rhs
        else RE.Types.ac_join cmp RE.Domain_bool.Or (RE.Types.apply_ac_subst st rhs :: leftover) in
      Printf.printf "  absorption on %-18s -> %s (leftover %d)\n" tgt (s r) (List.length leftover);
      assert (RE.Types.term_eq cmp r (acn exp))
    | None -> Printf.printf "  absorption on %-18s -> NO MATCH\n" tgt; assert (exp="__nomatch__")
  in
  test "(X|(X&Y))" "X";
  test "((X&Y)|X)" "X";
  test "(X|(Y|(X&Y)))" "(X|Y)";     (* extension: leftover Y *)
  test "(X|Y)" "__nomatch__";        (* no (a&b) operand *)
  (* xor cancel a^(a^b) -> b, modulo AC *)
  let lx = p "(a^(a^b))" and rx = p "b" in
  (match RE.Types.ac_match_root ac cmp lx (acn "(Y^(X^X))") with
   | Some (st,lo) -> let r=if lo=[] then RE.Types.apply_ac_subst st rx else RE.Types.ac_join cmp RE.Domain_bool.Xor (RE.Types.apply_ac_subst st rx::lo) in
     Printf.printf "  xor-cancel on (Y^(X^X)) -> %s\n" (s r); assert (RE.Types.term_eq cmp r (acn "Y"))
   | None -> Printf.printf "  xor-cancel: NO MATCH\n"; assert false);
  print_endline "ALL AC CORE TESTS PASSED"
