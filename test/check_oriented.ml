(* Standalone audit: every rule in a .rules file must be strictly
   KBO-oriented (RHS KBO-smaller than LHS). Usage: check_oriented FILE *)
let () =
  let module RE = Rule_enum in
  let dom = RE.Domain_bool.bool_domain in
  let cmp = dom.RE.Domain.sym_compare in
  let path = Sys.argv.(1) in
  let rules = RE.Parse.load_rules dom.RE.Domain.term_of_string path in
  let bad = List.filter (fun (l, r) -> RE.Kbo.kbo cmp l r <> RE.Kbo.Greater) rules in
  let bstr = RE.Types.to_string RE.Domain_bool.string_of_symbol in
  Printf.printf "rules: %d   not-KBO-Greater: %d\n" (List.length rules) (List.length bad);
  List.iteri (fun i (l, r) ->
    if i < 20 then
      Printf.printf "  BAD: %s -> %s   (kbo = %s)\n" (bstr l) (bstr r)
        (match RE.Kbo.kbo cmp l r with
         | RE.Kbo.Less -> "Less" | RE.Kbo.Equal -> "Equal"
         | RE.Kbo.Greater -> "Greater" | RE.Kbo.Incomparable -> "Incomparable"))
    bad;
  if bad = [] then print_endline "ALL RULES KBO-ORIENTED" else exit 1
