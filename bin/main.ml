let () =
  Random.self_init ();
  (* let dom = Rule_enum.Domain_int.int_domain in *)
  let dom = Rule_enum.Domain_bool.bool_domain in
  let max_size = 5 in
  let num_inputs = 100 in
  let max_vars = 2 in

  Printf.printf "Domain: bool, max vars: %d, max size: %d\n%!"
    max_vars max_size;

  let rs = Rule_enum.Algorithm.run dom max_size num_inputs max_vars in
  Printf.printf "\n=== Size-reducing rules (count: %d) ===\n"
    (List.length rs.size_rules);
  List.iter (fun (l, r) ->
    Printf.printf "  %s  ->  %s\n"
      (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
  ) rs.size_rules;
  Printf.printf "\n=== KBO-simplifying rules (count: %d) ===\n"
    (List.length rs.kbo_rules);
  List.iter (fun (l, r) ->
    Printf.printf "  %s  ->  %s\n"
      (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
  ) rs.kbo_rules;
  Printf.printf "\n=== Irreducible terms (count: %d) ===\n"
    (List.length rs.irreducible);
  List.iter (fun t ->
    Printf.printf "  %s  (size %d)\n"
      (Rule_enum.Types.to_string t) (Rule_enum.Types.size t)
  ) rs.irreducible;
  Printf.printf "\nSize-reducing rules: %d\n"    (List.length rs.size_rules);
  Printf.printf "KBO-simplifying rules: %d\n" (List.length rs.kbo_rules);
  Printf.printf "Irreducible terms: %d\n"     (List.length rs.irreducible);
  ()
