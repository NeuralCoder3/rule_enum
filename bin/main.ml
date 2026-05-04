let signature = [
  ("+", 2);
  ("-", 2);
  ("*", 2);
]

let () =
  Random.self_init ();
  let max_size = 5 in
  let num_inputs = 100 in
  Printf.printf "Running rule enumeration up to size %d with %d inputs\n%!"
    max_size num_inputs;
  let rs = Rule_enum.Algorithm.run signature max_size num_inputs in
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
  ) rs.irreducible
