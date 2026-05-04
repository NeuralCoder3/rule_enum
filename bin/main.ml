let () =
  Random.self_init ();
  let dom = Rule_enum.Domain_int.int_domain in
  let num_inputs = 100 in
  let max_vars = 3 in

  Printf.printf "Domain: int,  max vars: %d,  inputs: %d\n\n%!" max_vars num_inputs;

  let rs, iterations = Rule_enum.Algorithm.run ~max_size:7 dom num_inputs max_vars in

  List.iter (fun (summary : Rule_enum.Algorithm.iter_summary) ->
    let n = summary.size in
    Printf.printf "=== Size %d  (enumerated %d) ===\n" n summary.enumerated;

    let nsr = List.length summary.new_size_rules in
    let nkr = List.length summary.new_kbo_rules in
    let nir = List.length summary.new_irreducibles in

    if nsr > 0 then begin
      Printf.printf "  New size-reducing rules: %d\n" nsr;
      ()
      (* List.iter (fun (l, r) ->
        Printf.printf "    %s  ->  %s\n"
          (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
      ) summary.new_size_rules *)
    end else
      Printf.printf "  New size-reducing rules: 0\n";

    if nkr > 0 then begin
      Printf.printf "  New KBO-simplifying rules: %d\n" nkr;
      ()
      (* List.iter (fun (l, r) ->
        Printf.printf "    %s  ->  %s\n"
          (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
      ) summary.new_kbo_rules *)
    end else
      Printf.printf "  New KBO-simplifying rules: 0\n";

    if nir > 0 then begin
      Printf.printf "  New irreducible terms: %d\n" nir;
      (* List.iter (fun t ->
        Printf.printf "    %s\n" (Rule_enum.Types.to_string t)
      ) summary.new_irreducibles *)
       ()
    end else
      Printf.printf "  New irreducible terms: 0\n";

    Printf.printf "  Cumulative: size-reducing=%d  kbo=%d  irreducible=%d\n%!"
      (List.length rs.size_rules)
      (List.length rs.kbo_rules)
      (List.length rs.irreducible)
  ) iterations;

  Printf.printf "\n=== Final totals ===\n";
  Printf.printf "Size-reducing rules: %d\n" (List.length rs.size_rules);
  Printf.printf "KBO-simplifying rules: %d\n" (List.length rs.kbo_rules);
  Printf.printf "Irreducible terms: %d\n" (List.length rs.irreducible)
