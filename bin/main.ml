let () =
  Random.self_init ();
  let dom = Rule_enum.Domain_bool.bool_domain in
  let max_vars = 3 in

  Printf.printf "Domain: bool,  max vars: %d\n\n%!" max_vars;

  let rule_filename = Printf.sprintf "rules_bool_vars%d.txt" max_vars in
  let rule_oc = open_out rule_filename in

  let _rs, _iterations =
    Rule_enum.Algorithm.run ~max_size:100 dom
      ~num_random_inputs:0 ~max_vars
      ~forced_inputs:(Rule_enum.Domain_bool.all_inputs max_vars)
      ~on_iteration:(fun (s : Rule_enum.Algorithm.iter_summary) ->
        let n = s.size in
        let nsr = List.length s.new_size_rules in
        let nkr = List.length s.new_kbo_rules in
        let nir = List.length s.new_irreducibles in

        if n > 1 then Printf.fprintf rule_oc "\n\n";
        Printf.fprintf rule_oc "Iteration %d: enumerated %d\n" n s.enumerated;
        Printf.fprintf rule_oc "Size-Reducing: %d\n" nsr;
        List.iter (fun (l, r) ->
          Printf.fprintf rule_oc "  %s  ->  %s\n"
            (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
        ) s.new_size_rules;
        Printf.fprintf rule_oc "KBO-Simplifying: %d\n" nkr;
        List.iter (fun (l, r) ->
          Printf.fprintf rule_oc "  %s  ->  %s\n"
            (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
        ) s.new_kbo_rules;
        Printf.fprintf rule_oc "Irreducibles: %d\n" nir;
        List.iter (fun t ->
          Printf.fprintf rule_oc "  %s\n" (Rule_enum.Types.to_string t)
        ) s.new_irreducibles;
        
        Printf.printf "=== Size %d  (enumerated %d) ===\n" n s.enumerated;
        Printf.printf "  New size-reducing rules: %d\n" nsr;
        (* List.iter (fun (l, r) ->
          Printf.printf "    %s  ->  %s\n"
            (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
        ) s.new_size_rules; *)
        Printf.printf "  New KBO-simplifying rules: %d\n" nkr;
        (* List.iter (fun (l, r) ->
          Printf.printf "    %s  ->  %s\n"
            (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
        ) s.new_kbo_rules; *)
        Printf.printf "  New irreducible terms: %d\n" nir;
        (* List.iter (fun t ->
          Printf.printf "    %s\n" (Rule_enum.Types.to_string t)
        ) s.new_irreducibles; *)
        Printf.printf "  Cumulative: size-reducing=%d  kbo=%d  irreducible=%d\n%!"
          s.total_size_rules s.total_kbo_rules s.total_irreducible
      )
  in

  Printf.printf "\n=== Final totals ===\n";
  Printf.printf "Size-reducing rules: %d\n" (List.length _rs.size_rules);
  Printf.printf "KBO-simplifying rules: %d\n" (List.length _rs.kbo_rules);
  Printf.printf "Irreducible terms: %d\n" (List.length _rs.irreducible);

  (* print iterations as csv *)
  let output_file = Printf.sprintf "bool_vars%d.csv" max_vars in
  let oc = open_out output_file in
  Printf.fprintf oc "size,enumerated,new_size_rules,new_kbo_rules,new_irreducibles,total_size_rules,total_kbo_rules,total_irreducible\n";
  List.iter (fun (s : Rule_enum.Algorithm.iter_summary) ->
    Printf.fprintf oc "%d,%d,%d,%d,%d,%d,%d,%d\n"
      s.size s.enumerated 
      (List.length s.new_size_rules) 
      (List.length s.new_kbo_rules)
      (List.length s.new_irreducibles) 
      s.total_size_rules 
      s.total_kbo_rules 
      s.total_irreducible
  ) _iterations;
  close_out oc;
  Printf.printf "Iteration data written to %s\n" output_file
