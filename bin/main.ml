let run_with (type a) (dom : a Rule_enum.Domain.t) forced num_rand
      ~max_size ~max_vars ~domain_name ~output_file ~stats_file ~num_domains =
  Printf.printf "Domain: %s,  max vars: %d,  random inputs: %d,  max size: %d,  jobs: %d\n\n%!"
    domain_name max_vars num_rand max_size num_domains;

  let rs, iterations =
    Rule_enum.Algorithm.run ~max_size dom
      ~num_random_inputs:num_rand ~max_vars
      ~forced_inputs:forced ~num_domains
      ~on_iteration:(fun (s : Rule_enum.Algorithm.iter_summary) ->
        Printf.printf "Size %d  enum=%d  +SR=%d  +KR=%d  +IR=%d  total: SR=%d KR=%d IR=%d\n%!"
          s.size s.enumerated
          (List.length s.new_size_rules)
          (List.length s.new_kbo_rules)
          (List.length s.new_irreducibles)
          s.total_size_rules s.total_kbo_rules s.total_irreducible)
  in

  Printf.printf "\nFinal: SR=%d  KR=%d  IR=%d\n%!"
    (List.length rs.size_rules) (List.length rs.kbo_rules) (List.length rs.irreducible);

  if stats_file <> "" then begin
    let oc = open_out stats_file in
    Printf.fprintf oc "size,enumerated,new_size_rules,new_kbo_rules,new_irreducibles,total_size_rules,total_kbo_rules,total_irreducible\n";
    List.iter (fun (s : Rule_enum.Algorithm.iter_summary) ->
      Printf.fprintf oc "%d,%d,%d,%d,%d,%d,%d,%d\n"
        s.size s.enumerated
        (List.length s.new_size_rules) (List.length s.new_kbo_rules)
        (List.length s.new_irreducibles)
        s.total_size_rules s.total_kbo_rules s.total_irreducible
    ) iterations;
    close_out oc;
    Printf.printf "Stats written to %s\n%!" stats_file
  end;

  if output_file <> "" then begin
    let oc = open_out output_file in
    Printf.fprintf oc "=== Rule Enumeration Results ===\n";
    Printf.fprintf oc "Domain: %s, max vars: %d, max size: %d\n\n"
      domain_name max_vars max_size;

    List.iter (fun (s : Rule_enum.Algorithm.iter_summary) ->
      Printf.fprintf oc "--- Size %d (enumerated %d) ---\n" s.size s.enumerated;
      Printf.fprintf oc "  New irreducible: %d\n"
        (List.length s.new_irreducibles);

      let nsr = List.length s.new_size_rules in
      let nkr = List.length s.new_kbo_rules in
      if nsr > 0 then begin
        Printf.fprintf oc "  Size-reducing rules: %d\n" nsr;
        List.iter (fun (l, r) ->
          Printf.fprintf oc "    %s  ->  %s\n"
            (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
        ) s.new_size_rules
      end;
      if nkr > 0 then begin
        Printf.fprintf oc "  KBO-simplifying rules: %d\n" nkr;
        List.iter (fun (l, r) ->
          Printf.fprintf oc "    %s  ->  %s\n"
            (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r)
        ) s.new_kbo_rules
      end;
      Printf.fprintf oc "  Cumulative: SR=%d KR=%d IR=%d\n\n"
        s.total_size_rules s.total_kbo_rules s.total_irreducible
    ) iterations;

    Printf.fprintf oc "=== Final totals ===\n";
    Printf.fprintf oc "Size-reducing: %d\n" (List.length rs.size_rules);
    Printf.fprintf oc "KBO-simplifying: %d\n" (List.length rs.kbo_rules);
    Printf.fprintf oc "Irreducible:   %d\n" (List.length rs.irreducible);
    close_out oc;
    Printf.printf "Full report written to %s\n%!" output_file
  end

let () =
  let domain_name = ref "int" in
  let max_vars = ref 3 in
  let random_inputs = ref 100 in
  let use_full = ref false in
  let max_size = ref 7 in
  let output_file = ref "" in
  let stats_file = ref "" in
  let jobs = ref 0 in

  let speclist = [
    ("--domain", Arg.Set_string domain_name,
     " int|bool  Evaluation domain (default: int)");
    ("--max-vars", Arg.Set_int max_vars,
     " N  Maximum distinct variables (default: 3)");
    ("--random-inputs", Arg.Set_int random_inputs,
     " N  Number of random inputs, 0 = none (default: 100)");
    ("--full", Arg.Set use_full,
     " Use exhaustive input enumeration (for bool: all 2^n combos)");
    ("--max-size", Arg.Set_int max_size,
     " N  Maximum term size to enumerate (default: 7)");
    ("--output", Arg.Set_string output_file,
     " FILE  Write all rules and stats to FILE");
    ("--stats", Arg.Set_string stats_file,
     " FILE  Write per-iteration stats as CSV to FILE");
    ("--jobs", Arg.Set_int jobs,
     " N  Number of parallel workers (0 = all cores, default: 0)");
  ] in
  let usage = "Usage: rule_enum [options]" in
  Arg.parse speclist (fun _ -> ()) usage;

  Random.self_init ();

  let num_rand = if !use_full then 0 else !random_inputs in

  match !domain_name with
  | "int" ->
    let dom = Rule_enum.Domain_int.int_domain in
    run_with dom [] num_rand
      ~max_size:!max_size ~max_vars:!max_vars ~domain_name:!domain_name
      ~output_file:!output_file ~stats_file:!stats_file
      ~num_domains:!jobs
  | "bool" ->
    let dom = Rule_enum.Domain_bool.bool_domain in
    let forced = if !use_full then Rule_enum.Domain_bool.all_inputs !max_vars else [] in
    run_with dom forced num_rand
      ~max_size:!max_size ~max_vars:!max_vars ~domain_name:!domain_name
      ~output_file:!output_file ~stats_file:!stats_file
      ~num_domains:!jobs
  | _ ->
    Printf.eprintf "Unknown domain: %s (use int or bool)\n" !domain_name;
    exit 1
