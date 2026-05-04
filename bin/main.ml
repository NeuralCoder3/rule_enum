let write_header oc_header oc_report domain_name max_vars max_size =
  (match oc_header with Some oc ->
     Printf.fprintf oc "size,enumerated,new_size_rules,new_kbo_rules,new_irreducibles,total_size_rules,total_kbo_rules,total_irreducible,time_total,time_enum,time_norm,time_eval,time_match,time_apply,time_group,time_pmap,time_post\n";
     flush oc | None -> ());
  (match oc_report with Some oc ->
     Printf.fprintf oc "=== Rule Enumeration Results ===\nDomain: %s, max vars: %d, max size: %d\n\n"
       domain_name max_vars max_size;
     flush oc | None -> ())

let write_iteration ?oc_header ?oc_report (s : Rule_enum.Algorithm.iter_summary) =
  let open Rule_enum.Algorithm in
  let nsr = List.length s.new_size_rules in
  let nkr = List.length s.new_kbo_rules in
  let nir = List.length s.new_irreducibles in
  (match oc_header with Some oc ->
     Printf.fprintf oc "%d,%d,%d,%d,%d,%d,%d,%d,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f\n"
       s.size s.enumerated nsr nkr nir
       s.total_size_rules s.total_kbo_rules s.total_irreducible
       s.time_total s.time_enum s.time_normalize s.time_eval
       s.time_match s.time_apply s.time_group s.time_pmap s.time_post;
     flush oc | None -> ());
  (match oc_report with Some oc ->
     Printf.fprintf oc "--- Size %d (enumerated %d, %.3fs) ---\n" s.size s.enumerated s.time_total;
     Printf.fprintf oc "  New irreducible: %d\n" nir;
     if nsr > 0 then begin
       Printf.fprintf oc "  Size-reducing rules: %d\n" nsr;
       List.iter (fun (l, r) ->
         Printf.fprintf oc "    %s  ->  %s\n" (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r))
         s.new_size_rules
     end;
     if nkr > 0 then begin
       Printf.fprintf oc "  KBO-simplifying rules: %d\n" nkr;
       List.iter (fun (l, r) ->
         Printf.fprintf oc "    %s  ->  %s\n" (Rule_enum.Types.to_string l) (Rule_enum.Types.to_string r))
         s.new_kbo_rules
     end;
     Printf.fprintf oc "  Cumulative: SR=%d KR=%d IR=%d\n\n"
       s.total_size_rules s.total_kbo_rules s.total_irreducible;
     flush oc | None -> ())

let write_footer oc_report total_elapsed (rs : _ Rule_enum.Algorithm.rule_sets) =
  match oc_report with Some oc ->
    Printf.fprintf oc "=== Final totals (%.1fs) ===\n" total_elapsed;
    Printf.fprintf oc "Size-reducing: %d\nKBO-simplifying: %d\nIrreducible:   %d\n"
      (List.length rs.size_rules) (List.length rs.kbo_rules) (List.length rs.behaviors);
    flush oc
  | None -> ()

let run_with (type a) (dom : a Rule_enum.Domain.t) forced num_rand
      ~max_size ~max_vars ~domain_name ~output_file ~stats_file ~num_domains =
  Printf.printf "Domain: %s,  max vars: %d,  random inputs: %d,  max size: %d,  jobs: %d\n\n%!"
    domain_name max_vars num_rand max_size num_domains;

  let start_time = Unix.gettimeofday () in
  let oc_stats = if stats_file <> "" then Some (open_out stats_file) else None in
  let oc_report = if output_file <> "" then Some (open_out output_file) else None in
  write_header oc_stats oc_report domain_name max_vars max_size;

  let rs, _iterations =
    Rule_enum.Algorithm.run ~max_size dom ~num_random_inputs:num_rand ~max_vars
      ~forced_inputs:forced ~num_domains
      ~on_iteration:(fun s ->
        let elapsed = Unix.gettimeofday () -. start_time in
        Printf.printf "Size %d  [%.1fs / %.1fs]  enum=%d  +SR=%d  +KR=%d  +IR=%d  total: SR=%d KR=%d IR=%d\n%!"
          s.size elapsed s.Rule_enum.Algorithm.time_total
          s.Rule_enum.Algorithm.enumerated
          (List.length s.Rule_enum.Algorithm.new_size_rules)
          (List.length s.Rule_enum.Algorithm.new_kbo_rules)
          (List.length s.Rule_enum.Algorithm.new_irreducibles)
          s.Rule_enum.Algorithm.total_size_rules
          s.Rule_enum.Algorithm.total_kbo_rules
          s.Rule_enum.Algorithm.total_irreducible;
        write_iteration ?oc_header:oc_stats ?oc_report s)
  in

  let total_elapsed = Unix.gettimeofday () -. start_time in
  Printf.printf "\nFinal [%.1fs]: SR=%d  KR=%d  IR=%d\n%!"
    total_elapsed (List.length rs.size_rules) (List.length rs.kbo_rules)
    (List.length rs.behaviors);

  write_footer oc_report total_elapsed rs;
  Option.iter close_out oc_report; Option.iter close_out oc_stats

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
    ("--domain", Arg.Set_string domain_name, " int|bool  Evaluation domain (default: int)");
    ("--max-vars", Arg.Set_int max_vars, " N  Maximum distinct variables (default: 3)");
    ("--random-inputs", Arg.Set_int random_inputs, " N  Random inputs, 0 = none (default: 100)");
    ("--full", Arg.Set use_full, " Exhaustive enumeration (bool: all 2^n combos)");
    ("--max-size", Arg.Set_int max_size, " N  Maximum term size (default: 7)");
    ("--output", Arg.Set_string output_file, " FILE  Write full report to FILE");
    ("--stats", Arg.Set_string stats_file, " FILE  Write per-iteration CSV to FILE");
    ("--jobs", Arg.Set_int jobs, " N  Parallel workers (0 = all cores, default: 0)");
  ] in
  Arg.parse speclist (fun _ -> ()) "Usage: rule_enum [options]";

  Random.self_init ();
  let num_rand = if !use_full then 0 else !random_inputs in

  match !domain_name with
  | "int" ->
    run_with Rule_enum.Domain_int.int_domain [] num_rand
      ~max_size:!max_size ~max_vars:!max_vars ~domain_name:!domain_name
      ~output_file:!output_file ~stats_file:!stats_file ~num_domains:!jobs
  | "bool" ->
    let dom = Rule_enum.Domain_bool.bool_domain in
    let forced = if !use_full then Rule_enum.Domain_bool.all_inputs !max_vars else [] in
    run_with dom forced num_rand
      ~max_size:!max_size ~max_vars:!max_vars ~domain_name:!domain_name
      ~output_file:!output_file ~stats_file:!stats_file ~num_domains:!jobs
  | _ ->
    Printf.eprintf "Unknown domain: %s (use int or bool)\n" !domain_name;
    exit 1
