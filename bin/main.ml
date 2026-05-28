let write_header oc_header oc_report domain_name max_vcs max_size =
  (match oc_header with Some oc ->
     Printf.fprintf oc "size,enumerated,new_size_rules,new_kbo_rules,new_irreducibles,total_size_rules,total_kbo_rules,total_irreducible,time_total,time_enum,time_process,time_apply,time_group\n";
     flush oc | None -> ());
  (match oc_report with Some oc ->
     Printf.fprintf oc "=== Rule Enumeration Results ===\nDomain: %s, max VCs (k): %d, max size: %d\n\n"
       domain_name max_vcs max_size; flush oc | None -> ())

let write_iteration ?oc_header ?oc_report sym_str (s : 's Rule_enum.Algorithm.iter_summary) =
  let open Rule_enum.Algorithm in
  let nsr = List.length s.new_size_rules in let nkr = List.length s.new_kbo_rules in
  let nir = List.length s.new_irreducibles in
  (match oc_header with Some oc ->
     Printf.fprintf oc "%d,%d,%d,%d,%d,%d,%d,%d,%.4f,%.4f,%.4f,%.4f,%.4f\n"
       s.size s.enumerated nsr nkr nir s.total_size_rules s.total_kbo_rules s.total_irreducible
       s.time_total s.time_enum s.time_process s.time_apply s.time_group;
     flush oc | None -> ());
  (match oc_report with Some oc ->
     Printf.fprintf oc "--- Size %d (enumerated %d, %.3fs) ---\n" s.size s.enumerated s.time_total;
     if nir > 0 then begin
      Printf.fprintf oc "  New irreducible: %d\n" nir;
        List.iter (fun t -> Printf.fprintf oc "    %s\n" (Rule_enum.Types.to_string sym_str t)) s.new_irreducibles
     end;
     if nsr > 0 then begin Printf.fprintf oc "  Size-reducing rules: %d\n" nsr;
       List.iter (fun (l, r) -> Printf.fprintf oc "    %s  ->  %s\n"
         (Rule_enum.Types.to_string sym_str l) (Rule_enum.Types.to_string sym_str r)) s.new_size_rules end;
     if nkr > 0 then begin Printf.fprintf oc "  KBO-simplifying rules: %d\n" nkr;
       List.iter (fun (l, r) -> Printf.fprintf oc "    %s  ->  %s\n"
         (Rule_enum.Types.to_string sym_str l) (Rule_enum.Types.to_string sym_str r)) s.new_kbo_rules end;
     Printf.fprintf oc "  Cumulative: SR=%d KR=%d IR=%d\n\n" s.total_size_rules s.total_kbo_rules s.total_irreducible;
     flush oc | None -> ())

let write_footer oc_report sym_str total_elapsed (rs : _) =
  match oc_report with Some oc ->
    Printf.fprintf oc "=== Final totals (%.1fs) ===\n" total_elapsed;
    Printf.fprintf oc "Size-reducing: %d\nKBO-simplifying: %d\nIrreducible:   %d\n"
      (List.length rs.Rule_enum.Algorithm.size_rules) (List.length rs.Rule_enum.Algorithm.kbo_rules)
      (List.length rs.Rule_enum.Algorithm.behaviors);
    Printf.fprintf oc "\n=== Irreducible terms (by size) ===\n";
    let sorted = List.map (fun (t, _, _) -> t) rs.Rule_enum.Algorithm.behaviors
                 |> List.sort (fun a b -> compare (Rule_enum.Types.size a) (Rule_enum.Types.size b))
    in List.iter (fun t -> Printf.fprintf oc "  [size %d] %s\n" (Rule_enum.Types.size t)
      (Rule_enum.Types.to_string sym_str t)) sorted; flush oc
  | None -> ()

let run_with (type s) (dom : (s, 'a) Rule_enum.Domain.t) forced num_rand
      ~max_size ~max_vcs ~max_vars ~max_holes ~num_domains ~domain_name
      ~output_file ~stats_file ~rule_output ~irred_output
      ~use_smt ~use_smt_forced =
  let sym_str = dom.Rule_enum.Domain.sym_to_string in
  let effective_jobs =
    Rule_enum.Algorithm.effective_num_workers (Some num_domains) in
  let jobs_str =
    if effective_jobs <= 1 then "1 (single-threaded)"
    else string_of_int effective_jobs in
  Printf.printf "Domain: %s,  max VCs (k): %d,  max vars: %d,  max holes: %d,  random inputs: %d,  max size: %d,  jobs: %s,  smt: %b,  smt-forced: %b\n\n%!"
    domain_name max_vcs max_vars max_holes num_rand max_size jobs_str use_smt use_smt_forced;
  let start_time = Unix.gettimeofday () in
  let oc_stats = if stats_file <> "" then Some (open_out stats_file) else None in
  let oc_report = if output_file <> "" then Some (open_out output_file) else None in
  write_header oc_stats oc_report domain_name max_vcs max_size;
  let rs, _iters =
    Rule_enum.Algorithm.run ~max_size dom ~num_random_inputs:num_rand
      ~max_vcs ~max_vars ~max_holes ~num_domains:effective_jobs
      ~forced_inputs:forced ~use_smt ~use_smt_forced
      ~on_iteration:(fun s ->
        let elapsed = Unix.gettimeofday () -. start_time in
        Printf.printf "Size %d  [%.1fs / %.1fs]  enum=%d  +SR=%d  +KR=%d  +IR=%d  total: SR=%d KR=%d IR=%d\n%!"
          s.Rule_enum.Algorithm.size elapsed s.Rule_enum.Algorithm.time_total
          s.Rule_enum.Algorithm.enumerated
          (List.length s.Rule_enum.Algorithm.new_size_rules)
          (List.length s.Rule_enum.Algorithm.new_kbo_rules)
          (List.length s.Rule_enum.Algorithm.new_irreducibles)
          s.Rule_enum.Algorithm.total_size_rules s.Rule_enum.Algorithm.total_kbo_rules
          s.Rule_enum.Algorithm.total_irreducible;
        write_iteration ?oc_header:oc_stats ?oc_report sym_str s) in
  let total_elapsed = Unix.gettimeofday () -. start_time in
  Printf.printf "\nFinal [%.1fs]: SR=%d  KR=%d  IR=%d\n%!"
    total_elapsed (List.length rs.size_rules) (List.length rs.kbo_rules) (List.length rs.behaviors);
  if try Sys.getenv "RULE_ENUM_PROFILE" = "1" with Not_found -> false then
    Printf.eprintf
      "Tier stats: total=%d  tier2_short_circuit=%d  tier3_smt=%d  tier3_cex=%d\n%!"
      !Rule_enum.Algorithm.tier_calls
      !Rule_enum.Algorithm.tier2_short_circuit
      !Rule_enum.Algorithm.tier3_calls
      !Rule_enum.Algorithm.tier3_cex_added;
  write_footer oc_report sym_str total_elapsed rs;
  Option.iter close_out oc_report; Option.iter close_out oc_stats;
  (* Save rules / irreducibles in a parseable format if requested. *)
  if rule_output <> "" then begin
    let all_rules = rs.Rule_enum.Algorithm.size_rules
                  @ rs.Rule_enum.Algorithm.kbo_rules in
    Rule_enum.Parse.save_rules sym_str rule_output all_rules;
    Printf.printf "Saved %d rules to %s\n%!" (List.length all_rules) rule_output
  end;
  if irred_output <> "" then begin
    let irrs = List.map (fun (t, _, _) -> t) rs.Rule_enum.Algorithm.behaviors in
    Rule_enum.Parse.save_terms sym_str irred_output irrs;
    Printf.printf "Saved %d irreducibles to %s\n%!" (List.length irrs) irred_output
  end

(* Eval mode: load a rule set from disk, read input terms one per line,
   normalize each with the loaded rule set, write the normalized forms
   one per line. The output is in the same order as the input. *)
let eval_mode ~domain_name ~rules_input ~terms_input ~output_file =
  let module RE = Rule_enum in
  let load (type s) (dom : (s, _) RE.Domain.t) =
    let sym_str = dom.RE.Domain.sym_to_string in
    let sym_cmp = dom.RE.Domain.sym_compare in
    let decode = RE.Parse.decoder_of_symbols dom.RE.Domain.all_symbols in
    let rules = RE.Parse.load_rules decode rules_input in
    let terms = RE.Parse.load_terms decode terms_input in
    Printf.printf "Loaded %d rules and %d terms (domain %s)\n%!"
      (List.length rules) (List.length terms) domain_name;
    (* Use `norm_bottom` (no final canonicalize) so the normalized output
       preserves the user's hole identities. The default `normalize` does
       alpha-renaming of hole ids at the end, which is appropriate for
       synthesis but wrong for user input — it would map a result like
       `(-B)` (correct semantic answer) to `(-A)` (different meaning). *)
    let index = RE.Rewrite.index_rules rules in
    let normalize t = RE.Rewrite.norm_bottom ~sym_cmp ~index t in
    let out =
      if output_file = "" then stdout
      else open_out output_file
    in
    List.iter (fun t ->
      let n = normalize t in
      Printf.fprintf out "%s\n" (RE.Types.to_string sym_str n)) terms;
    if output_file <> "" then close_out out;
    Printf.printf "Normalized %d terms%s\n%!" (List.length terms)
      (if output_file <> "" then " to " ^ output_file else "")
  in
  match domain_name with
  | "int" -> load RE.Domain_int.int_domain
  | "bool" -> load RE.Domain_bool.bool_domain
  | _ -> Printf.eprintf "Unknown domain: %s (use int or bool)\n" domain_name; exit 1

let () =
  let domain_name = ref "int" in
  let max_vcs = ref 3 in
  (* -1 means "follow max_vcs" for max_vars; max_holes defaults to 0
     because we enumerate var-only and synthesize constP rules post-hoc
     from var-form equivalences (much smaller enumeration). Set --max-holes
     to a positive value to enumerate hole-leaf terms too. *)
  let max_vars = ref (-1) in
  let max_holes = ref 0 in
  let random_inputs = ref 100 in
  let use_full = ref false in let max_size = ref 7 in let output_file = ref "" in
  let stats_file = ref "" in let use_smt = ref false in
  let use_smt_forced = ref false in
  let rule_output = ref "" in let irred_output = ref "" in
  let eval = ref false in
  let rules_input = ref "" in let terms_input = ref "" in
  (* 0 means auto-detect (RULE_ENUM_JOBS env var, else recommended-domain-count). *)
  let jobs = ref 0 in
  let speclist = [
    ("--domain", Arg.Set_string domain_name, " int|bool  Evaluation domain (default: int)");
    ("--max-vcs", Arg.Set_int max_vcs, " K  Sum bound: distinct vars + distinct holes (default: 3)");
    ("--max-vars", Arg.Set_int max_vars, " N  Distinct-var cap (default: same as --max-vcs)");
    ("--max-holes", Arg.Set_int max_holes, " N  Distinct-hole cap (default: same as --max-vcs); set 0 to disable hole rules");
    ("--max-consts", Arg.Set_int max_holes, " N  Alias for --max-holes");
    ("--random-inputs", Arg.Set_int random_inputs, " N  Random inputs, 0 = none (default: 100)");
    ("--full", Arg.Set use_full, " Exhaustive enumeration (bool: all 2^n combos)");
    ("--max-size", Arg.Set_int max_size, " N  Maximum term size (default: 7)");
    ("--jobs", Arg.Set_int jobs, " N  Parallel worker count (default: RULE_ENUM_JOBS or all cores; 1 = single-threaded)");
    ("--output", Arg.Set_string output_file, " FILE  Write full report to FILE (synth) or normalized terms (eval)");
    ("--stats", Arg.Set_string stats_file, " FILE  Write per-iteration CSV to FILE");
    ("--smt", Arg.Set use_smt, " Enable SMT refinement (requires z3 in PATH)");
    ("--smt-forced", Arg.Set use_smt_forced, " Add SMT counterexamples to input set");
    ("--rule-output", Arg.Set_string rule_output, " FILE  Save rules (one per line) in load-able format");
    ("--irred-output", Arg.Set_string irred_output, " FILE  Save irreducibles (one per line) in load-able format");
    ("--eval", Arg.Set eval, " Run in eval mode: normalize terms from --terms-input using --rules-input");
    ("--rules-input", Arg.Set_string rules_input, " FILE  Rule set to load (eval mode)");
    ("--terms-input", Arg.Set_string terms_input, " FILE  Terms to normalize, one per line (eval mode)");
  ] in Arg.parse speclist (fun _ -> ()) "Usage: rule_enum [options]";
  if !eval then begin
    if !rules_input = "" || !terms_input = "" then begin
      Printf.eprintf "--eval requires --rules-input FILE and --terms-input FILE\n";
      exit 1
    end;
    eval_mode ~domain_name:!domain_name
      ~rules_input:!rules_input ~terms_input:!terms_input
      ~output_file:!output_file;
    exit 0
  end;
  (match Sys.getenv_opt "RULE_ENUM_SEED" with
   | Some s -> Random.init (int_of_string s)
   | None -> Random.self_init ());
  let num_rand = if !use_full then 0 else !random_inputs in
  let mv = if !max_vars < 0 then !max_vcs else !max_vars in
  let mh = if !max_holes < 0 then !max_vcs else !max_holes in
  (* Sanity: with max_holes=0 (default), enumeration produces only
     var-only terms; constP rules are still emitted post-hoc from var
     equivalences when var-KBO is Incomparable. *)
  match !domain_name with
  | "int" -> run_with Rule_enum.Domain_int.int_domain [] num_rand
      ~max_size:!max_size ~max_vcs:!max_vcs ~max_vars:mv ~max_holes:mh
      ~num_domains:!jobs ~domain_name:"int"
      ~output_file:!output_file ~stats_file:!stats_file
      ~rule_output:!rule_output ~irred_output:!irred_output
      ~use_smt:!use_smt ~use_smt_forced:!use_smt_forced
  | "bool" ->
    let dom = Rule_enum.Domain_bool.bool_domain in
    let forced = if !use_full then Rule_enum.Domain_bool.all_inputs !max_vcs else [] in
    run_with dom forced num_rand
      ~max_size:!max_size ~max_vcs:!max_vcs ~max_vars:mv ~max_holes:mh
      ~num_domains:!jobs ~domain_name:"bool"
      ~output_file:!output_file ~stats_file:!stats_file
      ~rule_output:!rule_output ~irred_output:!irred_output
      ~use_smt:!use_smt ~use_smt_forced:!use_smt_forced
  | _ -> Printf.eprintf "Unknown domain: %s (use int or bool)\n" !domain_name; exit 1
