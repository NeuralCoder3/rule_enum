let write_header oc_header oc_report domain_name max_vcs max_size =
  (match oc_header with Some oc ->
     Printf.fprintf oc "size,enumerated,new_size_rules,new_kbo_rules,new_irreducibles,total_size_rules,total_kbo_rules,total_irreducible,time_total,time_enum,time_process,time_apply,time_group\n";
     flush oc | None -> ());
  (match oc_report with Some oc ->
     Printf.fprintf oc "=== Rule Enumeration Results ===\nDomain: %s, max VCs (k): %d, max size: %d\n\n"
       domain_name max_vcs max_size; flush oc | None -> ())

let write_iteration ?oc_header ?oc_report to_str (s : 's Rule_enum.Algorithm.iter_summary) =
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
        List.iter (fun t -> Printf.fprintf oc "    %s\n" (to_str t)) s.new_irreducibles
     end;
     if nsr > 0 then begin Printf.fprintf oc "  Size-reducing rules: %d\n" nsr;
       List.iter (fun (l, r) -> Printf.fprintf oc "    %s  ->  %s\n"
         (to_str l) (to_str r)) s.new_size_rules end;
     if nkr > 0 then begin Printf.fprintf oc "  KBO-simplifying rules: %d\n" nkr;
       List.iter (fun (l, r) -> Printf.fprintf oc "    %s  ->  %s\n"
         (to_str l) (to_str r)) s.new_kbo_rules end;
     (* Unproven equivalences (SMT Unknown, passed random) decided this
        iteration: assumed = added on random confidence; skipped = declined
        under safe mode. *)
     if s.new_assumed <> [] then begin
       Printf.fprintf oc "  Assumed (unproven, added): %d\n" (List.length s.new_assumed);
       List.iter (fun (l, r) -> Printf.fprintf oc "    %s  ->  %s\n"
         (to_str l) (to_str r)) s.new_assumed end;
     if s.new_skipped <> [] then begin
       Printf.fprintf oc "  Skipped (unproven, not added): %d\n" (List.length s.new_skipped);
       List.iter (fun (l, r) -> Printf.fprintf oc "    %s  ->  %s\n"
         (to_str l) (to_str r)) s.new_skipped end;
     Printf.fprintf oc "  Cumulative: SR=%d KR=%d IR=%d assumed=%d skipped=%d\n\n"
       s.total_size_rules s.total_kbo_rules s.total_irreducible
       s.total_assumed s.total_skipped;
     flush oc | None -> ())

let write_footer oc_report to_str total_elapsed (rs : _) =
  match oc_report with Some oc ->
    Printf.fprintf oc "=== Final totals (%.1fs) ===\n" total_elapsed;
    Printf.fprintf oc "Size-reducing: %d\nKBO-simplifying: %d\nIrreducible:   %d\n"
      (List.length rs.Rule_enum.Algorithm.size_rules) (List.length rs.Rule_enum.Algorithm.kbo_rules)
      (List.length rs.Rule_enum.Algorithm.behaviors);
    (* Rules emitted on assumed (random-confident but not SMT-proven)
       equivalence — SMT returned Unknown and random could not refute. *)
    let assumed = List.rev rs.Rule_enum.Algorithm.assumed_rules in
    Printf.fprintf oc "\n=== Assumed (unproven) equivalences: %d ===\n"
      (List.length assumed);
    if assumed <> [] then
      Printf.fprintf oc "(SMT could not prove these; accepted on random confidence. Re-run with --safe-mode to exclude them.)\n";
    List.iter (fun (l, r) -> Printf.fprintf oc "    %s  ->  %s\n"
      (to_str l) (to_str r)) assumed;
    (* Candidate equivalences skipped in safe mode — SMT returned Unknown,
       random could not refute, and we declined to assume them. *)
    let skipped = List.rev rs.Rule_enum.Algorithm.skipped_rules in
    Printf.fprintf oc "\n=== Skipped (unproven, not assumed) equivalences: %d ===\n"
      (List.length skipped);
    if skipped <> [] then
      Printf.fprintf oc "(SMT could not prove these and random could not refute them; not emitted under --safe-mode. Drop --safe-mode, raise --smt-unknown-inputs, or RULE_ENUM_SMT_TIMEOUT_MS to resolve.)\n";
    List.iter (fun (l, r) -> Printf.fprintf oc "    %s  ->  %s\n"
      (to_str l) (to_str r)) skipped;
    Printf.fprintf oc "\n=== Irreducible terms (by size) ===\n";
    let sorted = List.map (fun (t, _, _) -> t) rs.Rule_enum.Algorithm.behaviors
                 |> List.sort (fun a b -> compare (Rule_enum.Types.size a) (Rule_enum.Types.size b))
    in List.iter (fun t -> Printf.fprintf oc "  [size %d] %s\n" (Rule_enum.Types.size t)
      (to_str t)) sorted; flush oc
  | None -> ()

let run_with (type s) (dom : (s, 'a) Rule_enum.Domain.t) forced num_rand
      ~max_size ~max_vcs ~max_vars ~max_holes ~num_domains ~domain_name
      ~output_file ~stats_file ~rule_output ~irred_output
      ~use_smt ~use_smt_forced ~assume_unproven ~unknown_inputs ~info ~progress =
  let to_str = dom.Rule_enum.Domain.term_to_string in
  let effective_jobs =
    Rule_enum.Algorithm.effective_num_workers (Some num_domains) in
  let jobs_str =
    if effective_jobs <= 1 then "1 (single-threaded)"
    else string_of_int effective_jobs in
  Printf.printf "Domain: %s,  max VCs (k): %d,  max vars: %d,  max holes: %d,  random inputs: %d,  max size: %d,  jobs: %s,  smt: %b,  smt-forced: %b,  safe-mode: %b\n\n%!"
    domain_name max_vcs max_vars max_holes num_rand max_size jobs_str use_smt use_smt_forced (not assume_unproven);
  let start_time = Unix.gettimeofday () in
  let oc_stats = if stats_file <> "" then Some (open_out stats_file) else None in
  let oc_report = if output_file <> "" then Some (open_out output_file) else None in
  write_header oc_stats oc_report domain_name max_vcs max_size;
  (* Snapshot the loadable rule / irreducible files from the current
     state. Called after every iteration so a long (or crashing) run
     always leaves the latest complete results on disk. Rewrites the
     whole file because irreducibles can be replaced mid-run (D_replace),
     not just appended — an append-only log would keep stale entries. *)
  let write_outputs (rs : (s, 'a) Rule_enum.Algorithm.rule_sets) =
    if rule_output <> "" then
      Rule_enum.Parse.save_rules to_str rule_output
        (rs.Rule_enum.Algorithm.size_rules @ rs.Rule_enum.Algorithm.kbo_rules);
    if irred_output <> "" then
      Rule_enum.Parse.save_terms to_str irred_output
        (List.map (fun (t, _, _) -> t) rs.Rule_enum.Algorithm.behaviors)
  in
  let rs, _iters =
    Rule_enum.Algorithm.run ~max_size dom ~num_random_inputs:num_rand
      ~max_vcs ~max_vars ~max_holes ~num_domains:effective_jobs
      ~forced_inputs:forced ~use_smt ~use_smt_forced ~assume_unproven
      ?unknown_inputs ~progress
      ~on_iteration:(fun rs s ->
        let open Rule_enum.Algorithm in
        let elapsed = Unix.gettimeofday () -. start_time in
        Printf.printf "Size %d  [%.1fs / %.1fs]  enum=%d  +SR=%d  +KR=%d  +IR=%d  total: SR=%d KR=%d IR=%d\n%!"
          s.size elapsed s.time_total s.enumerated
          (List.length s.new_size_rules) (List.length s.new_kbo_rules)
          (List.length s.new_irreducibles)
          s.total_size_rules s.total_kbo_rules s.total_irreducible;
        (* Surface unproven-equivalence decisions as they happen, not just
           in the final report. *)
        let na = List.length s.new_assumed and nk = List.length s.new_skipped in
        if na > 0 then
          Printf.printf "         +%d assumed (unproven, passed random, added) — total %d\n%!"
            na s.total_assumed;
        if nk > 0 then
          Printf.printf "         +%d skipped (unproven, passed random, not added: --safe-mode) — total %d\n%!"
            nk s.total_skipped;
        if info then begin
          let i = s.info in
          Printf.printf
            "    [info] enum=%d (var=%d hole=%d)  reducible=%d (rewrote)  irreducible-candidates=%d (raw %d)  bv-groups=%d\n%!"
            s.enumerated i.i_var_only i.i_with_holes i.i_reducible
            i.i_candidates_dedup i.i_candidates_raw i.i_bv_groups;
          Printf.printf
            "    [info] decisions: size-rule=%d kbo-rule=%d replace=%d dup-of-existing=%d\n%!"
            i.i_size_decisions i.i_kbo_decisions i.i_replace i.i_dup_skip;
          (* Equivalence oracle: the exact exhaustive count is nonzero
             exactly when the domain is small enough to decide without Z3
             (bool, low-width bv); SMT stats appear only when --smt is on. *)
          Printf.printf
            "    [info] equiv-oracle: exhaustive(exact, Z3-free)=%d%s\n%!"
            i.i_exhaustive
            (if use_smt then
               Printf.sprintf
                 "  smt-calls=%d  tier2-short-circuit=%d  unknown=%d (refuted=%d, accepted/declined=%d)  counterexamples=%d"
                 i.i_smt_calls i.i_tier2_short i.i_tier3_unknown i.i_tier3_refuted
                 (i.i_tier3_unknown - i.i_tier3_refuted) i.i_tier3_cex
             else "")
        end;
        write_iteration ?oc_header:oc_stats ?oc_report to_str s;
        write_outputs rs) in
  let total_elapsed = Unix.gettimeofday () -. start_time in
  Printf.printf "\nFinal [%.1fs]: SR=%d  KR=%d  IR=%d\n%!"
    total_elapsed (List.length rs.size_rules) (List.length rs.kbo_rules) (List.length rs.behaviors);
  if try Sys.getenv "RULE_ENUM_PROFILE" = "1" with Not_found -> false then begin
    Printf.eprintf
      "Tier stats: total=%d  tier2_short_circuit=%d  exhaustive_exact=%d  tier3_smt=%d  tier3_cex=%d  tier3_unknown=%d (refuted=%d)\n%!"
      !Rule_enum.Algorithm.tier_calls
      !Rule_enum.Algorithm.tier2_short_circuit
      (Atomic.get Rule_enum.Algorithm.exhaustive_calls)
      !Rule_enum.Algorithm.tier3_calls
      !Rule_enum.Algorithm.tier3_cex_added
      !Rule_enum.Algorithm.tier3_unknown
      !Rule_enum.Algorithm.tier3_unknown_refuted;
    Printf.eprintf "Anon cache: total=%d  distinct=%d  hit-rate=%.1f%%\n%!"
      !Rule_enum.Algorithm.anon_total
      !Rule_enum.Algorithm.anon_distinct
      (100.0 *. (1.0 -. float_of_int !Rule_enum.Algorithm.anon_distinct
                       /. float_of_int (max 1 !Rule_enum.Algorithm.anon_total)));
    let h = !Rule_enum.Types.cons_hits
    and m = !Rule_enum.Types.cons_misses in
    Printf.eprintf "Term cons cache: hits=%d  misses=%d  hit-rate=%.1f%%  unique=%d\n%!"
      h m (100.0 *. float_of_int h /. float_of_int (max 1 (h + m))) m
  end;
  write_footer oc_report to_str total_elapsed rs;
  Option.iter close_out oc_report; Option.iter close_out oc_stats;
  (* Final snapshot guarantees the files exist even if no iteration ran;
     otherwise this just re-confirms the last per-iteration write. *)
  write_outputs rs;
  if rule_output <> "" then
    Printf.printf "Saved %d rules to %s\n%!"
      (List.length rs.Rule_enum.Algorithm.size_rules
       + List.length rs.Rule_enum.Algorithm.kbo_rules) rule_output;
  if irred_output <> "" then
    Printf.printf "Saved %d irreducibles to %s\n%!"
      (List.length rs.Rule_enum.Algorithm.behaviors) irred_output

(* Eval mode: load a rule set from disk, read input terms one per line,
   normalize each with the loaded rule set, write the normalized forms
   one per line. The output is in the same order as the input. *)
let eval_mode ~domain_name ~rules_input ~terms_input ~output_file =
  let module RE = Rule_enum in
  let load (type s) (dom : (s, _) RE.Domain.t) =
    let sym_cmp = dom.RE.Domain.sym_compare in
    let of_string = dom.RE.Domain.term_of_string in
    let rules = RE.Parse.load_rules of_string rules_input in
    let terms = RE.Parse.load_terms of_string terms_input in
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
      Printf.fprintf out "%s\n" (dom.RE.Domain.term_to_string n)) terms;
    if output_file <> "" then close_out out;
    Printf.printf "Normalized %d terms%s\n%!" (List.length terms)
      (if output_file <> "" then " to " ^ output_file else "")
  in
  match domain_name with
  | "int" -> load RE.Domain_int.int_domain
  | "bv" -> load RE.Domain_bv.bv_domain
  | "bool" -> load RE.Domain_bool.bool_domain
  | _ -> Printf.eprintf "Unknown domain: %s (use int, bv, or bool)\n" domain_name; exit 1

let () =
  let domain_name = ref "int" in
  let max_vcs = ref 3 in
  (* -1 means "follow max_vcs". Both max_vars and max_holes default to
     max_vcs so the CLI matches Algorithm.run's own default and produces a
     COMPLETE rule set. Setting --max-holes 0 enumerates var-only and
     synthesizes constP rules post-hoc from var-form equivalences — much
     cheaper, but INCOMPLETE: constP reassociation rules whose canonical
     LHS leads with the larger hole (e.g. (B*(A*B)) -> (A*(B*B))) are never
     produced, because vars_to_holes of a first-occurrence-canonical var
     term can only ever put the first-occurring hole at H0. *)
  let max_vars = ref (-1) in
  let max_holes = ref (-1) in
  let random_inputs = ref 100 in
  let use_full = ref false in let max_size = ref 7 in let output_file = ref "" in
  let stats_file = ref "" in let use_smt = ref false in
  let use_smt_forced = ref false in
  let safe_mode = ref false in
  let info = ref (try Sys.getenv "RULE_ENUM_INFO" = "1" with Not_found -> false) in
  let progress = ref false in
  (* -1 = unset → use the algorithm's default (RULE_ENUM_SMT_UNKNOWN_INPUTS). *)
  let smt_unknown_inputs = ref (-1) in
  let rule_output = ref "" in let irred_output = ref "" in
  let eval = ref false in
  let rules_input = ref "" in let terms_input = ref "" in
  (* 0 means auto-detect (RULE_ENUM_JOBS env var, else recommended-domain-count). *)
  let jobs = ref 0 in
  let speclist = [
    ("--domain", Arg.Set_string domain_name, " int|bv|bool  Evaluation domain (default: int)");
    ("--max-vcs", Arg.Set_int max_vcs, " K  Sum bound: distinct vars + distinct holes (default: 3)");
    ("--max-vars", Arg.Set_int max_vars, " N  Distinct-var cap (default: same as --max-vcs)");
    ("--max-holes", Arg.Set_int max_holes, " N  Distinct-hole cap (default: same as --max-vcs); set 0 for fast var-only enumeration (INCOMPLETE: omits some constP reassociation rules)");
    ("--max-consts", Arg.Set_int max_holes, " N  Alias for --max-holes");
    ("--random-inputs", Arg.Set_int random_inputs, " N  Random inputs, 0 = none (default: 100)");
    ("--full", Arg.Set use_full, " Exhaustive enumeration (bool: all 2^n combos)");
    ("--max-size", Arg.Set_int max_size, " N  Maximum term size (default: 7)");
    ("--jobs", Arg.Set_int jobs, " N  Parallel worker count, capped at 64 (default: RULE_ENUM_JOBS or all cores; 1 = single-threaded)");
    ("--output", Arg.Set_string output_file, " FILE  Write full report to FILE (synth) or normalized terms (eval)");
    ("--stats", Arg.Set_string stats_file, " FILE  Write per-iteration CSV to FILE");
    ("--smt", Arg.Set use_smt, " Enable SMT refinement (requires z3 in PATH)");
    ("--smt-forced", Arg.Set use_smt_forced, " Add SMT counterexamples to input set");
    ("--safe-mode", Arg.Set safe_mode, " Do not assume unproven equivalences: when SMT returns Unknown and random can't refute, keep terms distinct (no rule)");
    ("--info", Arg.Set info, " Print detailed per-iteration counts (reducible/skipped terms, decision breakdown, SMT/tier activity). Also via RULE_ENUM_INFO=1");
    ("--progress", Arg.Set progress, " Show a progress bar (on a TTY) during each iteration, by enumerated terms processed");
    ("--smt-unknown-inputs", Arg.Set_int smt_unknown_inputs, " N  Extra random inputs to test when SMT returns Unknown (default 1000)");
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
  let unknown_inputs = if !smt_unknown_inputs < 0 then None else Some !smt_unknown_inputs in
  if mh = 0 && not !eval then
    Printf.eprintf
      "warning: --max-holes 0 (var-only) yields an INCOMPLETE rule set; some constP reassociation rules (e.g. (B*(A*B)) -> (A*(B*B))) will be missing. Use --max-holes %d for a complete set.\n%!"
      !max_vcs;
  (* With max_holes>0 (the default) hole-leaf terms are enumerated, so
     constP terms appear directly and group by behavior (holes canonicalize
     by sorted-id), giving a complete set of reassociation/commutativity
     rules. With max_holes=0 enumeration is var-only and constP rules are
     synthesized post-hoc from var equivalences — faster but incomplete. *)
  match !domain_name with
  | "int" -> run_with Rule_enum.Domain_int.int_domain [] num_rand
      ~max_size:!max_size ~max_vcs:!max_vcs ~max_vars:mv ~max_holes:mh
      ~num_domains:!jobs ~domain_name:"int"
      ~output_file:!output_file ~stats_file:!stats_file
      ~rule_output:!rule_output ~irred_output:!irred_output
      ~use_smt:!use_smt ~use_smt_forced:!use_smt_forced
      ~assume_unproven:(not !safe_mode)
      ~unknown_inputs
      ~info:!info
      ~progress:!progress
  | "bv" -> run_with Rule_enum.Domain_bv.bv_domain [] num_rand
      ~max_size:!max_size ~max_vcs:!max_vcs ~max_vars:mv ~max_holes:mh
      ~num_domains:!jobs ~domain_name:"bv"
      ~output_file:!output_file ~stats_file:!stats_file
      ~rule_output:!rule_output ~irred_output:!irred_output
      ~use_smt:!use_smt ~use_smt_forced:!use_smt_forced
      ~assume_unproven:(not !safe_mode)
      ~unknown_inputs
      ~info:!info
      ~progress:!progress
  | "bool" ->
    let dom = Rule_enum.Domain_bool.bool_domain in
    let forced = if !use_full then Rule_enum.Domain_bool.all_inputs !max_vcs else [] in
    run_with dom forced num_rand
      ~max_size:!max_size ~max_vcs:!max_vcs ~max_vars:mv ~max_holes:mh
      ~num_domains:!jobs ~domain_name:"bool"
      ~output_file:!output_file ~stats_file:!stats_file
      ~rule_output:!rule_output ~irred_output:!irred_output
      ~use_smt:!use_smt ~use_smt_forced:!use_smt_forced
      ~assume_unproven:(not !safe_mode)
      ~unknown_inputs
      ~info:!info
      ~progress:!progress
  | _ -> Printf.eprintf "Unknown domain: %s (use int, bv, or bool)\n" !domain_name; exit 1
