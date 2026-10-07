open Rule_enum

let domain = ref "int"
let max_vcs = ref 3
let max_vars = ref (-1)
let max_holes = ref (-1)
let max_size = ref 7
let random_inputs = ref 100
let full = ref false
let seed = ref 0
let smt = ref false
let ac_mode = ref false
let smt_timeout = ref 1000
let bv_width = ref 32
let stats_file = ref ""
let output_file = ref ""
let rule_output = ref ""
let irred_output = ref ""
let eval_mode = ref false
let rules_input = ref ""
let terms_input = ref ""
let lower_vars = ref false

let spec =
  Arg.align
    [ ("--domain", Arg.Set_string domain, " bool|int|bv|demo (default int)");
      ("--max-vcs", Arg.Set_int max_vcs, "K bound on distinct variables plus constants (default 3)");
      ("--max-vars", Arg.Set_int max_vars, "N bound on distinct variables (default K)");
      ("--max-holes", Arg.Set_int max_holes, "N bound on distinct uninterpreted constants (default K)");
      ("--max-size", Arg.Set_int max_size, "N largest term size (default 7)");
      ("--random-inputs", Arg.Set_int random_inputs, "N random inputs when not exhaustive (default 100)");
      ("--full", Arg.Set full, " require exhaustive test inputs");
      ("--seed", Arg.Set_int seed, "N random seed (default 0)");
      ("--smt", Arg.Set smt, " prove equivalences with z3 where not decided exhaustively");
      ("--ac", Arg.Set ac_mode, " rewrite modulo associativity and commutativity of the AC symbols");
      ("--smt-timeout", Arg.Set_int smt_timeout, "MS solver timeout per query (default 1000)");
      ("--bv-width", Arg.Set_int bv_width, "W bit width of the bv domain (default 32)");
      ("--stats", Arg.Set_string stats_file, "FILE per-size CSV statistics");
      ("--output", Arg.Set_string output_file, "FILE report (synthesis) or normal forms (--eval)");
      ("--rule-output", Arg.Set_string rule_output, "FILE synthesized rules");
      ("--irred-output", Arg.Set_string irred_output, "FILE normal forms");
      ("--eval", Arg.Set eval_mode, " normalize --terms-input with --rules-input");
      ("--rules-input", Arg.Set_string rules_input, "FILE rules for --eval");
      ("--terms-input", Arg.Set_string terms_input, "FILE terms for --eval, one per line");
      ("--lower-vars", Arg.Set lower_vars, " variables lowercase, constants uppercase (default: the reverse, as in the paper)") ]

let open_optional path = if path = "" then None else Some (open_out path)

let counts rules =
  let sr = List.length (List.filter Synth.shrinking rules) in
  (sr, List.length rules - sr)

module Run (T : Theory.THEORY) = struct
  module Sem = Semantics.Make (T)
  module Syn = Synth.Make (T)

  let show = Syntax.to_string ~lower_vars:!lower_vars T.name
  let show_rule = Syntax.rule_to_string ~lower_vars:!lower_vars T.name
  let unknown = ref 0

  let prover () =
    match T.smt with
    | Some encoding when !smt ->
        let prove = Smt.prover ~timeout_ms:!smt_timeout encoding in
        Some
          (fun s t ->
            match prove s t with
            | Smt.Equivalent -> true
            | Smt.Different -> false
            | Smt.Unknown -> incr unknown; false)
    | None when !smt -> failwith "domain has no SMT encoding"
    | _ -> None

  let oracle (caps : Enumerate.caps) =
    let rng = Random.State.make [| !seed |] in
    let inputs, complete =
      Sem.inputs ~force_exhaustive:!full rng ~samples:!random_inputs ~vars:caps.vars ~ucs:caps.ucs
    in
    let prove = prover () in
    if (not complete) && Option.is_none prove && !random_inputs = 0 then
      failwith "need --random-inputs, --full or --smt";
    Printf.printf
      "Domain: %s,  max VCs (k): %d,  max vars: %d,  max holes: %d,  inputs: %d (%s),  max size: %d,  smt: %b,  ac: %b\n\n%!"
      !domain caps.leaves caps.vars caps.ucs (Array.length inputs)
      (if complete then "exhaustive" else "random") !max_size !smt !ac_mode;
    Sem.oracle ?prove (inputs, complete)

  let totals st =
    let sr, kr = counts st.Synth.rules in
    (sr, kr, List.length (Synth.normal_forms st))

  let log_step start st (s : T.sym Synth.step) =
    let (sr, kr), (tsr, tkr, tir) = (counts s.new_rules, totals st) in
    Printf.printf "Size %d  [%.1fs / %.1fs]  enum=%d  +SR=%d  +KR=%d  +IR=%d  total: SR=%d KR=%d IR=%d\n%!"
      s.size (Unix.gettimeofday () -. start) s.seconds s.enumerated sr kr
      (List.length s.new_normal_forms) tsr tkr tir

  let csv_header =
    "size,enumerated,new_size_rules,new_kbo_rules,new_irreducibles,total_size_rules,total_kbo_rules,\
     total_irreducible,time_total\n"

  let csv_row oc st (s : T.sym Synth.step) =
    let (sr, kr), (tsr, tkr, tir) = (counts s.new_rules, totals st) in
    Printf.fprintf oc "%d,%d,%d,%d,%d,%d,%d,%d,%.4f\n%!" s.size s.enumerated sr kr
      (List.length s.new_normal_forms) tsr tkr tir s.seconds

  let report oc (s : T.sym Synth.step) =
    let section title items =
      if items <> [] then (
        Printf.fprintf oc "  %s: %d\n" title (List.length items);
        List.iter (Printf.fprintf oc "    %s\n") items)
    in
    let shrinking, same_size = List.partition Synth.shrinking s.new_rules in
    Printf.fprintf oc "--- Size %d (enumerated %d, candidates %d, %.3fs) ---\n" s.size s.enumerated
      s.candidates s.seconds;
    section "New irreducible" (List.map show s.new_normal_forms);
    section "Size-reducing rules" (List.map show_rule shrinking);
    section "KBO-simplifying rules" (List.map show_rule same_size);
    Printf.fprintf oc "\n%!"

  let snapshot st =
    if !rule_output <> "" then Syntax.write_lines !rule_output (List.map show_rule (Synth.rules st));
    if !irred_output <> "" then Syntax.write_lines !irred_output (List.map show (Synth.normal_forms st))

  let summary start st =
    let sr, kr, ir = totals st in
    Printf.printf "\nFinal [%.1fs]: SR=%d  KR=%d  IR=%d\n" (Unix.gettimeofday () -. start) sr kr ir;
    if Syn.exhausted st (!max_size + 1) then
      print_endline
        "Saturated: no larger left-hand sides exist; the rules are complete for ground terms of every size.";
    if !unknown > 0 then Printf.printf "SMT returned unknown %d times (treated as not equivalent).\n" !unknown

  let synthesize () =
    let pick n = if n < 0 then !max_vcs else n in
    let caps = { Enumerate.vars = pick !max_vars; ucs = pick !max_holes; leaves = !max_vcs } in
    let oracle = oracle caps and start = Unix.gettimeofday () in
    let csv = open_optional !stats_file and txt = open_optional !output_file in
    Option.iter (fun oc -> output_string oc csv_header) csv;
    let on_step st s =
      log_step start st s;
      Option.iter (fun oc -> csv_row oc st s) csv;
      Option.iter (fun oc -> report oc s) txt;
      snapshot st
    in
    summary start (Syn.run ~on_step caps ~max_size:!max_size oracle);
    List.iter (Option.iter close_out) [ csv; txt ]

  (* Terms to simplify are ground: their letters denote uninterpreted constants. *)
  let ground t =
    let offset = 1 + List.fold_left max (-1) (Term.ucs t) in
    Term.map_leaves (function Term.Var i -> Term.Uc (i + offset) | l -> l) t

  let normalize () =
    let parse = Syntax.parse ~lower_vars:!lower_vars ~symbols:T.symbols ~name:T.name in
    let rules, irregular =
      Syntax.read_lines !rules_input
      |> List.map (Syntax.parse_rule ~lower_vars:!lower_vars ~symbols:T.symbols ~name:T.name)
      |> List.partition (fun (l, r) -> Term.regular ~lhs:l r)
    in
    if irregular <> [] then
      Printf.eprintf "Skipping %d rules with right-hand side leaves not in the left-hand side, e.g. %s\n%!"
        (List.length irregular) (show_rule (List.hd irregular));
    let terms = List.map parse (Syntax.read_lines !terms_input) in
    Printf.printf "Loaded %d rules and %d terms (domain %s)\n%!" (List.length rules) (List.length terms) !domain;
    let index = Rewrite.of_list Syn.syms rules in
    let start = Unix.gettimeofday () in
    let normal = List.map (fun t -> Rewrite.normalize Syn.syms index (ground t)) terms in
    Printf.printf "Normalized %d terms in %.3fs\n%!" (List.length terms) (Unix.gettimeofday () -. start);
    let normal = List.map show normal in
    if !output_file = "" then List.iter print_endline normal else Syntax.write_lines !output_file normal

  let main () = if !eval_mode then normalize () else synthesize ()
end

let () =
  Arg.parse spec (fun a -> raise (Arg.Bad ("unexpected argument " ^ a))) "Usage: rule_enum [options]";
  if !eval_mode && (!rules_input = "" || !terms_input = "") then (
    prerr_endline "--eval requires --rules-input and --terms-input";
    exit 2);
  let run (module T : Theory.THEORY) =
    let module R = Run (struct include T let ac f = !ac_mode && T.ac f end) in
    R.main ()
  in
  match !domain with
  | "bool" -> run (module Boolean)
  | "bool_min" -> run (module Bool_min)
  | "int" -> run (module Integer)
  | "demo" -> run (module Demo)
  | "bv" -> run (module Bitvector.Make (struct let width = max 1 (min 62 !bv_width) end))
  | d ->
      prerr_endline ("unknown domain " ^ d);
      exit 2
