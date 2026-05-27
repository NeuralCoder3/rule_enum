(* Measures the cost of Rewrite.normalize on the actual workload from a real
   iteration. Prints how much wall time normalize would take if called on
   every enumerated term, separately from how long full process_term takes
   on the same input. *)

open Rule_enum

let bench_size n =
  let dom = Domain_int.int_domain in
  let max_vcs = 3 in
  let sym_cmp = dom.Domain.sym_compare in
  let inputs = Eval.generate_inputs dom 100 max_vcs in
  (* Run the algorithm up to size (n-1) to populate rules. *)
  let prev = if n > 1 then n - 1 else 1 in
  let rs, _ = Algorithm.run ~max_size:prev dom ~num_random_inputs:0
    ~forced_inputs:inputs ~max_vcs in
  let irrs = Algorithm.irreducibles rs in
  let rules = Algorithm.all_rules rs in
  let norm_index = Rewrite.index_rules rules in
  Printf.printf "Size %d: %d irreducibles, %d rules\n" n (List.length irrs) (List.length rules);
  let enumerated = Enum.enumerate_terms dom.Domain.all_symbols irrs n max_vcs in
  Printf.printf "  enumerated %d terms\n" (List.length enumerated);
  let t0 = Unix.gettimeofday () in
  let _ = List.map (fun t -> Rewrite.normalize ~sym_cmp ~index:norm_index t) enumerated in
  let t_norm = Unix.gettimeofday () -. t0 in
  let t1 = Unix.gettimeofday () in
  let _ = List.map (fun t ->
    let simplified, _ = Rewrite.normalize ~sym_cmp ~index:norm_index t in
    Eval.behavior dom inputs simplified) enumerated in
  let t_full = Unix.gettimeofday () -. t1 in
  Printf.printf "  normalize:   %.4fs  (%.2f µs/term)\n"
    t_norm (1e6 *. t_norm /. float_of_int (List.length enumerated));
  Printf.printf "  norm + eval: %.4fs  (%.2f µs/term)\n"
    t_full (1e6 *. t_full /. float_of_int (List.length enumerated));
  Printf.printf "  rewrite ratio: %.0f%% of (norm+eval)\n"
    (100. *. t_norm /. t_full)

let () =
  Random.init 42;
  List.iter bench_size [6; 7; 8]
