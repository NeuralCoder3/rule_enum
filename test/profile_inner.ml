(* Detailed breakdown of normalize. *)

open Rule_enum

let dom = Domain_int.int_domain
let sym_cmp = dom.Domain.sym_compare

let () =
  Printf.printf "Building up to size 9...\n%!";
  let rs, _ = Algorithm.run ~max_size:9 dom
    ~num_random_inputs:100 ~max_vcs:3 ~max_vars:3 ~max_holes:3
    ~num_domains:1 in
  let irrs = Algorithm.irreducibles rs in
  let rules = Algorithm.all_rules rs in
  Printf.printf "  Got %d irrs, %d rules\n%!"
    (List.length irrs) (List.length rules);

  let enumerated = Enum.enumerate_terms_caps dom.Domain.all_symbols irrs 10
    { max_vars = 3; max_holes = 3; max_vcs = 3 } in
  Printf.printf "  Enumerated %d at size 10\n%!" (List.length enumerated);

  let norm_index = Rewrite.index_rules rules in

  (* Counter for what happens in normalize. *)
  let n_terms = ref 0 in
  let n_size_reduced = ref 0 in
  let n_no_change = ref 0 in
  let n_changed_no_size = ref 0 in
  let total_size_in = ref 0 in
  let total_size_out = ref 0 in
  let t = Sys.time () in
  List.iter (fun term ->
    incr n_terms;
    total_size_in := !total_size_in + Types.size term;
    let (s, reduced) = Rewrite.normalize_canonical ~sym_cmp ~index:norm_index term in
    total_size_out := !total_size_out + Types.size s;
    if reduced then incr n_size_reduced
    else if Types.term_eq sym_cmp s term then incr n_no_change
    else incr n_changed_no_size) enumerated;
  let dt = Sys.time () -. t in
  Printf.printf "\nNormalize stats:\n";
  Printf.printf "  Total: %d in %.3fs (%.2f µs/term)\n%!"
    !n_terms dt (1e6 *. dt /. float_of_int !n_terms);
  Printf.printf "  Size-reduced: %d (%.1f%%)\n"
    !n_size_reduced (100. *. float_of_int !n_size_reduced /. float_of_int !n_terms);
  Printf.printf "  No change: %d (%.1f%%)\n"
    !n_no_change (100. *. float_of_int !n_no_change /. float_of_int !n_terms);
  Printf.printf "  Changed (same size): %d (%.1f%%)\n"
    !n_changed_no_size (100. *. float_of_int !n_changed_no_size /. float_of_int !n_terms);
  Printf.printf "  Avg size in: %.2f, out: %.2f\n"
    (float_of_int !total_size_in /. float_of_int !n_terms)
    (float_of_int !total_size_out /. float_of_int !n_terms)
