(* Time enumerate_terms in isolation against the real workload from a
   warmed-up algorithm run. Reports (µs/term) so the cost-per-output is
   comparable across optimizations. *)

open Rule_enum

let bench_size n =
  let dom = Domain_bool.bool_domain in
  let max_vcs = 3 in
  let forced = Domain_bool.all_inputs max_vcs in
  let prev = if n > 1 then n - 1 else 1 in
  let rs, _ = Algorithm.run ~max_size:prev dom ~num_random_inputs:0
    ~forced_inputs:forced ~max_vcs in
  let irrs = Algorithm.irreducibles rs in
  let t0 = Unix.gettimeofday () in
  let enumerated = Enum.enumerate_terms dom.Domain.all_symbols irrs n max_vcs in
  let dt = Unix.gettimeofday () -. t0 in
  Printf.printf "Size %d: %d irrs → %d enumerated, %.4fs (%.2f µs/term)\n%!"
    n (List.length irrs) (List.length enumerated) dt
    (1e6 *. dt /. float_of_int (max 1 (List.length enumerated)))

let () = List.iter bench_size [9; 10; 11]
