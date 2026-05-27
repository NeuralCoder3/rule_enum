(* Profile the actual algorithm pipeline at size 9. *)

open Rule_enum

let dom = Domain_int.int_domain
let sym_cmp = dom.Domain.sym_compare

let profile_iteration n irrs rules inputs =
  Printf.printf "\n=== Profile size %d (irrs=%d, rules=%d) ===\n%!"
    n (List.length irrs) (List.length rules);
  let t0 = Sys.time () in
  let enumerated = Enum.enumerate_terms_caps dom.Domain.all_symbols irrs n
    { max_vars = 3; max_holes = 3; max_vcs = 3 } in
  let t_enum = Sys.time () -. t0 in
  Printf.printf "  enum:           %.3fs  (%d terms)\n%!"
    t_enum (List.length enumerated);

  let t1 = Sys.time () in
  let norm_index = Rewrite.index_rules rules in
  let compiled_inputs = List.map Eval.compile inputs in
  let t_setup = Sys.time () -. t1 in
  Printf.printf "  setup (index+compile): %.3fs\n%!" t_setup;

  let t2 = Sys.time () in
  let normed = List.filter_map (fun t ->
    let (s, reduced) = Rewrite.normalize ~sym_cmp ~index:norm_index t in
    if reduced then None else Some s) enumerated in
  let t_norm = Sys.time () -. t2 in
  Printf.printf "  normalize:      %.3fs  (%d survived of %d)\n%!"
    t_norm (List.length normed) (List.length enumerated);

  let t3 = Sys.time () in
  let with_bv = List.map (fun s ->
    (s, Eval.behavior_compiled dom compiled_inputs s)) normed in
  let t_bv = Sys.time () -. t3 in
  Printf.printf "  bv-eval:        %.3fs\n%!" t_bv;

  let t4 = Sys.time () in
  let h = Hashtbl.create 1024 in
  List.iter (fun (s, bv) ->
    let prev = try Hashtbl.find h bv with Not_found -> [] in
    Hashtbl.replace h bv (s :: prev)) with_bv;
  let t_bucket = Sys.time () -. t4 in
  Printf.printf "  bucket-by-bv:   %.3fs  (%d distinct bvs)\n%!"
    t_bucket (Hashtbl.length h);

  let t5 = Sys.time () in
  let groups = Hashtbl.fold (fun _ terms acc -> terms :: acc) h [] in
  let _all_pairs = List.fold_left (fun n g -> n + List.length g * (List.length g - 1) / 2) 0 groups in
  let t_groups = Sys.time () -. t5 in
  Printf.printf "  groups extracted: %.3fs (%d groups)\n%!" t_groups (List.length groups);

  let t6a = Sys.time () in
  let _minimals_uncached = List.concat_map (fun terms ->
    List.filter (fun t -> not (List.exists (fun s -> Kbo.lt sym_cmp s t) terms)) terms
  ) groups in
  let t_kbo_uncached = Sys.time () -. t6a in
  Printf.printf "  kbo-minimal (uncached): %.3fs\n%!" t_kbo_uncached;

  let t6b = Sys.time () in
  let _minimals_cached = List.concat_map (fun terms ->
    let cached = List.map Kbo.cache terms in
    List.filter_map (fun ((t, _, _) as c) ->
      if not (List.exists (fun s -> Kbo.lt_cached sym_cmp s c) cached)
      then Some t else None) cached
  ) groups in
  let t_kbo = Sys.time () -. t6b in
  Printf.printf "  kbo-minimal (cached):   %.3fs\n%!" t_kbo;

  let total = t_enum +. t_setup +. t_norm +. t_bv +. t_bucket +. t_groups +. t_kbo in
  Printf.printf "  ----- TOTAL:    %.3fs (%.2f µs/term)\n%!" total
    (1e6 *. total /. float_of_int (max 1 (List.length enumerated)));
  Printf.printf "  Breakdown: enum=%.0f%% norm=%.0f%% bv=%.0f%% kbo-min=%.0f%% other=%.0f%%\n%!"
    (100.*.t_enum/.total) (100.*.t_norm/.total)
    (100.*.t_bv/.total) (100.*.t_kbo/.total)
    (100.*.(t_setup +. t_bucket +. t_groups)/.total)

let () =
  Printf.printf "Building up to size 8...\n%!";
  let rs, _ = Algorithm.run ~max_size:8 dom
    ~num_random_inputs:100 ~max_vcs:3 ~max_vars:3 ~max_holes:3 in
  let irrs = Algorithm.irreducibles rs in
  let rules = Algorithm.all_rules rs in
  let inputs = rs.Algorithm.inputs in
  Printf.printf "  Final: %d irrs, %d rules\n%!"
    (List.length irrs) (List.length rules);
  profile_iteration 8 irrs rules inputs;
  profile_iteration 9 irrs rules inputs
