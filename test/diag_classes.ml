(* Diagnostic: how many distinct behavior vectors do the algorithm's stored
   irreducibles cover? With exhaustive bool inputs over 3 vars there are at
   most 2^(2^3) = 256 ground equivalence classes, so the number of distinct
   behaviors should be ≤ 256. If the algorithm reports more irreducibles
   than distinct behaviors, those extras share a behavior — i.e., they live
   in the same ≈-class but the algorithm is keeping them as separate canonical
   reps. *)

open Rule_enum

let () =
  let dom = Domain_bool.bool_domain in
  let max_vcs = 3 in
  let forced = Domain_bool.all_inputs max_vcs in
  let max_size = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 8 in
  Printf.printf "Running bool, max-vcs=%d, max-size=%d, forced=%d inputs\n%!"
    max_vcs max_size (List.length forced);
  let rs, _ = Algorithm.run ~max_size dom ~num_random_inputs:0
    ~forced_inputs:forced ~max_vcs in
  let irr_count = List.length rs.Algorithm.behaviors in
  let module S = Set.Make (struct
    type t = bool list
    let compare = compare
  end) in
  let bvs = List.fold_left (fun acc (_, bv, _) -> S.add bv acc) S.empty rs.Algorithm.behaviors in
  let unique_bvs = S.cardinal bvs in
  Printf.printf "  irreducibles: %d\n" irr_count;
  Printf.printf "  unique behaviors: %d\n" unique_bvs;
  Printf.printf "  duplicates (irreducibles in multi-rep classes): %d\n"
    (irr_count - unique_bvs);
  (* For each behavior with > 1 irreducible, show how many. *)
  let by_bv = Hashtbl.create 64 in
  List.iter (fun (irr, bv, _) ->
    let lst = try Hashtbl.find by_bv bv with Not_found -> [] in
    Hashtbl.replace by_bv bv (irr :: lst)) rs.Algorithm.behaviors;
  let sym_str = Domain_bool.string_of_symbol in
  let multi = Hashtbl.fold (fun bv terms acc ->
    if List.length terms > 1 then (bv, terms) :: acc else acc) by_bv [] in
  let multi = List.sort (fun (_, a) (_, b) -> compare (List.length b) (List.length a)) multi in
  let show_n = min 3 (List.length multi) in
  Printf.printf "  top %d behaviors with most reps:\n" show_n;
  let rec take n = function [] -> [] | _ when n = 0 -> [] | x :: xs -> x :: take (n-1) xs in
  List.iter (fun (_, terms) ->
    let n = List.length terms in
    let unique = List.sort_uniq compare terms in
    let n_unique = List.length unique in
    Printf.printf "    %d reps (%d unique syntactic):\n" n n_unique;
    List.iter (fun t ->
      Printf.printf "      %s\n" (Types.to_string sym_str t))
      (take 3 (List.sort (fun a b -> compare (Types.size a) (Types.size b)) unique)))
    (take show_n multi)
