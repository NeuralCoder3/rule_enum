let holed_term start_id t =
  let next = ref start_id in let seen = Hashtbl.create 8 in
  let rec go = function
    | Types.Var v -> (match Hashtbl.find_opt seen v with
      | Some n -> Types.Hole n | None -> let n = !next in incr next; Hashtbl.add seen v n; Types.Hole n)
    | Types.Hole _ -> let n = !next in incr next; Types.Hole n
    | Types.Node (f, args) -> Types.Node (f, List.map go args)
  in let t' = go t in (t', !next)

(* Pre-compute a skeleton with holes numbered 0..k-1; the actual offset is
   added in `shift_holes` at use time, avoiding a Hashtbl allocation per use. *)
let prepare_skeleton t =
  let (t', n) = holed_term 0 t in (t', n)

let rec shift_holes shift = function
  | Types.Hole n -> Types.Hole (n + shift)
  | Types.Var _ as v -> v
  | Types.Node (f, args) -> Types.Node (f, List.map (shift_holes shift) args)

let rec partitions total parts =
  if parts <= 0 then (if total = 0 then [[]] else [])
  else if total < parts then []
  else let rec loop first = function
    | 0 -> [[total]] | k -> if first > total - k then []
      else List.map (fun r -> first :: r) (partitions (total - first) k) @ loop (first + 1) k
  in loop 1 (parts - 1)

let rec product = function
  | [] -> [[]] | xs :: xss -> List.concat_map (fun x -> List.map (fun r -> x :: r) (product xss)) xs

(* Set-partitions of {0, 1, ..., k-1} as a list of (id, block) assignments.
   Cache keyed on k (the ids list is always [0..k-1] in our enumeration, so
   keying on k avoids recomputing/rehashing the list on every call). *)
let set_partitions_cache : (int, (int * int) list list) Hashtbl.t = Hashtbl.create 8
let rec set_partitions_n k =
  match Hashtbl.find_opt set_partitions_cache k with Some p -> p | None ->
  let result = if k = 0 then [[]] else
    let x = k - 1 in
    List.concat_map (fun (part : (int * int) list) ->
      let maxb = List.fold_left (fun m (_, b) -> max m b) (-1) part in
      ((x, maxb + 1) :: part) :: List.init (maxb + 1) (fun b -> (x, b) :: part))
      (set_partitions_n (k - 1))
  in Hashtbl.add set_partitions_cache k result; result

let set_partitions ids = set_partitions_n (List.length ids)

let apply_partition (part : (int * int) list) max_vars t =
  (* Single pass over part to compute both block_count and max_id, then bail
     out before allocating if block_count exceeds the variable budget. *)
  let max_block = ref (-1) and max_id = ref (-1) in
  List.iter (fun (id, b) ->
    if id > !max_id then max_id := id;
    if b > !max_block then max_block := b) part;
  let block_count = !max_block + 1 in
  if block_count > max_vars then None else
  let hole_map = Array.make (!max_id + 1) (-1) in
  List.iter (fun (id, block) -> hole_map.(id) <- block) part;
  (* block_count ≤ max_vars (small), so an array beats a Hashtbl here. *)
  let var_of_block = Array.make block_count (-1) in
  let next_var = ref 0 in
  let rec go = function
    | Types.Hole id ->
      let b = hole_map.(id) in
      let v = var_of_block.(b) in
      if v >= 0 then Types.Var v
      else
        let v = !next_var in
        incr next_var;
        var_of_block.(b) <- v;
        Types.Var v
    | Types.Var _ as v -> v
    | Types.Node (f, args) -> Types.Node (f, List.map go args)
  in let result = go t in if !next_var <= max_vars then Some result else None

(* A subterm choice carries either a fresh hole or a pre-skeletonised
   irreducible (its hole-form) plus the number of holes it introduces. *)
type 's subterm = FreshHole | IrredCopy of 's Types.term * int

let enumerate_terms (symbols : (string * int * 's) list)
      (irreducible : 's Types.term list) (n : int) (max_vars : int) : 's Types.term list =
  if n = 1 then (if max_vars >= 1 then [Types.Var 0] else []) else
  let by_size = Hashtbl.create 16 in
  List.iter (fun t ->
    let sz = Types.size t in
    let (skel, nh) = prepare_skeleton t in
    let entry = (skel, nh) in
    let prev = try Hashtbl.find by_size sz with Not_found -> [] in
    Hashtbl.replace by_size sz (entry :: prev))
    irreducible;
  let options_for sz =
    let opts : 's subterm list = if sz = 1 then [FreshHole] else [] in
    match Hashtbl.find_opt by_size sz with
    | Some ts -> opts @ List.map (fun (skel, nh) -> IrredCopy (skel, nh)) ts
    | None -> opts
  in
  (* Estimate the seen set size optimistically: at size n with k irrs we get
     at most |symbols| * partitions * choices canonical terms. Even an order-of-
     magnitude guess avoids the cascade of Hashtbl resizes. *)
  let seen = Hashtbl.create (1 lsl 17) in let result = ref [] in
  let add t =
    if not (Hashtbl.mem seen t) then (Hashtbl.add seen t (); result := t :: !result)
  in
  List.iter (fun (_, arity, sym) ->
    List.iter (fun part ->
      let opts_per_arg = List.map options_for part in
      if List.for_all ((<>) []) opts_per_arg then
      List.iter (fun choices ->
        let next_id = ref 0 in
        let args = List.map (function
          | FreshHole -> let id = !next_id in incr next_id; Types.Hole id
          | IrredCopy (skel, nh) ->
            let shift = !next_id in next_id := !next_id + nh;
            shift_holes shift skel)
          choices
        in let term = Types.Node (sym, args) in
        let total_holes = !next_id in
        if total_holes <= 6 then
        List.iter (fun p -> match apply_partition p max_vars term with Some canon -> add canon | None -> ())
          (set_partitions_n total_holes))
        (product opts_per_arg))
      (partitions (n - 1) arity))
    symbols;
  !result
