(* Build a "holed" copy of a term: each Var replaced by a fresh numbered Hole. *)
let holed_term start_id t =
  let next = ref start_id in
  let seen = Hashtbl.create 8 in
  let rec go = function
    | Types.Var v ->
      (match Hashtbl.find_opt seen v with
       | Some n -> Types.Hole n
       | None -> let n = !next in incr next; Hashtbl.add seen v n; Types.Hole n)
    | Types.Hole _ -> let n = !next in incr next; Types.Hole n
    | Types.Node (f, args) -> Types.Node (f, List.map go args)
  in
  let t' = go t in (t', !next)

(* Integer partitions of [total] into [parts] positive parts *)
let rec partitions total parts =
  if parts <= 0 then (if total = 0 then [[]] else [])
  else if total < parts then []
  else
    let rec loop first = function
      | 0 -> [[total]]
      | k ->
        if first > total - k then []
        else List.map (fun r -> first :: r) (partitions (total - first) k)
             @ loop (first + 1) k
    in loop 1 (parts - 1)

(* Cartesian product *)
let rec product = function
  | [] -> [[]]
  | xs :: xss -> List.concat_map (fun x -> List.map (fun r -> x :: r) (product xss)) xs

(* All set partitions of distinct integers, as (id * block) pair lists *)
let set_partitions_cache = Hashtbl.create 8

let rec set_partitions ids =
  match Hashtbl.find_opt set_partitions_cache ids with
  | Some p -> p
  | None ->
    let result =
      let n = List.length ids in
      if n = 0 then [[]]
      else
        let x = List.hd ids in
        let sub = set_partitions (List.tl ids) in
        List.concat_map (fun (part : (int * int) list) ->
          let maxb = List.fold_left (fun m (_, b) -> max m b) (-1) part in
          ((x, maxb + 1) :: part)
          :: List.init (maxb + 1) (fun b -> (x, b) :: part)
        ) sub
    in Hashtbl.add set_partitions_cache ids result; result

(* Apply a set partition and canonicalize in one pass *)
let apply_partition (part : (int * int) list) max_vars t =
  let block_count = 1 + List.fold_left (fun m (_, b) -> max m b) (-1) part in
  if block_count > max_vars then None
  else
    let hole_map = Array.make 16 (-1) in
    List.iter (fun (id, block) -> hole_map.(id) <- block) part;
    let next_var = ref 0 in
    let var_of_block = Hashtbl.create 8 in
    let rec go = function
      | Types.Hole id ->
        let b = hole_map.(id) in
         (match Hashtbl.find_opt var_of_block b with
          | Some v -> Types.Var v
          | None ->
            let v = !next_var in
            incr next_var;
            Hashtbl.add var_of_block b v;
            Types.Var v)
      | Types.Var v -> Types.Var v
      | Types.Node (f, args) -> Types.Node (f, List.map go args)
    in
    let result = go t in
    if !next_var <= max_vars then Some result else None

(* Subterm choice: fresh variable hole, or copy of an existing irreducible *)
type subterm = FreshHole | IrredCopy of Types.term

let enumerate_terms (signature : (string * int) list)
      (irreducible : Types.term list) (n : int) (max_vars : int) : Types.term list =
  if n = 1 then
    if max_vars >= 1 then [Types.Var 0] else []
  else
    let by_size = Hashtbl.create 16 in
    List.iter (fun t ->
      let sz = Types.size t in
      Hashtbl.replace by_size sz
        (t :: (match Hashtbl.find_opt by_size sz with Some ts -> ts | None -> [])))
      irreducible;

    let options_for sz =
      let opts = if sz = 1 then [FreshHole] else [] in
      match Hashtbl.find_opt by_size sz with
      | Some ts -> opts @ List.map (fun t -> IrredCopy t) ts
      | None -> opts
    in

    let seen = Hashtbl.create 64 in
    let result = ref [] in
    let add t =
      let key = Types.to_string t in
      if not (Hashtbl.mem seen key) then
        (Hashtbl.add seen key (); result := t :: !result)
    in

    List.iter (fun (sym, arity) ->
      let sym_int = Types.Sym.of_string sym in
      List.iter (fun part ->
        let opts_per_arg = List.map options_for part in
        if List.for_all (( <> ) []) opts_per_arg then
          List.iter (fun choices ->
            let next_id = ref 0 in
            let args = List.map (function
              | FreshHole -> let id = !next_id in incr next_id; Types.Hole id
              | IrredCopy t -> let (t', nid) = holed_term !next_id t in
                              next_id := nid; t')
              choices
            in
            let term = Types.Node (sym_int, args) in
            let hole_ids = List.init !next_id (fun i -> i) in
            let s = List.length hole_ids in
            if s <= 6 then
              List.iter (fun p ->
                match apply_partition p max_vars term with
                | Some canon -> add canon | None -> ())
                (set_partitions hole_ids))
            (product opts_per_arg))
        (partitions (n - 1) arity))
      signature;
    !result
