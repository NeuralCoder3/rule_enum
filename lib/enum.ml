let holed_term_with_offset start_id t =
  let next_id = ref start_id in
  let mapping = Hashtbl.create 16 in
  let hole_ids = ref [] in
  let rec go = function
    | Types.Var v ->
      (match Hashtbl.find_opt mapping v with
       | Some n -> Types.Hole n
       | None ->
         let n = !next_id in
         incr next_id;
         Hashtbl.add mapping v n;
         hole_ids := n :: !hole_ids;
         Types.Hole n)
    | Types.Hole _ ->
      let n = !next_id in
      incr next_id;
      hole_ids := n :: !hole_ids;
      Types.Hole n
    | Types.Node (f, args) -> Types.Node (f, List.map go args)
  in
  let result = go t in
  (result, !next_id, List.rev !hole_ids)

let rec partitions total parts =
  if parts <= 0 then
    if total = 0 then [[]] else []
  else if total < parts then []
  else
    let rec loop first rest_needed =
      if first > total - rest_needed then []
      else if rest_needed = 0 then [[total]]
      else
        let rest_partitions = partitions (total - first) (parts - 1) in
        List.map (fun r -> first :: r) rest_partitions
        @ loop (first + 1) rest_needed
    in
    loop 1 (parts - 1)

let rec cartesian_product = function
  | [] -> [[]]
  | xs :: xss ->
    List.concat_map (fun x ->
      List.map (fun rest -> x :: rest) (cartesian_product xss)
    ) xs

type subterm_option =
  | FreshHole
  | IrredCopy of Types.term

let rec all_set_partitions (lst : int list) : (int * int) list list =
  let ids = List.sort_uniq compare lst in
  let n = List.length ids in
  if n = 0 then [[]]
  else
    let first = List.hd ids in
    let rest = List.tl ids in
    let sub = all_set_partitions rest in
    List.concat_map (fun (part : (int * int) list) ->
      let max_block = match part with
        | [] -> -1
        | _ -> List.fold_left (fun m (_, b) -> max m b) (-1) part
      in
      let with_new : (int * int) list = (first, max_block + 1) :: part in
      let merged = List.init (max_block + 1) (fun b ->
        (first, b) :: part
      ) in
      with_new :: merged
    ) sub

let block_count (part : (int * int) list) =
  match part with
  | [] -> 0
  | _ -> 1 + List.fold_left (fun m (_, b) -> max m b) (-1) part

let enumerate_terms (signature : (string * int) list)
      (irreducible : Types.term list) (n : int) (max_vars : int) : Types.term list =
  if n = 1 then (
    let t = Types.canonicalize (Types.Hole 0) in
    if Types.distinct_vars t <= max_vars then [t] else []
  )
  else
    let irred_by_size = Hashtbl.create 16 in
    List.iter (fun t ->
      let sz = Types.size t in
      let existing = match Hashtbl.find_opt irred_by_size sz with
        | Some ts -> ts
        | None -> [] in
      Hashtbl.replace irred_by_size sz (t :: existing)
    ) irreducible;

    let all_options_for_size s =
      let options = ref [] in
      if s = 1 then
        options := FreshHole :: !options;
      (match Hashtbl.find_opt irred_by_size s with
       | Some ts -> options := !options @ List.map (fun t -> IrredCopy t) ts
       | None -> ());
      !options
    in

    let seen = Hashtbl.create 64 in
    let unique = ref [] in
    let add_term t =
      let key = Types.to_string t in
      if not (Hashtbl.mem seen key) then begin
        Hashtbl.add seen key ();
        unique := t :: !unique
      end
    in

    List.iter (fun (sym, arity) ->
      let part_list = partitions (n - 1) arity in
      List.iter (fun part ->
        let per_arg_options = List.map all_options_for_size part in
        if List.for_all (fun opts -> opts <> []) per_arg_options then
          let combinations = cartesian_product per_arg_options in
          List.iter (fun opts ->
            let hole_id_counter = ref 0 in
            let all_hole_ids = ref [] in
            let args = List.map (fun opt ->
              match opt with
              | FreshHole ->
                let id = !hole_id_counter in
                incr hole_id_counter;
                all_hole_ids := id :: !all_hole_ids;
                Types.Hole id
              | IrredCopy t ->
                let (t', next_id, ids) = holed_term_with_offset !hole_id_counter t in
                hole_id_counter := next_id;
                all_hole_ids := ids @ !all_hole_ids;
                t'
            ) opts in
            let term_with_holes = Types.Node (sym, args) in
            let hole_ids = List.sort_uniq compare !all_hole_ids in
            if List.length hole_ids <= 6 then
              let parts = all_set_partitions hole_ids in
              List.iter (fun part ->
                if block_count part <= max_vars then (
                  let block_map = Hashtbl.create 16 in
                  List.iter (fun (id, block) ->
                    Hashtbl.add block_map id block
                  ) part;
                  let rec apply_merge = function
                    | Types.Hole id ->
                      let block = Hashtbl.find block_map id in
                      Types.Hole block
                    | Types.Var v -> Types.Var v
                    | Types.Node (f, args) ->
                      Types.Node (f, List.map apply_merge args)
                  in
                  let merged = apply_merge term_with_holes in
                  let canon = Types.canonicalize merged in
                  if Types.distinct_vars canon <= max_vars then
                    add_term canon
                )
              ) parts
          ) combinations
      ) part_list
    ) signature;
    !unique
