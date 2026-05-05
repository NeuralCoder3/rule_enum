(* Index rules by (root_symbol, arity, LHS_size) for O(1) lookup *)
type rule_index = (string * int * int, Types.rule list) Hashtbl.t

let index_rules rules =
  let idx = Hashtbl.create 64 in
  List.iter (fun ((lhs, _) as rule) ->
    match lhs with
    | Types.Node (f, args) ->
      let key = (f, List.length args, Types.size lhs) in
      let existing = match Hashtbl.find_opt idx key with Some rs -> rs | None -> [] in
      Hashtbl.replace idx key (rule :: existing)
    | _ -> ()
  ) rules;
  idx

let rewrite_at_root (lhs, rhs) t =
  match Types.match_renaming lhs t with
  | Some mapping -> Some (Types.apply_renaming mapping rhs)
  | None -> None

let rec step ~index t =
  let root_match = match t with
    | Types.Var _ | Types.Hole _ -> None
    | Types.Node (f, args) ->
      match Hashtbl.find_opt index (f, List.length args, Types.size t) with
      | None -> None
      | Some indexed_rules ->
        let rec try_rules = function
          | [] -> None
          | rule :: rest ->
            match rewrite_at_root rule t with
            | Some t' -> Some t'
            | None -> try_rules rest
        in try_rules indexed_rules
  in
  match root_match with
  | Some _ as r -> r
  | None ->
    (match t with
     | Types.Var _ | Types.Hole _ -> None
     | Types.Node (f, args) ->
       let rec try_args i =
         if i >= List.length args then None
         else
           match step ~index (List.nth args i) with
           | Some new_arg ->
             Some (Types.Node (f, List.mapi (fun j a -> if j = i then new_arg else a) args))
           | None -> try_args (i + 1)
       in try_args 0)

let normalize ~index t =
  let initial_size = Types.size t in
  let current = ref t in
  let min_size = ref initial_size in
  let changed = ref true in
  while !changed do
    match step ~index !current with
    | Some t' ->
      current := t';
      let sz = Types.size !current in
      if sz < !min_size then min_size := sz
    | None -> changed := false
  done;
  (Types.canonicalize !current, !min_size < initial_size)

let normalize_with_index rules t =
  normalize ~index:(index_rules rules) t
