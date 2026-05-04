let rewrite_at_root (lhs, rhs) t =
  match Types.match_renaming lhs t with
  | Some mapping -> Some (Types.apply_renaming mapping rhs)
  | None -> None

let rec step rules t =
  let rec try_rules = function
    | [] ->
      (match t with
       | Types.Var _ | Types.Hole _ -> None
       | Types.Node (f, args) ->
         let rec try_args i =
           if i >= List.length args then None
           else
             match step rules (List.nth args i) with
             | Some new_arg ->
               let new_args =
                 List.mapi (fun j a -> if j = i then new_arg else a) args in
               Some (Types.Node (f, new_args))
             | None -> try_args (i + 1)
         in
         try_args 0)
    | rule :: rest ->
      match rewrite_at_root rule t with
      | Some t' -> Some t'
      | None -> try_rules rest
  in
  try_rules rules

let normalize rules t =
  let initial_size = Types.size t in
  let current = ref t in
  let min_size = ref initial_size in
  let changed = ref true in
  while !changed do
    match step rules !current with
    | Some t' ->
      current := t';
      let sz = Types.size !current in
      if sz < !min_size then min_size := sz
    | None -> changed := false
  done;
  let result = Types.canonicalize !current in
  (result, !min_size < initial_size)

let apply_rules_size_only size_rules kbo_rules t =
  let all_rules = size_rules @ kbo_rules in
  normalize all_rules t
