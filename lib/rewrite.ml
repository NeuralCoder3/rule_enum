(* Index rules by (root_symbol, arity, LHS_size) for O(1) lookup *)
type rule_index = (int * int * int, Types.rule list) Hashtbl.t

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

let try_rules rules t =
  let rec loop = function
    | [] -> None
    | rule :: rest ->
      match rewrite_at_root rule t with Some t' -> Some t' | None -> loop rest
  in loop rules

(* Bottom-up normalise children first, then try root rewrite, recurse if rewrite succeeds.
   Does NOT canonicalise intermediate results — only the final normal form. *)
let rec norm_bottom ~index t =
  match t with
  | Types.Var _ | Types.Hole _ -> t
  | Types.Node (f, args) ->
    let args' = List.map (norm_bottom ~index) args in
    let t' = Types.Node (f, args') in
    match Hashtbl.find_opt index (f, List.length args', Types.size t') with
    | None -> t'
    | Some rules ->
      match try_rules rules t' with
      | None -> t'
      | Some t'' -> norm_bottom ~index t''

let normalize ~index t =
  let initial_size = Types.size t in
  let result = Types.canonicalize (norm_bottom ~index t) in
  (result, Types.size result < initial_size)

let normalize_with_index rules t =
  normalize ~index:(index_rules rules) t
