(* Substitution-based bottom-up rewriting.

   A rule (l, r) fires on a target t when ∃σ. apply_subst σ l = t, replacing t
   with apply_subst σ r. Rules are required by construction to satisfy
   r ≺ₖ l (partial KBO), which keeps rewriting size-non-increasing under any
   substitution and hence terminating. *)

let index_rules rules =
  let idx = Hashtbl.create 64 in
  List.iter (fun ((lhs, _) as rule) ->
    match lhs with Types.Node (f, args) ->
      let key = (f, List.length args) in
      Hashtbl.replace idx key (rule :: (match Hashtbl.find_opt idx key with Some r -> r | None -> []))
    | _ -> ()
  ) rules; idx

let rewrite_at_root (lhs, rhs) t =
  match Types.match_subst lhs t with
  | Some m -> Some (Types.apply_subst m rhs) | None -> None

let try_rules rules t = List.find_map (fun r -> rewrite_at_root r t) rules

let rec norm_bottom ~index t = match t with
  | Types.Var _ | Types.Hole _ -> t
  | Types.Node (f, args) ->
    let args' = List.map (norm_bottom ~index) args in
    let t' = Types.Node (f, args') in
    (match Hashtbl.find_opt index (f, List.length args') with
     | None -> t'
     | Some rules -> match try_rules rules t' with None -> t' | Some t'' -> norm_bottom ~index t'')

let normalize ~index t =
  let sz0 = Types.size t in
  let r = Types.canonicalize (norm_bottom ~index t) in
  (r, Types.size r < sz0)

let normalize_with_index rules t = normalize ~index:(index_rules rules) t
