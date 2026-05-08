let rec kbo_compare sym_cmp t1 t2 =
  let s1 = Types.size t1 and s2 = Types.size t2 in
  if s1 <> s2 then compare s1 s2
  else match t1, t2 with
    | Var v1, Var v2 -> Int.compare v1 v2
    | Hole n1, Hole n2 -> compare n1 n2
    | Var _, (Hole _ | Node _) -> -1
    | Hole _, Var _ -> 1
    | Hole _, Node _ -> -1
    | Node _, (Var _ | Hole _) -> 1
    | Node (f1, args1), Node (f2, args2) ->
      match sym_cmp f1 f2 with 0 -> lex_compare sym_cmp args1 args2 | c -> c
and lex_compare sym_cmp a b = match a, b with
  | [], [] -> 0
  | x::xs, y::ys -> (match kbo_compare sym_cmp x y with 0 -> lex_compare sym_cmp xs ys | c -> c)
  | _ -> compare (List.length a) (List.length b)

let lt sym_cmp a b = kbo_compare sym_cmp a b < 0
let minimum sym_cmp = function
  | [] -> failwith "Kbo.minimum: empty list"
  | t :: rest -> List.fold_left (fun best t -> if lt sym_cmp t best then t else best) t rest
