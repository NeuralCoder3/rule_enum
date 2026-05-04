let rec kbo_compare t1 t2 =
  let s1 = Types.size t1 and s2 = Types.size t2 in
  if s1 <> s2 then compare s1 s2
  else
    match t1, t2 with
    | Var v1, Var v2 -> String.compare v1 v2
    | Hole n1, Hole n2 -> compare n1 n2
    | Var _, (Hole _ | Node _) -> -1
    | Hole _, (Var _ | Node _) -> -1
    | Node _, (Var _ | Hole _) -> 1
    | Node (f1, args1), Node (f2, args2) ->
      match String.compare f1 f2 with
      | 0 -> lex_compare args1 args2
      | c -> c

and lex_compare args1 args2 =
  match args1, args2 with
  | [], [] -> 0
  | a1 :: rest1, a2 :: rest2 ->
    (match kbo_compare a1 a2 with
     | 0 -> lex_compare rest1 rest2
     | c -> c)
  | _ -> compare (List.length args1) (List.length args2)

let lt a b = kbo_compare a b < 0
let le a b = kbo_compare a b <= 0
let gt a b = kbo_compare a b > 0
let ge a b = kbo_compare a b >= 0

let minimum terms =
  match terms with
  | [] -> failwith "Kbo.minimum: empty list"
  | t :: rest ->
    List.fold_left (fun best t -> if lt t best then t else best) t rest
