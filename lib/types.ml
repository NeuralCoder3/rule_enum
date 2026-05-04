type symbol = string

type term =
  | Var of string
  | Hole of int
  | Node of symbol * term list

type rule = term * term

let rec size = function
  | Var _ -> 1
  | Hole _ -> 1
  | Node (_, args) -> 1 + List.fold_left (fun acc t -> acc + size t) 0 args

let rec map_vars f = function
  | Var v -> Var (f v)
  | Hole n -> Hole n
  | Node (sym, args) -> Node (sym, List.map (map_vars f) args)

let canonicalize t =
  let next_id = ref 0 in
  let mapping = Hashtbl.create 16 in
  let rec go = function
    | Var v ->
      (match Hashtbl.find_opt mapping v with
       | Some new_v -> Var new_v
       | None ->
         let idx = !next_id in
         incr next_id;
         let new_v = String.make 1 (Char.chr (Char.code 'a' + idx)) in
         Hashtbl.add mapping v new_v;
         Var new_v)
    | Hole n ->
      let n_str = string_of_int n in
      (match Hashtbl.find_opt mapping n_str with
       | Some new_v -> Var new_v
       | None ->
         let idx = !next_id in
         incr next_id;
         let new_v = String.make 1 (Char.chr (Char.code 'a' + idx)) in
         Hashtbl.add mapping n_str new_v;
         Var new_v)
    | Node (f, args) -> Node (f, List.map go args)
  in
  go t

let rec term_compare t1 t2 =
  let s1 = size t1 and s2 = size t2 in
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
    (match term_compare a1 a2 with
     | 0 -> lex_compare rest1 rest2
     | c -> c)
  | _ -> compare (List.length args1) (List.length args2)

let term_eq a b = term_compare a b = 0

module TermMap = Map.Make (struct
  type nonrec t = term
  let compare = term_compare
end)

module TermSet = Set.Make (struct
  type nonrec t = term
  let compare = term_compare
end)

let match_renaming pattern target =
  let map1 = Hashtbl.create 16 in
  let map2 = Hashtbl.create 16 in
  let rec go p t =
    match p, t with
    | Var pv, Var tv ->
      (match Hashtbl.find_opt map1 pv with
       | Some w -> w = tv
       | None ->
         (match Hashtbl.find_opt map2 tv with
          | Some w -> w = pv
          | None ->
            Hashtbl.add map1 pv tv;
            Hashtbl.add map2 tv pv;
            true))
    | Var _, (Hole _ | Node _) -> false
    | Hole _, Hole _ -> false
    | Hole _, (Var _ | Node _) -> false
    | Node (pf, pargs), Node (tf, targs) ->
      pf = tf && List.length pargs = List.length targs
      && List.for_all2 go pargs targs
    | Node _, (Var _ | Hole _) -> false
  in
  if go pattern target then
    Some (Hashtbl.fold (fun k v acc -> (k, v) :: acc) map1 [])
  else
    None

let apply_renaming mapping t =
  let tbl = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) mapping;
  let rec go = function
    | Var v ->
      (match Hashtbl.find_opt tbl v with
       | Some new_v -> Var new_v
       | None -> Var v)
    | Hole _ as h -> h
    | Node (f, args) -> Node (f, List.map go args)
  in
  go t

let rec to_string = function
  | Var v -> v
  | Hole n -> "?" ^ string_of_int n
  | Node (f, [a; b]) when String.length f = 1 ->
    "(" ^ to_string a ^ f ^ to_string b ^ ")"
  | Node (f, args) ->
    f ^ "(" ^ String.concat "," (List.map to_string args) ^ ")"
