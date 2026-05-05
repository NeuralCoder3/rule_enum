module Sym = struct
  let table = Hashtbl.create 16
  let names : string array ref = ref [||]
  let of_string s =
    match Hashtbl.find_opt table s with
    | Some i -> i
    | None ->
      let i = Array.length !names in
      Hashtbl.add table s i; names := Array.append !names [|s|]; i
  let to_string i = (!names).(i)
  let name i = to_string i  (* alias *)
end

type symbol = int

type term =
  | Var of int
  | Hole of int
  | Node of symbol * term list

type rule = term * term

let rec size = function
  | Var _ -> 1 | Hole _ -> 1
  | Node (_, args) -> 1 + List.fold_left (fun acc t -> acc + size t) 0 args

let distinct_vars t =
  let ht = Hashtbl.create 8 in
  let rec collect = function
    | Var v -> Hashtbl.replace ht v ()
    | Hole _ -> ()
    | Node (_, args) -> List.iter collect args
  in collect t; Hashtbl.length ht

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
       | None -> let id = !next_id in incr next_id; Hashtbl.add mapping v id; Var id)
    | Hole n ->
      (match Hashtbl.find_opt mapping n with
       | Some new_v -> Var new_v
       | None -> let id = !next_id in incr next_id; Hashtbl.add mapping n id; Var id)
    | Node (f, args) -> Node (f, List.map go args)
  in go t

let rec term_compare t1 t2 =
  let s1 = size t1 and s2 = size t2 in
  if s1 <> s2 then compare s1 s2
  else match t1, t2 with
    | Var v1, Var v2 -> Int.compare v1 v2
    | Hole n1, Hole n2 -> Int.compare n1 n2
    | Var _, (Hole _ | Node _) -> -1
    | Hole _, (Var _ | Node _) -> -1
    | Node _, (Var _ | Hole _) -> 1
    | Node (f1, args1), Node (f2, args2) ->
      match Int.compare f1 f2 with
      | 0 -> lex_compare args1 args2
      | c -> c

and lex_compare args1 args2 =
  match args1, args2 with
  | [], [] -> 0
  | a1 :: rest1, a2 :: rest2 ->
    (match term_compare a1 a2 with 0 -> lex_compare rest1 rest2 | c -> c)
  | _ -> compare (List.length args1) (List.length args2)

let term_eq a b = term_compare a b = 0

module TermMap = Map.Make (struct type nonrec t = term let compare = term_compare end)
module TermSet = Set.Make (struct type nonrec t = term let compare = term_compare end)

let rec assoc_opt_int x = function
  | [] -> None
  | (k, v) :: rest -> if k = x then Some v else assoc_opt_int x rest

let match_renaming pattern target =
  let map1 = ref [] in let map2 = ref [] in
  let rec go p t = match p, t with
    | Var pv, Var tv ->
      (match assoc_opt_int pv !map1 with
       | Some w -> w = tv
       | None ->
         match assoc_opt_int tv !map2 with
         | Some w -> w = pv
         | None -> map1 := (pv, tv) :: !map1; map2 := (tv, pv) :: !map2; true)
    | Hole _, _ | Var _, (Hole _ | Node _) | Node _, (Var _ | Hole _) -> false
    | Node (pf, pargs), Node (tf, targs) ->
      pf = tf && List.length pargs = List.length targs && List.for_all2 go pargs targs
  in if go pattern target then Some !map1 else None

let apply_renaming mapping t =
  let rec go = function
    | Var v -> Var (match assoc_opt_int v mapping with Some w -> w | None -> v)
    | Hole _ as h -> h
    | Node (f, args) -> Node (f, List.map go args)
  in go t

let var_name i = String.make 1 (Char.chr (Char.code 'a' + i))

let rec to_string = function
  | Var v -> var_name v
  | Hole n -> "?" ^ string_of_int n
  | Node (f, [a]) when String.length (Sym.to_string f) = 1 ->
    "(" ^ Sym.to_string f ^ to_string a ^ ")"
  | Node (f, [a; b]) when String.length (Sym.to_string f) = 1 ->
    "(" ^ to_string a ^ Sym.to_string f ^ to_string b ^ ")"
  | Node (f, args) ->
    Sym.to_string f ^ "(" ^ String.concat "," (List.map to_string args) ^ ")"
