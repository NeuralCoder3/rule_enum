(* Rule variables match arbitrary subterms; uninterpreted constants (Uc)
   match uninterpreted constants under order-preserving renaming. *)
type 'f t =
  | Var of int
  | Uc of int
  | App of 'f * 'f t list

(* Symbol precedence and the binary symbols treated modulo associativity
   and commutativity. *)
type 'f symbols = { precedence : 'f -> 'f -> int; ac : 'f -> bool }

let rec size = function
  | Var _ | Uc _ -> 1
  | App (_, ts) -> List.fold_left (fun n t -> n + size t) 1 ts

let rec fold f acc = function
  | App (_, ts) as t -> List.fold_left (fold f) (f acc t) ts
  | leaf -> f acc leaf

let rec map_leaves f = function
  | App (g, ts) -> App (g, List.map (map_leaves f) ts)
  | leaf -> f leaf

let leaves t = List.rev (fold (fun acc -> function App _ -> acc | l -> l :: acc) [] t)

let ids pick t = List.sort_uniq Int.compare (List.filter_map pick (leaves t))
let vars t = ids (function Var i -> Some i | _ -> None) t
let ucs t = ids (function Uc i -> Some i | _ -> None) t

let rename ~var ~uc t =
  map_leaves (function Var i -> Var (var i) | Uc i -> Uc (uc i) | t -> t) t

let regular ~lhs rhs =
  let subset xs ys = List.for_all (fun x -> List.mem x ys) xs in
  subset (vars rhs) (vars lhs) && subset (ucs rhs) (ucs lhs)

(* Representative modulo variable renaming and order-preserving Uc renaming. *)
let canonical t =
  let first_seen =
    List.fold_left
      (fun seen -> function Var i when not (List.mem i seen) -> i :: seen | _ -> seen)
      [] (leaves t)
    |> List.rev
  in
  let us = ucs t in
  rename ~var:(fun i -> Combi.index_of i first_seen) ~uc:(fun i -> Combi.index_of i us) t

(* All renamings of [t] into [vars] variable and [ucs] Uc slots. *)
let instances ~vars:nv ~ucs:nu t =
  let vs = vars t and us = ucs t in
  Combi.injections (List.length vs) nv
  |> Seq.flat_map (fun vimg ->
         Combi.increasing (List.length us) nu
         |> Seq.map (fun uimg ->
                rename
                  ~var:(fun i -> List.nth vimg (Combi.index_of i vs))
                  ~uc:(fun i -> List.nth uimg (Combi.index_of i us))
                  t))

let rec operands f = function
  | App (g, [ a; b ]) when g = f -> operands f a @ operands f b
  | t -> [ t ]

(* Total order extending the (AC-)KBO with Ucs below all symbols, treating
   variables as minimal constants; their mutual order is free since the
   KBO never compares them. *)
let rec compare ?(var = Int.compare) syms s t =
  match Int.compare (size s) (size t) with
  | 0 -> (
      match s, t with
      | Var i, Var j -> var i j
      | Var _, _ -> -1
      | _, Var _ -> 1
      | Uc i, Uc j -> Int.compare i j
      | Uc _, _ -> -1
      | _, Uc _ -> 1
      | App (f, ss), App (g, ts) -> (
          match syms.precedence f g with
          | 0 when syms.ac f -> compare_ac ~var syms f (operands f s) (operands f t)
          | 0 -> List.compare (compare ~var syms) ss ts
          | c -> c))
  | c -> c

(* Korovin-Voronkov AC-KBO: compare the operands headed above [f], then the
   number of operands, then all operands, as multisets. *)
and compare_ac ~var syms f ss ts =
  let multiset xs ys =
    let descending = List.sort (fun a b -> compare ~var syms b a) in
    List.compare (compare ~var syms) (descending xs) (descending ys)
  in
  let big = List.filter (function App (g, _) -> syms.precedence g f > 0 | _ -> false) in
  match multiset (big ss) (big ts) with
  | 0 -> ( match Int.compare (List.length ss) (List.length ts) with 0 -> multiset ss ts | c -> c)
  | c -> c

(* AC chains are flattened, sorted and nested to the right. *)
let app syms f args =
  if not (syms.ac f) then App (f, args)
  else
    let rec nest = function [ t ] -> t | t :: ts -> App (f, [ t; nest ts ]) | [] -> App (f, args) in
    nest (List.sort (compare syms) (List.concat_map (operands f) args))

let rec normalize syms = function
  | App (f, ts) -> app syms f (List.map (normalize syms) ts)
  | leaf -> leaf
