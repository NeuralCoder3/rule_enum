type 's term =
  | Var of int
  | Hole of int
  | Node of 's * 's term list

type 's rule = 's term * 's term

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

let distinct_holes t =
  let ht = Hashtbl.create 8 in
  let rec collect = function
    | Hole n -> Hashtbl.replace ht n ()
    | Var _ -> ()
    | Node (_, args) -> List.iter collect args
  in collect t; Hashtbl.length ht

let num_distinct_vcs t = distinct_vars t + distinct_holes t

let rec has_hole = function
  | Hole _ -> true
  | Var _ -> false
  | Node (_, args) -> List.exists has_hole args

let var_counts t =
  let ht = Hashtbl.create 8 in
  let rec go = function
    | Var v ->
      let c = try Hashtbl.find ht v with Not_found -> 0 in
      Hashtbl.replace ht v (c + 1)
    | Hole _ -> ()
    | Node (_, args) -> List.iter go args
  in go t; ht

let var_counts_le ca cb =
  Hashtbl.fold (fun v c acc ->
    acc && c <= (try Hashtbl.find cb v with Not_found -> 0)) ca true

(* Fast var-count representation: a small association list of (var_id, count)
   sorted by var_id ascending. Pure-functional, no Hashtbl allocation.
   Comparable in O(min(la, lb)) time. *)
type var_counts_arr = (int * int) list

let var_counts_arr t : var_counts_arr =
  let rec collect acc = function
    | Var v -> v :: acc
    | Hole _ -> acc
    | Node (_, args) -> List.fold_left collect acc args
  in
  let ids = List.sort_uniq compare (collect [] t) in
  List.map (fun v ->
    let c = ref 0 in
    let rec count = function
      | Var v' when v' = v -> incr c
      | Var _ | Hole _ -> ()
      | Node (_, args) -> List.iter count args
    in count t; (v, !c)) ids

(* Are all (v, ca[v]) ≤ (v, cb[v]) ? Both lists sorted by v. *)
let rec var_counts_arr_le (a : var_counts_arr) (b : var_counts_arr) =
  match a, b with
  | [], _ -> true
  | _ :: _, [] -> false
  | (va, ca) :: a_rest, (vb, cb) :: b_rest ->
    if va = vb then ca <= cb && var_counts_arr_le a_rest b_rest
    else if va < vb then false  (* a has v that b doesn't *)
    else var_counts_arr_le a b_rest  (* skip b's smaller v *)

let rec map_vars f = function
  | Var v -> Var (f v) | Hole n -> Hole n
  | Node (sym, args) -> Node (sym, List.map (map_vars f) args)

(* Renumber Vars and Holes each by left-to-right first occurrence, into
   separate id spaces. Vars become 0, 1, 2, …; Holes become 0, 1, 2, …
   independently.

   Hot path: called on every normalized term. Uses small stack-allocated
   arrays (bounded by `canonicalize_max_slots`) instead of Hashtbls to
   avoid per-call GC pressure. The arrays are -1-filled lazily; linear
   search is fastest for ≤ 16 entries. *)
let canonicalize_max_slots = 32

let canonicalize t =
  let vmap = Array.make canonicalize_max_slots (-1) in
  let hmap = Array.make canonicalize_max_slots (-1) in
  let vmap_keys = Array.make canonicalize_max_slots 0 in
  let hmap_keys = Array.make canonicalize_max_slots 0 in
  let next_v = ref 0 and next_h = ref 0 in
  let lookup_or_insert keys vmap next k =
    let n = !next in
    let i = ref 0 in
    let found = ref (-1) in
    while !found < 0 && !i < n do
      if keys.(!i) = k then found := vmap.(!i);
      incr i
    done;
    if !found >= 0 then !found
    else begin
      let id = n in
      keys.(n) <- k;
      vmap.(n) <- id;
      incr next;
      id
    end
  in
  let rec go = function
    | Var v -> Var (lookup_or_insert vmap_keys vmap next_v v)
    | Hole h -> Hole (lookup_or_insert hmap_keys hmap next_h h)
    | Node (f, args) -> Node (f, List.map go args)
  in go t

let rec term_compare sym_cmp t1 t2 =
  let s1 = size t1 and s2 = size t2 in
  if s1 <> s2 then compare s1 s2
  else match t1, t2 with
    | Var v1, Var v2 -> Int.compare v1 v2
    | Hole n1, Hole n2 -> Int.compare n1 n2
    | Var _, (Hole _ | Node _) -> -1
    | Hole _, Var _ -> 1
    | Hole _, Node _ -> -1
    | Node _, (Var _ | Hole _) -> 1
    | Node (f1, args1), Node (f2, args2) ->
      match sym_cmp f1 f2 with
      | 0 -> lex_compare sym_cmp args1 args2
      | c -> c

and lex_compare sym_cmp args1 args2 =
  match args1, args2 with
  | [], [] -> 0
  | a1 :: r1, a2 :: r2 ->
    (match term_compare sym_cmp a1 a2 with 0 -> lex_compare sym_cmp r1 r2 | c -> c)
  | _ -> compare (List.length args1) (List.length args2)

let term_eq sym_cmp a b = term_compare sym_cmp a b = 0

let rec assoc_opt_int x = function
  | [] -> None | (k, v) :: rest -> if k = x then Some v else assoc_opt_int x rest

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

(* Var-only general substitution (legacy, used for hole-free LHS rules). *)
let match_subst pattern target =
  let map = ref [] in
  let rec go p t = match p, t with
    | Var pv, _ ->
      (match assoc_opt_int pv !map with
       | Some s -> s = t
       | None -> map := (pv, t) :: !map; true)
    | Hole _, _ -> false
    | Node _, (Var _ | Hole _) -> false
    | Node (pf, pargs), Node (tf, targs) ->
      pf = tf && List.length pargs = List.length targs && List.for_all2 go pargs targs
  in if go pattern target then Some !map else None

let apply_subst mapping t =
  let rec go = function
    | Var v -> (match assoc_opt_int v mapping with Some s -> s | None -> Var v)
    | Hole _ as h -> h
    | Node (f, args) -> Node (f, List.map go args)
  in go t

(* Combined match for rules whose LHS may contain both Var and Hole.

   - `Var pv` in pattern matches any subterm (general substitution).
   - `Hole ph` in pattern matches only a size-0 leaf in target: another
     `Hole _` or a 0-arity `Node`. `Var _` in target does NOT match a Hole
     in pattern (Mapping A: at rule-application time the user term is
     ground, no Vars).
   - Hole order preservation: as new Hole ids are discovered in the
     pattern's left-to-right traversal, their images (in that discovery
     order) must be strictly increasing under `term_compare sym_cmp`.
     This realizes the proof's `Canonical` orbit-representative selector
     — we only fire the rule in its canonical orientation.

   Returns `Some (var_map, hole_map)` on success. The hole_map is in
   discovery order, most recent first (head). *)
let match_var_const sym_cmp pattern target =
  let vmap = ref [] in
  let hmap = ref [] in
  let rec go p t = match p, t with
    | Var pv, _ ->
      (match assoc_opt_int pv !vmap with
       | Some s -> term_eq sym_cmp s t
       | None -> vmap := (pv, t) :: !vmap; true)
    | Hole ph, target ->
      let is_size0 = match target with
        | Hole _ -> true
        | Node (_, []) -> true
        | _ -> false
      in
      if not is_size0 then false
      else (match assoc_opt_int ph !hmap with
        | Some s -> term_eq sym_cmp s target
        | None ->
          let ord_ok = match !hmap with
            | [] -> true
            | (_, prev_img) :: _ -> term_compare sym_cmp prev_img target < 0
          in
          if not ord_ok then false
          else (hmap := (ph, target) :: !hmap; true))
    | Node _, (Var _ | Hole _) -> false
    | Node (pf, pargs), Node (tf, targs) ->
      sym_cmp pf tf = 0 && List.length pargs = List.length targs && List.for_all2 go pargs targs
  in if go pattern target then Some (!vmap, !hmap) else None

let apply_var_const vmap hmap t =
  let rec go = function
    | Var v -> (match assoc_opt_int v vmap with Some s -> s | None -> Var v)
    | Hole n -> (match assoc_opt_int n hmap with Some s -> s | None -> Hole n)
    | Node (f, args) -> Node (f, List.map go args)
  in go t

let var_names_cache =
  Array.init 26 (fun i -> String.make 1 (Char.chr (Char.code 'a' + i)))
let hole_names_cache =
  Array.init 26 (fun i -> String.make 1 (Char.chr (Char.code 'A' + i)))
let var_name i =
  if i >= 0 && i < 26 then var_names_cache.(i)
  else String.make 1 (Char.chr (Char.code 'a' + i))
let hole_name i =
  if i >= 0 && i < 26 then hole_names_cache.(i)
  else String.make 1 (Char.chr (Char.code 'A' + i))

let rec to_string sym_str = function
  | Var v -> var_name v
  | Hole n -> hole_name n
  | Node (f, [a]) when String.length (sym_str f) = 1 ->
    "(" ^ sym_str f ^ to_string sym_str a ^ ")"
  | Node (f, [a; b]) when String.length (sym_str f) = 1 ->
    "(" ^ to_string sym_str a ^ sym_str f ^ to_string sym_str b ^ ")"
  | Node (f, args) ->
    sym_str f ^ "(" ^ String.concat "," (List.map (to_string sym_str) args) ^ ")"
