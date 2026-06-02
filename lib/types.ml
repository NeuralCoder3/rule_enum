type 's term =
  | Var of int
  | Hole of int
  | Node of 's * 's term list

type 's rule = 's term * 's term

(* Hash-cons table for term subtree sharing.

   Storage-synchronized representation: every distinct subterm is
   represented by exactly one heap object across the whole run, so
   common subterms (`a`, `(a+b)`, etc.) consume O(distinct subterms)
   memory instead of O(total occurrences). Bv arrays in `rs.behaviors`
   are unaffected, but the term forest collapses.

   Implementation notes:
   - `mk_node` always returns a physically-identical reference for
     structurally-equal `(sym, args)` inputs, provided the args were
     themselves produced by `mk_node` (or are leaves from the leaf
     caches below).
   - Hash key combines `sym` with the *physical pointers* of args,
     making hashing O(arity) regardless of subterm depth.
   - Bucket lookup uses physical equality on args (also O(arity)) which
     is valid because hash-consed args are unique by structure.
   - Cache holds STRONG references — terms persist for the run's
     lifetime. `clear_cons_cache ()` resets between runs.

   Thread-safety: the cache is a single shared Hashtbl with no locking.
   It is only ever touched by the main domain. Worker domains (the
   parallel `process_term` pool) run with `worker_mode` set, in which
   `mk_node` builds a plain `Node` without consulting the cache, so no
   two domains ever access the table concurrently. The main domain
   blocks while workers run (Pool.run waits on a condition), and
   enumeration / apply / kbo-extract — where the bulk of consing and all
   persistent term storage happen — are serial on the main domain, so
   the dedup win is preserved. *)
let cons_cache : (int, Obj.t list) Hashtbl.t = Hashtbl.create (1 lsl 14)
let cons_hits = ref 0
let cons_misses = ref 0

(* Per-domain flag: true in pool workers, false on the main domain.
   Gates `mk_node` away from the shared cache in workers. *)
let worker_mode = Stdlib.Domain.DLS.new_key (fun () -> false)
let enter_worker_mode () = Stdlib.Domain.DLS.set worker_mode true

(* Pre-cached leaves. Var 0..63 and Hole 0..63 are pre-allocated so that
   `mk_var i` / `mk_hole i` always reuse one heap object per i, enabling
   the physical-equality fast path in `mk_node`. `Var`/`Hole` carry only
   an int so the same object is sound across any `'s term` parameter;
   we cast through `Obj` to escape OCaml's value restriction. *)
let leaf_cache_size = 64
let var_leaves : Obj.t array = Array.init leaf_cache_size (fun i -> Obj.repr (Var i))
let hole_leaves : Obj.t array = Array.init leaf_cache_size (fun i -> Obj.repr (Hole i))
let mk_var i =
  if i >= 0 && i < leaf_cache_size then (Obj.obj var_leaves.(i) : _ term)
  else Var i
let mk_hole i =
  if i >= 0 && i < leaf_cache_size then (Obj.obj hole_leaves.(i) : _ term)
  else Hole i

let rec args_phys_eq a b = match a, b with
  | [], [] -> true
  | x :: xs, y :: ys -> x == y && args_phys_eq xs ys
  | _ -> false

let mk_node sym args =
  if Stdlib.Domain.DLS.get worker_mode then Node (sym, args)
  else begin
    let h = ref (Hashtbl.hash sym) in
    List.iter (fun a -> h := (!h * 31) lxor (Obj.magic a : int)) args;
    let key = !h land max_int in
    let bucket = try Hashtbl.find cons_cache key with Not_found -> [] in
    let matches obj =
      match (Obj.obj obj : _ term) with
      | Node (s', args') -> s' = sym && args_phys_eq args' args
      | Var _ | Hole _ -> false
    in
    match List.find_opt matches bucket with
    | Some t -> incr cons_hits; (Obj.obj t : _ term)
    | None ->
      incr cons_misses;
      let node = Node (sym, args) in
      Hashtbl.replace cons_cache key (Obj.repr node :: bucket);
      node
  end

let clear_cons_cache () =
  Hashtbl.clear cons_cache;
  cons_hits := 0; cons_misses := 0

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

(* Canonicalization treats Var and Hole differently because they have
   different equivalence semantics:

   - `Var i` is a *schema variable* — renaming-equivalent. Two terms that
     differ only by var renaming represent the same equivalence class, so
     vars get renumbered by left-to-right first occurrence.

   - `Hole i` is a *constant placeholder* (constP) with a fixed identity
     and a linear order (matching the proof's `Signature.linOrderC`).
     Two terms with hole-ids permuted are NOT equivalent — `Hole 0 + Hole 1`
     and `Hole 1 + Hole 0` differ in orientation, and that orientation
     matters for non-commutative operators. Holes get renumbered by
     *sorted-id rank* so the relative order of distinct hole ids is
     preserved while the term uses the smallest available ids 0..k-1. *)
let canonicalize t =
  let vmap = Array.make canonicalize_max_slots (-1) in
  let vmap_keys = Array.make canonicalize_max_slots 0 in
  let next_v = ref 0 in
  let lookup_or_insert_v k =
    let n = !next_v in
    let i = ref 0 in
    let found = ref (-1) in
    while !found < 0 && !i < n do
      if vmap_keys.(!i) = k then found := vmap.(!i);
      incr i
    done;
    if !found >= 0 then !found
    else begin
      let id = n in
      vmap_keys.(n) <- k; vmap.(n) <- id;
      incr next_v; id
    end
  in
  (* Pass 1: collect distinct hole ids. *)
  let h_count = ref 0 in
  let h_ids = Array.make canonicalize_max_slots 0 in
  let rec collect = function
    | Var _ -> ()
    | Hole h ->
      let n = !h_count in
      let i = ref 0 in let found = ref false in
      while not !found && !i < n do
        if h_ids.(!i) = h then found := true;
        incr i
      done;
      if not !found then (h_ids.(n) <- h; incr h_count)
    | Node (_, args) -> List.iter collect args
  in
  collect t;
  (* Sort the distinct ids ascending; rank[k] = sorted position of id k. *)
  let n = !h_count in
  let sorted = Array.sub h_ids 0 n in
  Array.sort compare sorted;
  let h_rank h =
    let i = ref 0 in let r = ref (-1) in
    while !r < 0 && !i < n do
      if sorted.(!i) = h then r := !i;
      incr i
    done;
    !r
  in
  let rec go = function
    | Var v -> mk_var (lookup_or_insert_v v)
    | Hole h -> mk_hole (h_rank h)
    | Node (f, args) -> mk_node f (List.map go args)
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
    | Node (f, args) -> mk_node f (List.map go args)
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
    | Node (f, args) -> mk_node f (List.map go args)
  in go t

(* Combined match for rules whose LHS may contain both Var and Hole.

   - `Var pv` in pattern matches any subterm (general substitution).
   - `Hole ph` in pattern matches a size-0 leaf in target: `Var _`,
     `Hole _`, or a 0-arity `Node`. Distinct hole ids must map to
     distinct images, and the *first-appearance* order of hole ids
     must match the order of their images under the canonical leaf
     comparator `term_compare sym_cmp`. This realizes the proof's
     `Canonical` orbit-representative selector — we only fire the rule
     in its canonical orientation, never in a permuted one.

   Returns `Some (var_map, hole_map)` on success. The hole_map is in
   discovery order, most recent first (head). *)
(* Order constraint on hole images: `img(Hole i) < img(Hole j)` iff
   `i < j`. This matches the proof's `Canonical` orbit-representative
   selector — the rule fires only on targets whose leaves at the hole
   positions, indexed by hole id, are in the same linear order as the
   hole ids themselves.

   Crucially, the constraint is on HOLE ID, not on discovery order. So
   LHS `Hole 0 + Hole 1` fires on targets with left arg < right arg
   (canonical orientation), and LHS `Hole 1 + Hole 0` fires on targets
   with left arg > right arg (non-canonical) — exactly the asymmetry
   needed to support commutativity rules. *)
let match_var_const sym_cmp pattern target =
  let vmap = ref [] in
  let hmap = ref [] in
  let order_ok_for_new ph img =
    List.for_all (fun (h', img') ->
      if h' = ph then true  (* same id: covered by consistency lookup *)
      else if ph < h' then term_compare sym_cmp img img' < 0
      else term_compare sym_cmp img' img < 0)
      !hmap
  in
  let rec go p t = match p, t with
    | Var pv, _ ->
      (match assoc_opt_int pv !vmap with
       | Some s -> term_eq sym_cmp s t
       | None -> vmap := (pv, t) :: !vmap; true)
    | Hole ph, target ->
      let is_size0 = match target with
        | Var _ -> true
        | Hole _ -> true
        | Node (_, []) -> true
        | _ -> false
      in
      if not is_size0 then false
      else (match assoc_opt_int ph !hmap with
        | Some s -> term_eq sym_cmp s target
        | None ->
          if not (order_ok_for_new ph target) then false
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

(* Reinterpret every `Var i` as `Hole i` (treating schema variables as
   constant placeholders for the purpose of KBO ordering). After this
   the term is NoVar, so the classical-KBO totality on NoVar applies.

   Used when emitting a rule whose var-form is KBO-Incomparable but
   whose constP-form is orderable. *)
let vars_to_holes t =
  let rec go = function
    | Var v -> mk_hole v
    | Hole _ as h -> h
    | Node (f, args) -> mk_node f (List.map go args)
  in go t

(* Generate the renaming-orbit of a hole-containing term: for each
   permutation π of the term's distinct hole ids, produce the term with
   hole ids relabeled by π. With the new canonicalize semantics (hole ids
   renumbered by sorted-id rank), each orbit member is a distinct canonical
   form, representing a different orientation.

   For terms with ≤ 1 distinct hole, the orbit is just `[t]`. For k
   distinct holes, the orbit has up to k! members; structural symmetries
   in the term (e.g., `Hole 0 + Hole 0`) collapse some, so we dedupe. *)
let hole_permutations t =
  let h_ids = ref [] in
  let max_id = ref (-1) in
  let rec collect = function
    | Hole h ->
      if h > !max_id then max_id := h;
      if not (List.mem h !h_ids) then h_ids := h :: !h_ids
    | Var _ -> ()
    | Node (_, args) -> List.iter collect args
  in
  collect t;
  let ids = List.sort compare !h_ids in
  let n = List.length ids in
  if n <= 1 then [t]
  else
    let rec perms_of = function
      | [] -> [[]]
      | xs ->
        List.concat_map (fun x ->
          let rest = List.filter ((<>) x) xs in
          List.map (fun p -> x :: p) (perms_of rest)) xs
    in
    let all_perms = perms_of ids in
    let seen = Hashtbl.create (List.length all_perms) in
    (* Reusable scratch array: index by source hole id, value is the
       permuted hole id. Indexed lookup is O(1) vs the O(n) List.assoc
       used previously. *)
    let map = Array.make (!max_id + 1) 0 in
    List.filter_map (fun perm ->
      List.iter2 (fun src dst -> map.(src) <- dst) ids perm;
      let rec go = function
        | Hole h -> mk_hole map.(h)
        | Var _ as v -> v
        | Node (f, args) -> mk_node f (List.map go args)
      in
      let result = go t in
      if Hashtbl.mem seen result then None
      else (Hashtbl.add seen result (); Some result)) all_perms

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
