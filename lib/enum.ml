(* Enumeration of canonical rule-construction terms.

   Two leaf kinds appear: `Var i` (schema variable, general substitution
   at rule application) and `Hole n` (ConstPlaceholder, order-preserving
   size-0 rename). The combined bound `k` (= max_vcs) caps the sum of
   distinct vars and distinct holes per term, mirroring the proof's
   `numDistinctVCs ≤ k`.

   Subterm reuse: each known irreducible is "skeletonised" once. Each
   `Var v` becomes a fresh v-slot id (consistent across occurrences of
   the same v); each `Hole n` becomes a fresh h-slot id (consistent
   across occurrences of the same n). At use time we just shift the two
   id spaces by independent offsets. The two `FreshVarSlot` /
   `FreshHoleSlot` options at leaf positions introduce a single new slot
   in their respective namespace.

   After choosing arguments, two independent set-partitions (one on
   v-slot ids, one on h-slot ids) are enumerated; their Cartesian
   product yields all canonical (var, hole) renumberings. We reject any
   combination whose distinct-var + distinct-hole sum exceeds `k`. *)

(* Skeleton conversion. Var v and Hole n are each remapped into a fresh
   per-original-id slot in their own namespace. Returns (skeleton,
   num_v_slots, num_h_slots). *)
let prepare_skeleton t =
  let next_v = ref 0 and next_h = ref 0 in
  let seen_v = Hashtbl.create 4 and seen_h = Hashtbl.create 4 in
  let rec go = function
    | Types.Var v ->
      (match Hashtbl.find_opt seen_v v with
       | Some sid -> Types.Var sid
       | None -> let sid = !next_v in incr next_v;
         Hashtbl.add seen_v v sid; Types.Var sid)
    | Types.Hole n ->
      (match Hashtbl.find_opt seen_h n with
       | Some sid -> Types.Hole sid
       | None -> let sid = !next_h in incr next_h;
         Hashtbl.add seen_h n sid; Types.Hole sid)
    | Types.Node (f, args) -> Types.Node (f, List.map go args)
  in let t' = go t in (t', !next_v, !next_h)

let rec shift_slots ~v_shift ~h_shift = function
  | Types.Var v -> Types.Var (v + v_shift)
  | Types.Hole n -> Types.Hole (n + h_shift)
  | Types.Node (f, args) ->
    Types.Node (f, List.map (shift_slots ~v_shift ~h_shift) args)

let rec partitions total parts =
  if parts <= 0 then (if total = 0 then [[]] else [])
  else if total < parts then []
  else let rec loop first = function
    | 0 -> [[total]] | k -> if first > total - k then []
      else List.map (fun r -> first :: r) (partitions (total - first) k) @ loop (first + 1) k
  in loop 1 (parts - 1)

let rec product = function
  | [] -> [[]] | xs :: xss -> List.concat_map (fun x -> List.map (fun r -> x :: r) (product xss)) xs

(* Set partitions of {0..k-1} as (id, block) pair lists. Block ids are
   assigned in left-to-right order of first appearance, so the resulting
   canonical-id numbering is well-defined. *)
let set_partitions_cache : (int, (int * int) list list) Hashtbl.t = Hashtbl.create 8
let rec set_partitions_n k =
  match Hashtbl.find_opt set_partitions_cache k with Some p -> p | None ->
  let result = if k = 0 then [[]] else
    let x = k - 1 in
    List.concat_map (fun (part : (int * int) list) ->
      let maxb = List.fold_left (fun m (_, b) -> max m b) (-1) part in
      ((x, maxb + 1) :: part) :: List.init (maxb + 1) (fun b -> (x, b) :: part))
      (set_partitions_n (k - 1))
  in Hashtbl.add set_partitions_cache k result; result

(* Caps for the enumeration. Either bound:
   * `max_vars` — upper bound on distinct `Var`s per term;
   * `max_holes` — upper bound on distinct `Hole`s per term;
   * `max_vcs` — upper bound on distinct vars + distinct holes (the
     proof's numDistinctVCs).
   All three must hold simultaneously. To disable holes entirely, pass
   `max_holes = 0`. *)
type caps = { max_vars : int; max_holes : int; max_vcs : int }

(* Apply two independent partitions (one for v-slots, one for h-slots) to
   a skeleton, producing a canonical term over `Var 0..nv-1` and
   `Hole 0..nh-1`. Rejects when caps are exceeded. Canonical renumbering
   is by left-to-right first occurrence in the resulting term, computed
   here in a single pass. *)
let apply_partitions ~v_part ~h_part ~caps ~max_v_slots ~max_h_slots t =
  let v_block_count = 1 + List.fold_left (fun m (_, b) -> max m b) (-1) v_part in
  let h_block_count = 1 + List.fold_left (fun m (_, b) -> max m b) (-1) h_part in
  if v_block_count > caps.max_vars
     || h_block_count > caps.max_holes
     || v_block_count + h_block_count > caps.max_vcs then None else
  let v_map = Array.make (max max_v_slots 1) (-1) in
  let h_map = Array.make (max max_h_slots 1) (-1) in
  List.iter (fun (id, b) -> v_map.(id) <- b) v_part;
  List.iter (fun (id, b) -> h_map.(id) <- b) h_part;
  (* Canonical block→canonical-id, in left-to-right first-appearance order. *)
  let v_canon = Array.make (max v_block_count 1) (-1) in
  let h_canon = Array.make (max h_block_count 1) (-1) in
  let next_v = ref 0 and next_h = ref 0 in
  let rec go = function
    | Types.Var slot_id ->
      let b = v_map.(slot_id) in
      let c = v_canon.(b) in
      if c >= 0 then Types.Var c
      else (let c = !next_v in incr next_v; v_canon.(b) <- c; Types.Var c)
    | Types.Hole slot_id ->
      let b = h_map.(slot_id) in
      let c = h_canon.(b) in
      if c >= 0 then Types.Hole c
      else (let c = !next_h in incr next_h; h_canon.(b) <- c; Types.Hole c)
    | Types.Node (f, args) -> Types.Node (f, List.map go args)
  in
  let result = go t in
  if !next_v <= caps.max_vars
     && !next_h <= caps.max_holes
     && !next_v + !next_h <= caps.max_vcs then Some result else None

type 's subterm =
  | FreshVarSlot
  | FreshHoleSlot
  | IrredCopy of 's Types.term * int * int  (* skel, num_v_slots, num_h_slots *)

let enumerate_terms_caps (symbols : (string * int * 's) list)
      (irreducible : 's Types.term list) (n : int) (caps : caps)
      : 's Types.term list =
  if n = 1 then begin
    let var_leaf = if caps.max_vars >= 1 && caps.max_vcs >= 1
                   then [Types.Var 0] else [] in
    let hole_leaf = if caps.max_holes >= 1 && caps.max_vcs >= 1
                    then [Types.Hole 0] else [] in
    let consts = List.filter_map (fun (_, ar, s) ->
      if ar = 0 then Some (Types.Node (s, [])) else None) symbols in
    var_leaf @ hole_leaf @ consts
  end else
  let by_size = Hashtbl.create 16 in
  List.iter (fun t ->
    let sz = Types.size t in
    let (skel, nv, nh) = prepare_skeleton t in
    let prev = try Hashtbl.find by_size sz with Not_found -> [] in
    Hashtbl.replace by_size sz ((skel, nv, nh) :: prev))
    irreducible;
  let options_for sz =
    let opts : 's subterm list =
      if sz = 1 then begin
        let v_opt = if caps.max_vars > 0 then [FreshVarSlot] else [] in
        let h_opt = if caps.max_holes > 0 then [FreshHoleSlot] else [] in
        v_opt @ h_opt
      end else [] in
    let from_irr =
      match Hashtbl.find_opt by_size sz with
      | Some ts -> List.map (fun (skel, nv, nh) -> IrredCopy (skel, nv, nh)) ts
      | None -> []
    in
    (* For sz = 1 also include 0-arity constant nodes from the signature. *)
    let consts =
      if sz = 1 then
        List.filter_map (fun (_, ar, s) ->
          if ar = 0 then Some (IrredCopy (Types.Node (s, []), 0, 0))
          else None) symbols
      else []
    in
    opts @ consts @ from_irr
  in
  let seen = Hashtbl.create (1 lsl 17) in let result = ref [] in
  let add t =
    if not (Hashtbl.mem seen t) then (Hashtbl.add seen t (); result := t :: !result)
  in
  List.iter (fun (_, arity, sym) ->
    if arity = 0 then ()  (* Only at size 1, handled in the base case. *)
    else
    List.iter (fun part ->
      let opts_per_arg = List.map options_for part in
      if List.for_all ((<>) []) opts_per_arg then
      List.iter (fun choices ->
        let next_v = ref 0 and next_h = ref 0 in
        let args = List.map (function
          | FreshVarSlot ->
            let id = !next_v in incr next_v; Types.Var id
          | FreshHoleSlot ->
            let id = !next_h in incr next_h; Types.Hole id
          | IrredCopy (skel, nv, nh) ->
            let v_shift = !next_v in next_v := !next_v + nv;
            let h_shift = !next_h in next_h := !next_h + nh;
            shift_slots ~v_shift ~h_shift skel)
          choices
        in let term = Types.Node (sym, args) in
        let total_v = !next_v and total_h = !next_h in
        (* Cap total slot count to keep set_partitions tractable. *)
        if total_v + total_h <= 6 then
        List.iter (fun v_part ->
          List.iter (fun h_part ->
            match apply_partitions ~v_part ~h_part ~caps
                    ~max_v_slots:total_v ~max_h_slots:total_h term with
            | Some canon ->
              (* `apply_partitions` produces ONE first-occurrence
                 labeling per partition (e.g. `A*(B*A)` for the
                 0/1/0 slot pattern). But under constP semantics
                 each labeling is a distinct canonical term — the
                 other labelings of the same partition shape
                 (e.g. `B*(A*B)`) are real terms in the equivalence
                 class. Expand to the hole-id permutation orbit so
                 every labeling becomes a candidate; bv-bucketing
                 then groups commutative-equivalent ones and the
                 winner-extraction emits the appropriate rules. *)
              List.iter add (Types.hole_permutations canon)
            | None -> ())
            (set_partitions_n total_h))
          (set_partitions_n total_v))
        (product opts_per_arg))
      (partitions (n - 1) arity))
    symbols;
  !result

(* Convenience wrapper: a single combined cap `k` used as max_vars,
   max_holes, AND max_vcs (the proof's setting). *)
let enumerate_terms (symbols : (string * int * 's) list)
      (irreducible : 's Types.term list) (n : int) (k : int) : 's Types.term list =
  enumerate_terms_caps symbols irreducible n
    { max_vars = k; max_holes = k; max_vcs = k }
