(* Substitution-based bottom-up rewriting with discrimination-tree indexes.

   Two indexes are maintained:
   * `hole_index` — rules whose LHS contains at least one `Hole` (constP).
     Matched with `Types.match_var_const`: vars get general substitution,
     holes get order-preserving size-0 rename (image is `Hole _` or a
     0-arity `Node`).
   * `var_index` — rules whose LHS has no `Hole`. Matched with the cheap
     `Types.match_subst` (var-only general substitution).

   At each node visited bottom-up by `norm_bottom`, hole rules are tried
   first (more selective; faster to fail); on miss, var rules are tried.
   First match wins; the rewritten subterm normalizes to fixed point.

   Hole rules are only KBO-decreasing when their canonical orientation
   is matched — the order-preservation constraint in `match_var_const`
   enforces this. *)

type 's tok = TSym of 's * int | TVar

let rec linearize_into acc = function
  | Types.Var _ | Types.Hole _ -> TVar :: acc
  | Types.Node (f, args) ->
    let acc = TSym (f, List.length args) :: acc in
    List.fold_left linearize_into acc args

let linearize t = List.rev (linearize_into [] t)

type 's dt_node = {
  mutable rules : 's Types.rule list;
  sym_children : ('s * int, 's dt_node) Hashtbl.t;
  mutable var_child : 's dt_node option;
}

let make_node () = { rules = []; sym_children = Hashtbl.create 4; var_child = None }

type 's rule_index = {
  hole_root : 's dt_node;
  var_root  : 's dt_node;
}

let insert_into root ((lhs, _) as rule) =
  match lhs with
  | Types.Node _ ->
    let n = List.fold_left (fun n tok ->
      match tok with
      | TSym (f, k) ->
        (match Hashtbl.find_opt n.sym_children (f, k) with
         | Some c -> c
         | None -> let c = make_node () in Hashtbl.add n.sym_children (f, k) c; c)
      | TVar ->
        (match n.var_child with
         | Some c -> c
         | None -> let c = make_node () in n.var_child <- Some c; c))
      root (linearize lhs) in
    n.rules <- rule :: n.rules
  | Types.Var _ | Types.Hole _ -> ()

let index_rules rules =
  let hole_root = make_node () and var_root = make_node () in
  List.iter (fun ((lhs, _) as rule) ->
    if Types.has_hole lhs then insert_into hole_root rule
    else insert_into var_root rule) rules;
  { hole_root; var_root }

(* Walk a DT against the target; return the first rule that confirms.

   Work is represented as a `term list list` — a stack of sibling groups
   instead of one flat list, so descending into a Node's args just
   prepends a fresh group rather than concatenating with `args @ rest`.
   This eliminates list allocation in the hot DT-walk path. *)
let walk_dt ~confirm_rewrite root target =
  let rec walk n work =
    let try_rules () = List.find_map (fun rule -> confirm_rewrite rule target) n.rules in
    match work with
    | [] -> try_rules ()
    | [] :: rest -> walk n rest
    | (t :: ts) :: rest ->
      let work' = ts :: rest in
      let by_sym = match t with
        | Types.Node (f, args) ->
          (match Hashtbl.find_opt n.sym_children (f, List.length args) with
           | Some c -> walk c (args :: work')
           | None -> None)
        | Types.Var _ | Types.Hole _ -> None
      in
      match by_sym with
      | Some _ as r -> r
      | None ->
        match n.var_child with
        | Some c ->
          let r = walk c work' in
          if r <> None then r else try_rules ()
        | None -> try_rules ()
  in walk root [[target]]

let try_rewrite sym_cmp idx target =
  let confirm_hole (lhs, rhs) tgt =
    match Types.match_var_const sym_cmp lhs tgt with
    | Some (vmap, hmap) -> Some (Types.apply_var_const vmap hmap rhs)
    | None -> None
  in
  let confirm_var (lhs, rhs) tgt =
    match Types.match_subst lhs tgt with
    | Some m -> Some (Types.apply_subst m rhs)
    | None -> None
  in
  match walk_dt ~confirm_rewrite:confirm_hole idx.hole_root target with
  | Some _ as r -> r
  | None -> walk_dt ~confirm_rewrite:confirm_var idx.var_root target

(* Returns (term, changed?). `changed=false` lets normalize skip the
   final canonicalize pass when the term wasn't rewritten — enumerated
   terms are already canonical. *)
let rec norm_bottom_tracked ~sym_cmp ~index t = match t with
  | Types.Var _ | Types.Hole _ -> (t, false)
  | Types.Node (f, args) ->
    let any_changed = ref false in
    let args' = List.map (fun a ->
      let (a', c) = norm_bottom_tracked ~sym_cmp ~index a in
      if c then any_changed := true; a') args in
    let t' = Types.mk_node f args' in
    (match try_rewrite sym_cmp index t' with
     | None -> (t', !any_changed)
     | Some t'' ->
       let (t''', _) = norm_bottom_tracked ~sym_cmp ~index t'' in
       (t''', true))

let norm_bottom ~sym_cmp ~index t =
  fst (norm_bottom_tracked ~sym_cmp ~index t)

let normalize ~sym_cmp ~index t =
  let sz0 = Types.size t in
  let r = Types.canonicalize (norm_bottom ~sym_cmp ~index t) in
  (r, Types.size r < sz0)

(* Hot-path variant: caller guarantees `t` is already canonical (e.g.,
   produced by `Enum.enumerate_terms_caps`). If no rewrite fires, we
   return the term as-is without re-walking it through canonicalize. *)
let normalize_canonical ~sym_cmp ~index t =
  let sz0 = Types.size t in
  let (r, changed) = norm_bottom_tracked ~sym_cmp ~index t in
  let r = if changed then Types.canonicalize r else r in
  (r, Types.size r < sz0)

(* Hot path for process_term:
   * Returns `None` as soon as ANY recursion level shrinks (input size at
     that level > output size). Subterm shrinkage propagates up because
     `size` is additive over `Node`.
   * Returns `Some (simplified, changed)` if the term doesn't size-reduce.

   This avoids the leftover rewrite work and the canonicalize pass for
   the ~67% of enumerated terms that reduce (the common case). *)
exception Size_reduced

let normalize_canonical_or_skip ~sym_cmp ~index t =
  let rec go t =
    match t with
    | Types.Var _ | Types.Hole _ -> (t, false)
    | Types.Node (f, args) ->
      let in_sz = Types.size t in
      let any_changed = ref false in
      let args' = List.map (fun a ->
        let (a', c) = go a in
        if c then any_changed := true; a') args in
      let t' = Types.mk_node f args' in
      if Types.size t' < in_sz then raise Size_reduced;
      match try_rewrite sym_cmp index t' with
      | None -> (t', !any_changed)
      | Some t'' ->
        if Types.size t'' < in_sz then raise Size_reduced;
        let (t''', _) = go t'' in
        if Types.size t''' < in_sz then raise Size_reduced;
        (t''', true)
  in
  try
    let (r, changed) = go t in
    let r = if changed then Types.canonicalize r else r in
    Some (r, changed)
  with Size_reduced -> None

let normalize_with_index ~sym_cmp rules t = normalize ~sym_cmp ~index:(index_rules rules) t

(* Backwards-compatible single-rule helpers used by tests. *)
let rewrite_at_root sym_cmp (lhs, rhs) t =
  if Types.has_hole lhs then
    match Types.match_var_const sym_cmp lhs t with
    | Some (vmap, hmap) -> Some (Types.apply_var_const vmap hmap rhs)
    | None -> None
  else
    match Types.match_subst lhs t with
    | Some m -> Some (Types.apply_subst m rhs)
    | None -> None
