(* Substitution-based bottom-up rewriting with a discrimination-tree index.

   A rule (l, r) fires on a target t when ∃σ. apply_subst σ l = t, replacing t
   with apply_subst σ r. Rules are required by construction to satisfy
   r ≺ₖ l (partial KBO), which keeps rewriting size-non-increasing under any
   substitution and hence terminating.

   The index is a discrimination tree: patterns are linearised in pre-order to
   token sequences (`Sym(f, arity)` for nodes, `TVar` for variables) and
   inserted into a trie. Lookup walks the trie against a target by exploring
   both the symbol-matching edge (when the target's current subterm is the
   right node) and the wildcard edge (skipping the entire current subterm).
   Common prefixes among rule LHSs are shared, replacing the previous linear
   scan over each `(symbol, arity)` bucket. The DT can return false-positives
   (variable consistency isn't tracked across the tree), so the caller still
   confirms each candidate with `match_subst`. *)

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

type 's rule_index = 's dt_node

let index_rules rules =
  let root = make_node () in
  List.iter (fun ((lhs, _) as rule) ->
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
  ) rules; root

(* Walk the DT against a target term and try each candidate rule, returning
   the first successful rewrite. Implemented as a manual stack-based DFS so
   we can short-circuit on the first match. *)
let try_rewrite root target =
  let rec walk n work =
    let try_rules () =
      match List.find_map (fun (lhs, rhs) ->
        match Types.match_subst lhs target with
        | Some m -> Some (Types.apply_subst m rhs)
        | None -> None) n.rules with
      | Some _ as r -> r
      | None -> None
    in
    match work with
    | [] -> try_rules ()
    | t :: rest ->
      (* Prefer Sym edge first: it's the more specific match. *)
      let by_sym = match t with
        | Types.Node (f, args) ->
          (match Hashtbl.find_opt n.sym_children (f, List.length args) with
           | Some c -> walk c (args @ rest)
           | None -> None)
        | Types.Var _ | Types.Hole _ -> None
      in
      match by_sym with
      | Some _ as r -> r
      | None ->
        match n.var_child with
        | Some c ->
          let r = walk c rest in
          if r <> None then r else try_rules ()
        | None -> try_rules ()
  in walk root [target]

let rec norm_bottom ~index t = match t with
  | Types.Var _ | Types.Hole _ -> t
  | Types.Node (f, args) ->
    let args' = List.map (norm_bottom ~index) args in
    let t' = Types.Node (f, args') in
    (match try_rewrite index t' with
     | None -> t'
     | Some t'' -> norm_bottom ~index t'')

let normalize ~index t =
  let sz0 = Types.size t in
  let r = Types.canonicalize (norm_bottom ~index t) in
  (r, Types.size r < sz0)

let normalize_with_index rules t = normalize ~index:(index_rules rules) t

(* Backwards-compatible single-rule helper used by tests. *)
let rewrite_at_root (lhs, rhs) t =
  match Types.match_subst lhs t with
  | Some m -> Some (Types.apply_subst m rhs) | None -> None
