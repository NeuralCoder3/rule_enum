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

(* AC-bucket entry with data precomputed once at index time so the hot
   `ac_try_rewrite` path does no per-rule flattening or allocation:
   - `ae_ps`      : the LHS operands flattened w.r.t. the (AC) head.
   - `ae_n_pat`   : |ae_ps| — the length precheck for AC extension.
   - `ae_req`     : sorted multiset of (symbol, arity) over the ae_ps
                    operands that are Nodes of arity>0. Any such pattern
                    operand can ONLY match a target operand headed by the
                    same symbol (ac_match_full's Node/Node case), and the
                    match is a bijection to distinct target operands — so a
                    rule can fire only if ae_req is a sub-multiset of the
                    target operands' symbols. A cheap, sound prefilter. *)
type 's ac_entry = {
  ae_rule  : 's Types.rule;
  ae_ps    : 's Types.term list;
  ae_n_pat : int;
  ae_req   : ('s * int) list;
}

(* A head-symbol bucket for AC rewriting: entries grouped by their `ae_req`
   (required operand-symbol multiset). Most rules share a `ae_req` (e.g.
   `[(|,2)]`, `[(~,1)]`), so the sub-multiset prefilter runs ONCE per group
   rather than once per rule — turning the per-node scan from O(#rules) into
   O(#distinct req) prefilter checks, expanding to entry matching only for
   groups whose requirement the target actually satisfies. *)
type 's ac_bucket = {
  ab_groups : (('s * int) list * 's ac_entry list) list;
  ab_all    : 's ac_entry list;   (* flat list, for non-AC heads *)
}

type 's rule_index = {
  hole_root : 's dt_node;
  var_root  : 's dt_node;
  (* AC mode: rules bucketed by LHS head symbol (LHS is always a Node when
     indexed). Consulted only when `Types.ac_enabled ()`. *)
  ac_buckets : ('s, 's ac_bucket) Hashtbl.t;
}

(* Sorted (symbol, arity) multiset of the arity>0 Node operands in `ops`. *)
let node_syms sym_cmp ops =
  let kcmp (g1, a1) (g2, a2) = let c = sym_cmp g1 g2 in if c <> 0 then c else compare a1 a2 in
  List.sort kcmp
    (List.filter_map (function
       | Types.Node (g, (_ :: _ as args)) -> Some (g, List.length args)
       | _ -> None) ops)

(* Is sorted multiset `req` contained in sorted multiset `have`? *)
let rec submultiset sym_cmp req have =
  match req, have with
  | [], _ -> true
  | _ :: _, [] -> false
  | (g1, a1) :: rq, (g2, a2) :: hv ->
    let c = let c = sym_cmp g1 g2 in if c <> 0 then c else compare a1 a2 in
    if c = 0 then submultiset sym_cmp rq hv          (* matched, consume both *)
    else if c > 0 then submultiset sym_cmp req hv    (* skip smaller have elt *)
    else false                                       (* req elt missing *)

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
  (* Precompute AC-entry data with the run's AC config (None ⇒ AC off, the
     bucket is never consulted, so trivial entries are fine). *)
  let acinfo = Types.ac_get () in
  let raw = Hashtbl.create 16 in   (* head -> entry list (reverse insertion) *)
  List.iter (fun ((lhs, _) as rule) ->
    if Types.has_hole lhs then insert_into hole_root rule
    else insert_into var_root rule;
    match lhs with
    | Types.Node (f, _) ->
      let ps = match acinfo with
        | Some (is_ac, cmp) when is_ac f -> Types.ac_flatten is_ac cmp f lhs
        | _ -> [lhs] in
      let req = match acinfo with
        | Some (_, cmp) -> node_syms cmp ps
        | None -> [] in
      (* Match the MOST CONSTRAINED operands first. `ac_flatten` yields ps in
         `term_compare` order, which puts Var/Hole leaves (match anything)
         BEFORE Node operands (match only a same-head target) — so the naive
         order binds wildcards first and only discovers a structural mismatch
         after descending. Reordering Node operands to the front makes a
         non-matching rule fail on the first assignment. `ac_match_ops` finds
         a bijection regardless of operand order, so existence is unchanged;
         the first substitution found can differ, but every match is sound
         (confluence is re-verified by the AC test). *)
      let ps =
        let nodes, leaves = List.partition (function
          | Types.Node (_, _ :: _) -> true | _ -> false) ps in
        nodes @ leaves in
      let entry = { ae_rule = rule; ae_ps = ps; ae_n_pat = List.length ps; ae_req = req } in
      let cur = try Hashtbl.find raw f with Not_found -> [] in
      Hashtbl.replace raw f (entry :: cur)
    | _ -> ()) rules;
  let ac_buckets = Hashtbl.create 16 in
  Hashtbl.iter (fun f entries_rev ->
    let entries = List.rev entries_rev in
    (* group entries by ae_req, preserving first-seen group order *)
    let tbl = Hashtbl.create 16 and order = ref [] in
    List.iter (fun e ->
      if not (Hashtbl.mem tbl e.ae_req) then order := e.ae_req :: !order;
      Hashtbl.replace tbl e.ae_req (e :: (try Hashtbl.find tbl e.ae_req with Not_found -> [])))
      entries;
    let groups = List.rev_map (fun req -> (req, List.rev (Hashtbl.find tbl req))) !order in
    Hashtbl.replace ac_buckets f { ab_groups = groups; ab_all = entries })
    raw;
  { hole_root; var_root; ac_buckets }

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

(* AC rewrite at a single node: try every rule whose LHS head matches the
   target head, matching modulo AC (`Types.ac_match_root`). On an AC head
   the match may cover a sub-multiset of the operands; the unmatched
   operands (`leftover`) are re-joined with the substituted RHS. First
   match wins. *)
(* Termination gate for AC rewriting. Modulo AC, the `match_var_const`
   orientation guard that (in the DT path) stops a same-size rule from
   firing on a variable target is gone — so a same-size (KR) rule could
   otherwise ping-pong a term through its AC-equal forms forever. We keep
   the AC rewrite relation inside a well-founded order: accept a rewrite
   only when its (AC-normalised) result is STRICTLY smaller than the
   target by (size, then the canonical `term_compare` total order). Every
   sound rule here is size-non-increasing and, when same-size, oriented
   toward the canonical rep, so this never blocks a needed reduction but
   guarantees normalization terminates. *)
(* Termination gate: a rewrite is accepted only if its result strictly
   decreases (size, then the canonical `term_compare`). PRECONDITION:
   `target` is AC-canonical — every caller passes a `build_node`-built node
   or a term canonicalized once up front, so the same-size tie-break can
   compare against it directly. `size` is invariant under `ac_normalize`, so
   the common strict-size case needs no normalization at all; only the
   same-size (KR) tie-break normalizes `res`. *)
let ac_decreases is_ac sym_cmp target res =
  let ds = compare (Types.size res) (Types.size target) in
  if ds <> 0 then ds < 0
  else Types.term_compare sym_cmp (Types.ac_normalize is_ac sym_cmp res) target < 0

let ac_try_rewrite is_ac sym_cmp buckets target =
  match target with
  | Types.Node (f, _) ->
    (match Hashtbl.find_opt buckets f with
     | None -> None
     | Some bucket ->
       let accept res =
         if ac_decreases is_ac sym_cmp target res then Some res else None in
       let build (_, rhs) st leftover = match leftover with
         | [] -> accept (Types.apply_ac_subst st rhs)
         | _ ->
           let rhs' = Types.apply_ac_subst st rhs in
           let ops = Types.ac_flatten is_ac sym_cmp f rhs' @ leftover in
           accept (Types.ac_join sym_cmp f ops)
       in
       if is_ac f then
         (* Flatten the target ONCE. Check each requirement GROUP's
            sub-multiset prefilter once; only for a satisfied group do we
            attempt its entries (length check + backtracking match). *)
         let ts = Types.ac_flatten is_ac sym_cmp f target in
         let n_tgt = List.length ts in
         let tgt_syms = node_syms sym_cmp ts in
         List.find_map (fun (req, es) ->
           if not (submultiset sym_cmp req tgt_syms) then None
           else List.find_map (fun e ->
             if e.ae_n_pat > n_tgt then None
             else match Types.ac_match_ops is_ac sym_cmp Types.empty_subst e.ae_ps ts with
               | None -> None
               | Some (st, leftover) -> build e.ae_rule st leftover) es)
           bucket.ab_groups
       else
         (* Non-AC head: match the direct arguments (small bucket). *)
         List.find_map (fun e ->
           match Types.ac_match_root is_ac sym_cmp (fst e.ae_rule) target with
           | None -> None
           | Some (st, leftover) -> build e.ae_rule st leftover)
           bucket.ab_all)
  | _ -> None

let try_rewrite sym_cmp idx target =
  match Types.ac_get () with
  | Some (is_ac, cmp) -> ac_try_rewrite is_ac cmp idx.ac_buckets target
  | None ->
  let confirm_hole (lhs, rhs) tgt =
    (* A same-size (commutativity) hole rule must not bind a target var:
       that would be a non-KBO reorientation. Only strictly size-reducing
       hole rules may image a hole onto a var (always KBO-decreasing). *)
    let allow_var_image = Types.size lhs > Types.size rhs in
    match Types.match_var_const ~allow_var_image sym_cmp lhs tgt with
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

(* Rebuild a node, AC-normalising it when AC mode is on (keeps the term in
   canonical AC form between rewrite steps). *)
let build_node f args = match Types.ac_get () with
  | Some (is_ac, cmp) -> Types.ac_build is_ac cmp f args
  | None -> Types.mk_node f args

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
    let t' = build_node f args' in
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

(* Hot path for process_term: decide whether an enumerated term is an
   irreducible candidate. Returns `None` (skip) as soon as ANY rule fires
   anywhere in the term — at a subterm or the root, whether it shrinks or
   keeps the size. A reducible term is never irreducible, so it is dropped
   without finishing the rewrite. Returns `Some t` (t unchanged, already
   canonical from enumeration) only when no rule applies.

   Why dropping same-size rewrites is safe: the enumerator builds size-n
   terms from the listed canonical irreducibles, and every term that is
   irreducible w.r.t. the current rules is a listed canonical rep (each
   completed iteration orients all same-size equivalences into rules —
   this relies on `vars_to_holes` keeping distinct leaves distinct so
   mixed var/hole equivalences like a*(b*A) ≡ A*(a*b) are orientable). So
   the normal form of a reducible term is itself enumerated — in this
   iteration if same-size, an earlier one if smaller — and processed on
   its own. Verified by the exhaustive soundness+confluence tests.

   Set RULE_ENUM_NO_SKIP=1 to fall back to the conservative behavior
   (skip only on size reduction, process same-size normal forms). *)
exception Reducible

let no_skip = try Sys.getenv "RULE_ENUM_NO_SKIP" = "1" with Not_found -> false

(* Memo for the reducibility check across shared subterms. Irreducibility is
   COMPOSITIONAL — a term is irreducible iff every child is irreducible AND no
   rule fires at the (canonical) root — so a subterm's verdict depends only on
   the subterm and the (fixed, per-subpass) rule index. Enumerated terms are
   built from a shared pool of irreducibles, so the same subterms recur
   constantly; caching `subterm -> verdict` skips re-scanning known subtrees.

   NB we cannot statically assume an operand is irreducible even though it is
   a copy of a base irreducible: the enumerator's `apply_partitions` step
   merges distinct leaf slots (e.g. `f(x,y)` with x≠y can become `f(x,x)`,
   which idempotence now reduces), so an operand can become reducible after
   leaf identification. The memo therefore still CHECKS each subterm against
   the current rules — it only avoids REDOING that check for a repeat. A memo
   is valid for one rule index, so callers make a fresh one per subpass (and
   per chunk, to stay race-free across workers). *)
type 's check_res = Irr of 's Types.term | Red
type 's memo = ('s Types.term, 's check_res) Hashtbl.t
let make_memo () : 's memo = Hashtbl.create 4096

let normalize_canonical_or_skip_plain ~sym_cmp ~index t =
  if no_skip then begin
    (* Conservative: skip only on strict size reduction; for same-size
       rewrites return the fully-normalized form for `decide`. *)
    let rec go t = match t with
      | Types.Var _ | Types.Hole _ -> (t, false)
      | Types.Node (f, args) ->
        let in_sz = Types.size t in
        let changed = ref false in
        let args' = List.map (fun a -> let (a', c) = go a in if c then changed := true; a') args in
        let t' = build_node f args' in
        if Types.size t' < in_sz then raise Reducible;
        (match try_rewrite sym_cmp index t' with
         | None -> (t', !changed)
         | Some t'' ->
           if Types.size t'' < in_sz then raise Reducible;
           let (t''', _) = go t'' in
           if Types.size t''' < in_sz then raise Reducible;
           (t''', true))
    in
    (try let (r, c) = go t in Some (if c then Types.canonicalize r else r)
     with Reducible -> None)
  end else
    (* `go` raises `Reducible` at the first rule firing (in a subterm via
       recursion, or at the current node); if it returns, the term is
       irreducible. `build_node` keeps every node AC-canonical as it is
       rebuilt bottom-up, which the termination gate relies on (it compares a
       rewrite's canonical result against the — canonical — target for the
       same-size tie-break; a raw enumerator term is only rename-canonical,
       not AC-canonical). *)
    let rec go t = match t with
      | Types.Var _ | Types.Hole _ -> t
      | Types.Node (f, args) ->
        let t' = build_node f (List.map go args) in
        (match try_rewrite sym_cmp index t' with
         | None -> t'
         | Some _ -> raise Reducible)
    in
    try Some (go t) with Reducible -> None

(* Memoized dispatcher: with a memo (and outside no_skip debug mode), use the
   compositional cache; otherwise fall back to the plain single-term walk.
   `check` returns `Irr canonical` or `Red`, short-circuiting on the first
   reducible child — the same verdict as the plain path (which raises at the
   first redex) — and the cached `Irr` value is the `build_node`-canonical
   form the plain path returns. *)
let normalize_canonical_or_skip ?memo ~sym_cmp ~index t =
  match memo with
  | Some memo when not no_skip ->
    let rec check t = match t with
      | Types.Var _ | Types.Hole _ -> Irr t
      | Types.Node (f, args) ->
        (match Hashtbl.find_opt memo t with
         | Some r -> r
         | None ->
           let rec conv acc = function
             | [] -> Some (List.rev acc)
             | a :: rest ->
               (match check a with Irr c -> conv (c :: acc) rest | Red -> None) in
           let r = match conv [] args with
             | None -> Red
             | Some cargs ->
               let t' = build_node f cargs in
               (match try_rewrite sym_cmp index t' with None -> Irr t' | Some _ -> Red) in
           Hashtbl.replace memo t r; r)
    in
    (match check t with Irr ct -> Some ct | Red -> None)
  | _ -> normalize_canonical_or_skip_plain ~sym_cmp ~index t

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
