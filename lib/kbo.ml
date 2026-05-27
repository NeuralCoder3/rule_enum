(* Knuth-Bendix-style ordering, partial on terms with variables.

   Classical KBO with unit weight = `Types.size` and symbol precedence =
   `sym_cmp`. Substitution-monotone.

   Definition (s ≺ₖ t):
   * var_counts(s) ≤ var_counts(t) pointwise, AND
   * one of:
     - size(s) < size(t), or
     - size(s) = size(t) AND
       * s and t are both Vars and equal, or
       * s and t are both Holes ordered by id (NoVar — KBO total), or
       * s = Node(f, ss), t = Node(g, ts) with f ≠ g and f ≺ g in
         sym_cmp (precedence — substitution-stable since heads are
         fixed), or
       * s = Node(f, ss), t = Node(f, ts) (same head), and at the first
         differing arg position i, ss[i] ≺ₖ ts[i] (recursive KBO; each
         recursion re-checks var_counts).

   The recursive lex into args uses KBO itself (not pure-syntactic
   size/lex), so each step is substitution-stable on its own. Var-vs-Node
   at a recursive arg position fails the var_counts gate and yields
   Incomparable — the case that caused the `(A+A)*a → a*(A+A)`
   infinite-loop bug. *)

type result = Less | Equal | Greater | Incomparable

let flip = function Less -> Greater | Greater -> Less | r -> r

let rec kbo sym_cmp t1 t2 =
  let s1 = Types.size t1 and s2 = Types.size t2 in
  let ca = Types.var_counts t1 and cb = Types.var_counts t2 in
  let a_le_b = Types.var_counts_le ca cb in
  let b_le_a = Types.var_counts_le cb ca in
  if s1 < s2 then (if a_le_b then Less else Incomparable)
  else if s1 > s2 then (if b_le_a then Greater else Incomparable)
  else (* same size *)
    match t1, t2 with
    | Types.Var v1, Types.Var v2 ->
      if v1 = v2 then Equal else Incomparable
    | Types.Hole n1, Types.Hole n2 ->
      (* NoVar — KBO total on holes via id precedence. *)
      if n1 = n2 then Equal
      else if n1 < n2 then Less else Greater
    | Types.Var _, (Types.Hole _ | Types.Node _) -> Incomparable
    | Types.Hole _, (Types.Var _ | Types.Node _) -> Incomparable
    | Types.Node _, (Types.Var _ | Types.Hole _) -> Incomparable
    | Types.Node (f1, args1), Types.Node (f2, args2) ->
      let arity_ok = List.length args1 = List.length args2 in
      match sym_cmp f1 f2 with
      | 0 when arity_ok && a_le_b && b_le_a ->
        (* Same head, var-counts match — lex via recursive KBO. *)
        kbo_lex sym_cmp args1 args2
      | 0 -> Incomparable  (* arity mismatch or var-count mismatch *)
      | c when c < 0 ->
        (* head(t1) ≺ head(t2) in precedence — t1 ≺ t2 if vars permit. *)
        if a_le_b then Less else Incomparable
      | _ ->
        if b_le_a then Greater else Incomparable

and kbo_lex sym_cmp a b = match a, b with
  | [], [] -> Equal
  | x :: xs, y :: ys ->
    (match kbo sym_cmp x y with
     | Equal -> kbo_lex sym_cmp xs ys
     | r -> r)
  | _ -> Incomparable

let lt sym_cmp a b = kbo sym_cmp a b = Less

(* Cached KBO inputs: (term, size, var_counts_arr). Avoids re-walking each
   term per pair-comparison in O(n²) loops like KBO-minimal extraction. *)
type 'a cached = 'a Types.term * int * Types.var_counts_arr

let cache t : 'a cached = (t, Types.size t, Types.var_counts_arr t)

let kbo_cached sym_cmp (t1, s1, ca) (t2, s2, cb) =
  let a_le_b = Types.var_counts_arr_le ca cb in
  let b_le_a = Types.var_counts_arr_le cb ca in
  if s1 < s2 then (if a_le_b then Less else Incomparable)
  else if s1 > s2 then (if b_le_a then Greater else Incomparable)
  else
    match t1, t2 with
    | Types.Var v1, Types.Var v2 ->
      if v1 = v2 then Equal else Incomparable
    | Types.Hole n1, Types.Hole n2 ->
      if n1 = n2 then Equal
      else if n1 < n2 then Less else Greater
    | Types.Var _, (Types.Hole _ | Types.Node _) -> Incomparable
    | Types.Hole _, (Types.Var _ | Types.Node _) -> Incomparable
    | Types.Node _, (Types.Var _ | Types.Hole _) -> Incomparable
    | Types.Node (f1, args1), Types.Node (f2, args2) ->
      let arity_ok = List.length args1 = List.length args2 in
      match sym_cmp f1 f2 with
      | 0 when arity_ok && a_le_b && b_le_a ->
        (* Same head: lex via recursive KBO. Args sub-terms — fall back to
           uncached recursion (their cached metadata isn't precomputed). *)
        kbo_lex sym_cmp args1 args2
      | 0 -> Incomparable
      | c when c < 0 -> if a_le_b then Less else Incomparable
      | _ -> if b_le_a then Greater else Incomparable

let lt_cached sym_cmp a b = kbo_cached sym_cmp a b = Less

(* Total syntactic order used only as a deterministic tiebreaker — NOT ≺ₖ. *)
let compare_total = Types.term_compare

let minimum sym_cmp = function
  | [] -> failwith "Kbo.minimum: empty list"
  | t :: rest ->
    List.fold_left (fun best t ->
      match kbo sym_cmp t best with
      | Less -> t
      | Greater | Equal -> best
      | Incomparable ->
        if compare_total sym_cmp t best < 0 then t else best)
      t rest
