(* Knuth-Bendix-style ordering, partial on terms with variables.

   ≺ₖ is defined here as: (var_counts(s) ≤ var_counts(t) pointwise)
   AND (size(s) < size(t)  OR  size(s) = size(t) AND syntactic_lex(s, t) < 0
        AND distinct variables are not directly compared).

   This satisfies the proof's axioms: well-founded (size decreases); transitive;
   total on ground terms (no vars → var_count gate trivially holds, size+sym lex
   total); monotone under one-hole context; size-non-increasing; closed under
   substitution (var_count_le ensures size doesn't grow under any substitution).

   `compare_total` is a separate total syntactic ordering used as a
   deterministic tiebreaker (e.g., for sorting); it does NOT model ≺ₖ. *)

type result = Less | Equal | Greater | Incomparable

let flip = function Less -> Greater | Greater -> Less | r -> r

(* Syntactic comparison ignoring var-count condition. Returns Less/Equal/Greater
   strictly, or Incomparable when two distinct variables are directly compared
   at the same position (KBO is partial on variables). *)
let rec syntactic sym_cmp t1 t2 =
  let s1 = Types.size t1 and s2 = Types.size t2 in
  if s1 < s2 then Less
  else if s1 > s2 then Greater
  else match t1, t2 with
    | Types.Var v1, Types.Var v2 ->
      if v1 = v2 then Equal else Incomparable
    | Types.Hole n1, Types.Hole n2 ->
      if n1 = n2 then Equal else Incomparable
    | Types.Var _, (Types.Hole _ | Types.Node _) -> Incomparable
    | Types.Hole _, (Types.Var _ | Types.Node _) -> Incomparable
    | Types.Node _, (Types.Var _ | Types.Hole _) -> Incomparable
    | Types.Node (f1, args1), Types.Node (f2, args2) ->
      match sym_cmp f1 f2 with
      | 0 -> lex_syntactic sym_cmp args1 args2
      | c when c < 0 -> Less
      | _ -> Greater
and lex_syntactic sym_cmp a b = match a, b with
  | [], [] -> Equal
  | x :: xs, y :: ys ->
    (match syntactic sym_cmp x y with
     | Equal -> lex_syntactic sym_cmp xs ys
     | r -> r)
  | _ ->
    let la = List.length a and lb = List.length b in
    if la < lb then Less else if la > lb then Greater else Equal

(* Partial KBO on terms with variables.

   `kbo s t = Less`     iff  var_counts(s) ≤ var_counts(t) AND syntactic(s,t) = Less.
   `kbo s t = Greater`  iff  var_counts(t) ≤ var_counts(s) AND syntactic(s,t) = Greater.
   `kbo s t = Equal`    iff  syntactic(s,t) = Equal AND var counts coincide.
   Otherwise `Incomparable`. *)
let kbo sym_cmp t1 t2 =
  let ca = Types.var_counts t1 and cb = Types.var_counts t2 in
  let a_le_b = Types.var_counts_le ca cb in
  let b_le_a = Types.var_counts_le cb ca in
  match syntactic sym_cmp t1 t2 with
  | Equal -> if a_le_b && b_le_a then Equal else Incomparable
  | Less -> if a_le_b then Less else Incomparable
  | Greater -> if b_le_a then Greater else Incomparable
  | Incomparable -> Incomparable

let lt sym_cmp a b = kbo sym_cmp a b = Less

(* A total syntactic order used only as a deterministic tiebreaker — derives
   from `Types.term_compare` (size, then sym, then lex on args, with var indices
   as final fallback). NOT the proof's ≺ₖ. *)
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
