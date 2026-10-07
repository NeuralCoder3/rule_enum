(* Knuth-Bendix order with unit weights; Ucs are nullary symbols below all
   others. Below a common AC symbol only ground terms are ordered (by the
   AC-KBO), as substitution may change their operands. *)
type order = Less | Equal | Greater | Unordered

module IntMap = Map.Make (Int)

let var_counts t =
  List.fold_left
    (fun m -> function
      | Term.Var i -> IntMap.update i (fun c -> Some (1 + Option.value c ~default:0)) m
      | _ -> m)
    IntMap.empty (Term.leaves t)

let dominates a b =
  IntMap.for_all (fun x n -> Option.value (IntMap.find_opt x a) ~default:0 >= n) b

let head_compare (syms : _ Term.symbols) s t =
  match s, t with
  | Term.Uc i, Term.Uc j -> Int.compare i j
  | Term.Uc _, _ -> -1
  | _, Term.Uc _ -> 1
  | Term.App (f, _), Term.App (g, _) -> syms.precedence f g
  | _ -> 0

let args = function Term.App (_, ts) -> ts | _ -> []

let rec compare syms s t =
  if s = t then Equal
  else
    let vs = var_counts s and vt = var_counts t in
    let orient c =
      if c > 0 && dominates vs vt then Greater
      else if c < 0 && dominates vt vs then Less
      else Unordered
    in
    match Int.compare (Term.size s) (Term.size t), s, t with
    | 0, Term.Var _, _ | 0, _, Term.Var _ -> Unordered
    | 0, Term.App (f, _), _ when syms.Term.ac f && head_compare syms s t = 0 ->
        if IntMap.is_empty vs && IntMap.is_empty vt then orient (Term.compare syms s t) else Unordered
    | 0, _, _ -> (
        match head_compare syms s t with
        | 0 -> orient (lex syms (args s) (args t))
        | c -> orient c)
    | c, _, _ -> orient c

and lex syms ss ts =
  match ss, ts with
  | s :: ss, t :: ts -> (
      match compare syms s t with
      | Equal -> lex syms ss ts
      | Greater -> 1
      | Less -> -1
      | Unordered -> 0)
  | _ -> 0

let greater syms s t = compare syms s t = Greater
