type 'f rule = 'f Term.t * 'f Term.t
type 'f t = ('f, 'f rule) Index.t
type 'f binding = { vars : (int * 'f Term.t) list; ucs : (int * int) list }

let empty = Index.empty
let add (syms : _ Term.symbols) ((lhs, _) as rule) index = Index.add syms.ac lhs rule index
let of_list syms rules = List.fold_left (fun index rule -> add syms rule index) empty rules

let rec picks = function
  | [] -> []
  | x :: xs -> (x, xs) :: List.map (fun (y, ys) -> (y, x :: ys)) (picks xs)

(* Variables bind subterms, Ucs bind Ucs by a strictly monotone map, and AC
   operands bind operands one-to-one; [k] continues with the binding and
   backtracks on [None]. *)
let rec bind (syms : _ Term.symbols) p t b k =
  match p, t with
  | Term.Var x, _ -> (
      match List.assoc_opt x b.vars with
      | Some s -> if s = t then k b else None
      | None -> k { b with vars = (x, t) :: b.vars })
  | Term.Uc a, Term.Uc c -> (
      match List.assoc_opt a b.ucs with
      | Some c' -> if c = c' then k b else None
      | None ->
          if List.for_all (fun (a', c') -> Int.compare a a' = Int.compare c c') b.ucs
          then k { b with ucs = (a, c) :: b.ucs }
          else None)
  | Term.App (f, _), Term.App (g, _) when f = g && syms.ac f ->
      assign syms (Term.operands f p) (Term.operands f t) b (fun b rest -> if rest = [] then k b else None)
  | Term.App (f, ps), Term.App (g, ts) when f = g && List.compare_lengths ps ts = 0 -> bind_all syms ps ts b k
  | _ -> None

and bind_all syms ps ts b k =
  match ps, ts with
  | p :: ps, t :: ts -> bind syms p t b (fun b -> bind_all syms ps ts b k)
  | _ -> k b

and assign syms ps ts b k =
  match ps with
  | [] -> k b ts
  | p :: ps -> List.find_map (fun (t, rest) -> bind syms p t b (fun b -> assign syms ps rest b k)) (picks ts)

let no_binding = { vars = []; ucs = [] }
let matches syms pattern term = bind syms pattern term no_binding Option.some

(* At the root, an AC pattern may also match part of the operands (extension). *)
let redex syms lhs term =
  match lhs, term with
  | Term.App (f, _), Term.App (g, _) when f = g && syms.Term.ac f ->
      assign syms (Term.operands f lhs) (Term.operands f term) no_binding (fun b rest -> Some (b, rest))
  | _ -> bind syms lhs term no_binding (fun b -> Some (b, []))

let instantiate syms b rhs =
  Term.normalize syms
    (Term.map_leaves
       (function Term.Var x -> List.assoc x b.vars | Term.Uc a -> Term.Uc (List.assoc a b.ucs) | t -> t)
       rhs)

let rewrite_root syms index term =
  List.find_map
    (fun (lhs, rhs) ->
      Option.map
        (fun (b, rest) ->
          match lhs, rest with
          | Term.App (f, _), _ :: _ -> Term.app syms f (instantiate syms b rhs :: rest)
          | _ -> instantiate syms b rhs)
        (redex syms lhs term))
    (Index.candidates syms.ac index term)

let reducible_at_root syms index term =
  List.exists
    (fun (lhs, _) -> Option.is_some (redex syms lhs term))
    (Index.candidates syms.Term.ac index term)

let rec normalize syms index term =
  let term =
    match term with
    | Term.App (f, ts) -> Term.app syms f (List.map (normalize syms index) ts)
    | leaf -> leaf
  in
  match rewrite_root syms index term with Some t -> normalize syms index t | None -> term
