(* Discrimination tree over the preorder token sequence of patterns. An AC
   chain is matched modulo permutation and extension, so it contributes only
   the sorted heads of its non-variable operands, which the subject's
   operand heads must contain as a sub-multiset. *)
type 'f token = Symbol of 'f * int | Constant | Wildcard | Close

type ('f, 'a) t = { values : 'a list; children : ('f token * ('f, 'a) t) list }

let empty = { values = []; children = [] }

let head = function
  | Term.App (f, ts) -> Some (Symbol (f, List.length ts))
  | Term.Uc _ -> Some Constant
  | Term.Var _ -> None

let operand_heads f t = List.sort compare (List.filter_map head (Term.operands f t))

let rec tokens ac = function
  | Term.Var _ -> [ Wildcard ]
  | Term.Uc _ -> [ Constant ]
  | Term.App (f, ts) as t when ac f -> (Symbol (f, List.length ts) :: operand_heads f t) @ [ Close ]
  | Term.App (f, ts) -> Symbol (f, List.length ts) :: List.concat_map (tokens ac) ts

let add ac pattern value index =
  let rec go node = function
    | [] -> { node with values = value :: node.values }
    | tok :: rest ->
        let child = Option.value (List.assoc_opt tok node.children) ~default:empty in
        { node with children = (tok, go child rest) :: List.remove_assoc tok node.children }
  in
  go index (tokens ac pattern)

(* Values whose pattern may match [term] at the root. *)
let candidates ac index term =
  let follow tok node k acc =
    Option.fold ~none:acc ~some:(fun c -> k c acc) (List.assoc_opt tok node.children)
  in
  let rec go node pending acc =
    match pending with
    | [] -> List.rev_append node.values acc
    | t :: rest -> (
        let acc = follow Wildcard node (fun c -> go c rest) acc in
        match t with
        | Term.App (f, ts) when ac f ->
            follow (Symbol (f, List.length ts)) node (fun c -> within c rest (operand_heads f t)) acc
        | Term.App (f, ts) -> follow (Symbol (f, List.length ts)) node (fun c -> go c (ts @ rest)) acc
        | Term.Uc _ -> follow Constant node (fun c -> go c rest) acc
        | Term.Var _ -> acc)
  and within node rest heads acc =
    let acc = follow Close node (fun c -> go c rest) acc in
    let rec choose previous acc = function
      | [] -> acc
      | h :: hs when Some h = previous -> choose previous acc hs
      | h :: hs -> choose (Some h) (follow h node (fun c -> within c rest hs) acc) hs
    in
    choose None acc heads
  in
  go index [ term ] []
