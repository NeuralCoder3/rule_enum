type sym = Not | And | Or | Xor
type value = bool

let symbols = [ (Not, 1); (And, 2); (Or, 2); (Xor, 2) ]
let compare = Stdlib.compare
let ac = function And | Or | Xor -> true | Not -> false
let name = function Not -> "~" | And -> "&" | Or -> "|" | Xor -> "^"

let eval f args =
  match f, args with
  | Not, [ a ] -> not a
  | And, [ a; b ] -> a && b
  | Or, [ a; b ] -> a || b
  | Xor, [ a; b ] -> a <> b
  | _ -> invalid_arg "Boolean.eval"

let sample = Random.State.bool
let universe = Some [ false; true ]

let smt =
  Some { Theory.sort = "Bool"; op = (function Not -> "not" | And -> "and" | Or -> "or" | Xor -> "xor") }
