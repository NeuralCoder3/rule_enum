type sym = Not | And | Bot | Top
type value = bool

let symbols = [ (Not, 1); (And, 2); (Bot, 0); (Top, 0) ]
let compare = Stdlib.compare
let ac = function And -> true | Not -> false | Bot -> false | Top -> true
let name = function Not -> "~" | And -> "&" | Bot -> "⊥" | Top -> "⊤"

let eval f args =
  match f, args with
  | Not, [ a ] -> not a
  | And, [ a; b ] -> a && b
  | Bot, [] -> false
  | Top, [] -> true
  | _ -> invalid_arg "Boolean.eval"

let sample = Random.State.bool
let universe = Some [ false; true ]

let smt =
  Some { Theory.sort = "Bool"; op = (function Not -> "not" | And -> "and" | Bot -> "false" | Top -> "true") }
