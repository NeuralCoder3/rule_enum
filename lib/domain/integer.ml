type sym = Neg | Mul | Sub | Add
type value = int

let symbols = [ (Neg, 1); (Add, 2); (Sub, 2); (Mul, 2) ]
let compare = Stdlib.compare
let ac = function Add | Mul -> true | Neg | Sub -> false
let name = function Neg | Sub -> "-" | Mul -> "*" | Add -> "+"

let eval f args =
  match f, args with
  | Neg, [ a ] -> -a
  | Add, [ a; b ] -> a + b
  | Sub, [ a; b ] -> a - b
  | Mul, [ a; b ] -> a * b
  | _ -> invalid_arg "Integer.eval"

let sample rng = Random.State.int rng 21 - 10
let universe = None
let smt = Some { Theory.sort = "Int"; op = name }
