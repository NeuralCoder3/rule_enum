type sym = Add | Zero
type value = int

let symbols = [ (Add, 2); (Zero, 0) ]
let compare = Stdlib.compare
let ac = function Add -> true | Zero -> false
let name = function Add -> "+" | Zero -> "0"

let eval f args =
  match f, args with
  | Add, [ a; b ] -> a + b
  | Zero, [] -> 0
  | _ -> invalid_arg "Demo.eval"

let sample rng = Random.State.int rng 21 - 10
let universe = None
let smt = Some { Theory.sort = "Int"; op = name }
