module Make (W : sig
  val width : int
end) =
struct
  type sym = Not | Neg | Add | Sub | Mul | Shl | Shr | And | Or
  type value = int

  let symbols =
    [ (Not, 1); (Neg, 1); (Add, 2); (Sub, 2); (Mul, 2); (Shl, 2); (Shr, 2); (And, 2); (Or, 2) ]

  let compare = Stdlib.compare
  let ac = function Add | Mul | And | Or -> true | Not | Neg | Sub | Shl | Shr -> false

  let name = function
    | Not -> "~" | Neg | Sub -> "-" | Add -> "+" | Mul -> "*"
    | Shl -> "<<" | Shr -> ">>" | And -> "&" | Or -> "|"

  let mask = (1 lsl W.width) - 1
  let norm x = x land mask
  let shift op a b = if b >= W.width then 0 else norm (op a b)

  let eval f args =
    match f, args with
    | Not, [ a ] -> norm (lnot a)
    | Neg, [ a ] -> norm (-a)
    | Add, [ a; b ] -> norm (a + b)
    | Sub, [ a; b ] -> norm (a - b)
    | Mul, [ a; b ] -> norm (a * b)
    | Shl, [ a; b ] -> shift ( lsl ) a b
    | Shr, [ a; b ] -> shift ( lsr ) a b
    | And, [ a; b ] -> a land b
    | Or, [ a; b ] -> a lor b
    | _ -> invalid_arg "Bitvector.eval"

  (* Half of the samples are small so that shifts are exercised. *)
  let sample rng =
    if Random.State.bool rng then Random.State.int rng (min (mask + 1) (2 * W.width))
    else norm (Random.State.bits rng lor (Random.State.bits rng lsl 30) lor (Random.State.bits rng lsl 60))

  let universe = if W.width <= 8 then Some (List.init (mask + 1) Fun.id) else None

  let smt =
    Some
      { Theory.sort = Printf.sprintf "(_ BitVec %d)" W.width;
        op =
          (function
          | Not -> "bvnot" | Neg -> "bvneg" | Add -> "bvadd" | Sub -> "bvsub" | Mul -> "bvmul"
          | Shl -> "bvshl" | Shr -> "bvlshr" | And -> "bvand" | Or -> "bvor") }
end
