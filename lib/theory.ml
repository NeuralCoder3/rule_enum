type 'f smt = { sort : string; op : 'f -> string }

module type SIGNATURE = sig
  type sym

  val symbols : (sym * int) list
  val compare : sym -> sym -> int
  val ac : sym -> bool
  val name : sym -> string
end

module type THEORY = sig
  include SIGNATURE

  type value

  val eval : sym -> value list -> value
  val sample : Random.State.t -> value
  val universe : value list option
  val smt : sym smt option
end
