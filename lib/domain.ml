type 'a t = {
  eval_op   : string -> 'a list -> 'a;
  generate_inputs : int -> int -> (string * 'a) list list;
  to_string : 'a -> string;
  equal     : 'a -> 'a -> bool;
  compare   : 'a -> 'a -> int;
  signature : (string * int) list;
}
