type ('s, 'a) t = {
  eval_op   : 's -> 'a list -> 'a;
  generate_inputs : int -> int -> (string * 'a) list list;
  to_string : 'a -> string;
  equal     : 'a -> 'a -> bool;
  compare   : 'a -> 'a -> int;
  all_symbols : (string * int * 's) list;  (* name * arity * symbol *)
  sym_to_string : 's -> string;
  sym_compare  : 's -> 's -> int;
  smt_sort  : Z3.context -> Z3.Sort.sort;
  encode_op : Z3.context -> 's -> Z3.Expr.expr list -> Z3.Expr.expr;
}