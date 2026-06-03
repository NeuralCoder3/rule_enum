type ('s, 'a) t = {
  eval_op   : 's -> 'a list -> 'a;
  generate_inputs : int -> int -> (string * 'a) list list;
  to_string : 'a -> string;
  equal     : 'a -> 'a -> bool;
  compare   : 'a -> 'a -> int;
  all_symbols : (string * int * 's) list;
  sym_to_string : 's -> string;
  sym_compare  : 's -> 's -> int;
  (* Term rendering / parsing for this domain. Round-trip:
     `term_of_string (term_to_string t) = t`. Both are wired from the
     shared grammar helpers (`Types.to_string`, `Parse.term_parser`) over
     `all_symbols`; keeping them on the domain lets callers (eval mode,
     report/rule output) print and parse terms without re-deriving a
     symbol decoder each time. *)
  term_to_string : 's Types.term -> string;
  term_of_string : string -> 's Types.term;
  int_to_val : int -> 'a;
  smt_sort  : Z3.context -> Z3.Sort.sort;
  encode_op : Z3.context -> 's -> Z3.Expr.expr list -> Z3.Expr.expr;
}