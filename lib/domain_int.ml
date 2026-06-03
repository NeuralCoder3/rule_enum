type symbol = Plus | Minus | Times | UMinus

let compare_symbol a b = match a, b with
  | UMinus, UMinus -> 0 | UMinus, _ -> -1 | _, UMinus -> 1
  | Times, Times -> 0 | Times, _ -> -1 | _, Times -> 1
  | Minus, Minus -> 0 | Minus, _ -> -1 | _, Minus -> 1
  | Plus, Plus -> 0

let string_of_symbol = function
  | Plus -> "+" | Minus -> "-" | Times -> "*" | UMinus -> "-"

let symbol_of_string = function
  | "+" -> Plus | "-" -> Minus | "*" -> Times | s -> failwith ("Unknown int symbol: " ^ s)

let all_symbols = [
  ("-", 1, UMinus); ("+", 2, Plus); ("-", 2, Minus); ("*", 2, Times);
]

(* Small signed values in [-10, 10]. *)
let sample () = Random.int 21 - 10

let int_domain : (symbol, int) Domain.t = {
  Domain.eval_op = (fun sym args -> match sym, args with
    | Plus, [a; b] -> a + b
    | Minus, [a; b] -> a - b
    | Times, [a; b] -> a * b
    | UMinus, [a] -> -a
    | _ -> failwith "bad arity for int op"
  );
  Domain.sample = sample;
  Domain.generate_inputs = Domain.inputs_of_sampler sample;
  Domain.to_string = string_of_int;
  Domain.equal = Int.equal;
  Domain.compare = Int.compare;
  Domain.all_symbols = all_symbols;
  Domain.sym_to_string = string_of_symbol;
  Domain.sym_compare = compare_symbol;
  Domain.term_to_string = Types.to_string string_of_symbol;
  Domain.term_of_string = Parse.term_parser all_symbols;
  Domain.int_to_val = (fun n -> n);
  Domain.smt_sort = (fun ctx -> Z3.Arithmetic.Integer.mk_sort ctx);
  Domain.encode_op = (fun ctx sym args -> match sym, args with
    | Plus, [a; b] -> Z3.Arithmetic.mk_add ctx [a; b]
    | Minus, [a; b] -> Z3.Arithmetic.mk_sub ctx [a; b]
    | Times, [a; b] -> Z3.Arithmetic.mk_mul ctx [a; b]
    | UMinus, [a] -> Z3.Arithmetic.mk_unary_minus ctx a
    | _ -> failwith "bad arity for int SMT op"
  );
}