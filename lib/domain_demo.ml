type symbol = Plus | Zero

let compare_symbol a b = compare a b

let string_of_symbol = function
  | Plus -> "+" | Zero -> "0"

let symbol_of_string = function
  | "+" -> Plus | "0" -> Zero | s -> failwith ("Unknown int symbol: " ^ s)

let all_symbols = [
  ("+", 2, Plus); ("0", 0, Zero);
]

(* Small signed values in [-10, 10]. *)
let sample () = Random.int 21 - 10

let demo_domain : (symbol, int) Domain.t = {
  Domain.eval_op = (fun sym args -> match sym, args with
    | Plus, [a; b] -> a + b
    | Zero, [] -> 0
    | _ -> failwith "bad arity for int op"
  );
  Domain.sample = sample;
  Domain.values = None;   (* infinite domain — equivalence needs SMT *)
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
    | Zero, [] -> Z3.Arithmetic.Integer.mk_numeral_i ctx 0
    | _ -> failwith "bad arity for int SMT op"
  );
}