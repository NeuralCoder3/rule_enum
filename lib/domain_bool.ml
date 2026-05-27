type symbol = Not | And | Or | Xor

let compare_symbol a b = match a, b with
  | Not, Not -> 0 | Not, _ -> -1 | _, Not -> 1
  | And, And -> 0 | And, _ -> -1 | _, And -> 1
  | Or, Or -> 0 | Or, _ -> -1 | _, Or -> 1
  | Xor, Xor -> 0

let string_of_symbol = function
  | Not -> "!" | And -> "&" | Or -> "|" | Xor -> "^"

let symbol_of_string = function
  | "!" -> Not | "&" -> And | "|" -> Or | "^" -> Xor
  | s -> failwith ("Unknown bool symbol: " ^ s)

let bool_domain : (symbol, bool) Domain.t = {
  Domain.eval_op = (fun sym args -> match sym, args with
    | Not, [a] -> not a
    | And, [a; b] -> a && b
    | Or, [a; b] -> a || b
    | Xor, [a; b] -> a <> b
    | _ -> failwith "bad arity for bool op"
  );
  Domain.generate_inputs = (fun num_inputs k ->
    Random.self_init ();
    let var_names = List.init k (fun i ->
      String.make 1 (Char.chr (Char.code 'a' + i))) in
    let hole_names = List.init k (fun i ->
      String.make 1 (Char.chr (Char.code 'A' + i))) in
    let names = var_names @ hole_names in
    List.init num_inputs (fun _ ->
      List.map (fun v -> (v, Random.bool ())) names)
  );
  Domain.to_string = string_of_bool;
  Domain.equal = Bool.equal;
  Domain.compare = Bool.compare;
  Domain.all_symbols = [
    ("!", 1, Not); ("&", 2, And); ("|", 2, Or); ("^", 2, Xor);
  ];
  Domain.sym_to_string = string_of_symbol;
  Domain.sym_compare = compare_symbol;
  Domain.int_to_val = (fun n -> n <> 0);
  Domain.smt_sort = (fun ctx -> Z3.Boolean.mk_sort ctx);
  Domain.encode_op = (fun ctx sym args -> match sym, args with
    | Not, [a] -> Z3.Boolean.mk_not ctx a
    | And, [a; b] -> Z3.Boolean.mk_and ctx [a; b]
    | Or, [a; b] -> Z3.Boolean.mk_or ctx [a; b]
    | Xor, [a; b] -> Z3.Boolean.mk_xor ctx a b
    | _ -> failwith "bad arity for bool SMT op"
  );
}

let all_inputs k =
  let var_names = List.init k (fun i ->
    String.make 1 (Char.chr (Char.code 'a' + i))) in
  let hole_names = List.init k (fun i ->
    String.make 1 (Char.chr (Char.code 'A' + i))) in
  let names = var_names @ hole_names in
  let rec go = function
    | [] -> [[]]
    | v :: vs ->
      let rest = go vs in
      List.concat_map (fun assigns ->
        [(v, true) :: assigns; (v, false) :: assigns]
      ) rest
  in go names
