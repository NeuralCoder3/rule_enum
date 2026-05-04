type t = int

let eval_op (sym : string) (args : t list) : t =
  match sym, args with
  | "+", [a; b] -> a + b
  | "-", [a; b] -> a - b
  | "*", [a; b] -> a * b
  | "/", [a; b] -> if b = 0 then a else a / b
  | _ -> failwith ("Unknown integer operation: " ^ sym)

let generate_random_inputs num_inputs num_vars =
  Random.self_init ();
  let var_names = List.init num_vars (fun i ->
    String.make 1 (Char.chr (Char.code 'a' + i))) in
  List.init num_inputs (fun _ ->
    List.map (fun v -> (v, Random.int 21 - 10)) var_names)

let to_string = string_of_int

let equal = Int.equal

let compare = Int.compare
