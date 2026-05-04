type t = bool

let eval_op (sym : string) (args : t list) : t =
  match sym, args with
  | "&", [a; b] -> a && b
  | "|", [a; b] -> a || b
  | "^", [a; b] -> a <> b
  | _ -> failwith ("Unknown boolean operation: " ^ sym)

let generate_random_inputs num_inputs num_vars =
  Random.self_init ();
  let var_names = List.init num_vars (fun i ->
    String.make 1 (Char.chr (Char.code 'a' + i))) in
  List.init num_inputs (fun _ ->
    List.map (fun v -> (v, Random.bool ())) var_names)

let to_string = string_of_bool

let equal = Bool.equal

let compare = Bool.compare
