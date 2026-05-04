let int_domain : int Domain.t = {
  Domain.eval_op = (fun sym args ->
    match sym, args with
    | "-", [a] -> -a
    | "+", [a; b] -> a + b
    | "-", [a; b] -> a - b
    | "*", [a; b] -> a * b
    | "/", [a; b] -> if b = 0 then a else a / b
    | _ -> failwith ("Unknown integer operation: " ^ sym)
  );
  Domain.generate_inputs = (fun num_inputs num_vars ->
    Random.self_init ();
    let var_names = List.init num_vars (fun i ->
      String.make 1 (Char.chr (Char.code 'a' + i))) in
    List.init num_inputs (fun _ ->
      List.map (fun v -> (v, Random.int 21 - 10)) var_names)
  );
  Domain.to_string = string_of_int;
  Domain.equal = Int.equal;
  Domain.compare = Int.compare;
  Domain.signature = [
    ("-", 1);
    ("+", 2);
    ("-", 2);
    ("*", 2);
  ];
}
