let bool_domain : bool Domain.t = {
  Domain.eval_op = (fun sym args ->
    match sym, args with
    | "!", [a] -> not a
    | "&", [a; b] -> a && b
    | "|", [a; b] -> a || b
    | "^", [a; b] -> a <> b
    | _ -> failwith ("Unknown boolean operation: " ^ sym)
  );
  Domain.generate_inputs = (fun num_inputs num_vars ->
    Random.self_init ();
    let var_names = List.init num_vars (fun i ->
      String.make 1 (Char.chr (Char.code 'a' + i))) in
    List.init num_inputs (fun _ ->
      List.map (fun v -> (v, Random.bool ())) var_names)
  );
  Domain.to_string = string_of_bool;
  Domain.equal = Bool.equal;
  Domain.compare = Bool.compare;
  Domain.signature = [
    ("!", 1);
    ("&", 2);
    ("|", 2);
    ("^", 2);
  ];
}

let all_inputs max_vars =
  let var_names = List.init max_vars (fun i ->
    String.make 1 (Char.chr (Char.code 'a' + i))) in
  let rec go = function
    | [] -> [[]]
    | v :: vs ->
      let rest = go vs in
      List.concat_map (fun assigns ->
        [(v, true) :: assigns; (v, false) :: assigns]
      ) rest
  in
  go var_names
