type input = (string * int) list

let lookup var input =
  match List.assoc_opt var input with
  | Some v -> v
  | None -> failwith ("Unbound variable: " ^ var)

let rec eval (inp : input) = function
  | Types.Var v -> lookup v inp
  | Types.Hole _ -> failwith "Cannot evaluate a Hole"
  | Types.Node ("+", [a; b]) -> eval inp a + eval inp b
  | Types.Node ("-", [a; b]) -> eval inp a - eval inp b
  | Types.Node ("*", [a; b]) -> eval inp a * eval inp b
  | Types.Node ("/", [a; b]) ->
    let denom = eval inp b in
    if denom = 0 then eval inp a
    else eval inp a / denom
  | Types.Node (f, _) ->
    failwith ("Unknown operation: " ^ f)

let behavior (inputs : input list) (t : Types.term) : int list =
  List.map (fun inp -> eval inp t) inputs

let generate_random_inputs num_inputs num_vars =
  Random.self_init ();
  let var_names = List.init num_vars (fun i ->
    String.make 1 (Char.chr (Char.code 'a' + i))) in
  List.init num_inputs (fun _ ->
    List.map (fun v -> (v, Random.int 21 - 10)) var_names)
