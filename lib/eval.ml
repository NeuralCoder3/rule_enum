type input = (string * Domain.t) list

let lookup var input =
  match List.assoc_opt var input with
  | Some v -> v
  | None -> failwith ("Unbound variable: " ^ var)

let rec eval (inp : input) = function
  | Types.Var v -> lookup v inp
  | Types.Hole _ -> failwith "Cannot evaluate a Hole"
  | Types.Node (f, args) ->
    let arg_vals = List.map (eval inp) args in
    Domain.eval_op f arg_vals

let behavior (inputs : input list) (t : Types.term) : Domain.t list =
  List.map (fun inp -> eval inp t) inputs

let generate_random_inputs num_inputs num_vars =
  Domain.generate_random_inputs num_inputs num_vars
