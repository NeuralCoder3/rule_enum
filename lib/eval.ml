type 'a input = (string * 'a) list

let lookup var input =
  match List.assoc_opt var input with
  | Some v -> v
  | None -> failwith ("Unbound variable: " ^ var)

let rec eval (dom : 'a Domain.t) (inp : 'a input) = function
  | Types.Var v -> lookup v inp
  | Types.Hole _ -> failwith "Cannot evaluate a Hole"
  | Types.Node (f, args) ->
    let arg_vals = List.map (eval dom inp) args in
    dom.Domain.eval_op f arg_vals

let behavior (dom : 'a Domain.t) (inputs : 'a input list) (t : Types.term) : 'a list =
  List.map (fun inp -> eval dom inp t) inputs

let generate_inputs dom num_inputs num_vars =
  dom.Domain.generate_inputs num_inputs num_vars
