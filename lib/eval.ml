type 'a input = (string * 'a) list

let lookup var input =
  match List.assoc_opt var input with Some v -> v
  | None -> failwith ("Unbound variable: " ^ var)

let rec eval (dom : ('s, 'a) Domain.t) (inp : 'a input) = function
  | Types.Var v -> lookup (Types.var_name v) inp
  | Types.Hole _ -> failwith "Cannot evaluate a Hole"
  | Types.Node (f, args) ->
    dom.Domain.eval_op f (List.map (eval dom inp) args)

let behavior dom inputs t = List.map (fun inp -> eval dom inp t) inputs

let make_examples dom inputs t = List.map (fun inp -> (inp, eval dom inp t)) inputs

let generate_inputs dom = dom.Domain.generate_inputs
