open Z3

type result = Equivalent | CounterExample of (string * int) list | Unknown

let ctx = Z3.mk_context []
let solver () = Solver.mk_solver ctx None

let rec encode (dom : ('s, 'a) Domain.t) vars = function
  | Types.Var v -> List.assoc (Types.var_name v) vars
  | Types.Hole _ -> failwith "SMT: Hole"
  | Types.Node (f, args) ->
    dom.Domain.encode_op ctx f (List.map (encode dom vars) args)

let check_equiv (dom : ('s, 'a) Domain.t) var_names t1 t2 =
  let decls = List.map (fun n ->
    (n, Z3.Expr.mk_const_s ctx n (dom.Domain.smt_sort ctx))) var_names in
  let vars = List.map (fun (n, e) -> (n, e)) decls in
  let diff = Boolean.mk_not ctx (Boolean.mk_eq ctx (encode dom vars t1) (encode dom vars t2)) in
  let s = solver () in Solver.add s [diff];
  match Solver.check s [] with
  | Solver.UNSATISFIABLE -> Equivalent
  | Solver.SATISFIABLE ->
    (match Solver.get_model s with Some m ->
      let assigns = List.filter_map (fun (n, e) ->
        try let v = Model.eval m e true |> Option.get in
            let i = if Z3.Expr.is_numeral v then int_of_string (Expr.to_string v)
                    else if Boolean.is_true v then 1 else 0
            in Some (n, i) with _ -> None) vars
      in if assigns <> [] then CounterExample assigns else Unknown
     | None -> Unknown)
  | Solver.UNKNOWN -> Unknown
