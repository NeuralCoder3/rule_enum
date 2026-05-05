(* SMT verification using Z3 OCaml bindings. *)
open Z3

let ctx = Z3.mk_context []

let solver () = Solver.mk_solver ctx None

type result = Equivalent | CounterExample of (string * int) list | Unknown

let bool_sort = Boolean.mk_sort ctx
let int_sort  = Arithmetic.Integer.mk_sort ctx

(* Create a boolean Z3 variable *)
let mk_bool name = Boolean.mk_const_s ctx name

(* Create an integer Z3 variable *)
let mk_int name = Arithmetic.Integer.mk_const_s ctx name

(* Encode a boolean term as a Z3 expression *)
let rec encode_bool vars = function
  | Types.Var v -> List.assoc (Types.var_name v) vars
  | Types.Node (f, [a; b]) ->
    let ea = encode_bool vars a in
    let eb = encode_bool vars b in
    (match Types.Sym.to_string f with
     | "&" -> Boolean.mk_and ctx [ea; eb]
     | "|" -> Boolean.mk_or ctx [ea; eb]
     | "^" -> Boolean.mk_xor ctx ea eb
     | s -> failwith ("Unknown bool op: " ^ s))
  | Types.Node (f, [a]) when Types.Sym.to_string f = "!" ->
    Boolean.mk_not ctx (encode_bool vars a)
  | _ -> failwith "encode_bool: unsupported term"

(* Encode an integer term as a Z3 expression *)
let rec encode_int vars = function
  | Types.Var v -> List.assoc (Types.var_name v) vars
  | Types.Node (f, [a; b]) ->
    let ea = encode_int vars a in
    let eb = encode_int vars b in
    (match Types.Sym.to_string f with
     | "+" -> Arithmetic.mk_add ctx [ea; eb]
     | "-" -> Arithmetic.mk_sub ctx [ea; eb]
     | "*" -> Arithmetic.mk_mul ctx [ea; eb]
     | s -> failwith ("Unknown int op: " ^ s))
  | Types.Node (f, [a]) when Types.Sym.to_string f = "-" ->
    Arithmetic.mk_unary_minus ctx (encode_int vars a)
  | _ -> failwith "encode_int: unsupported term"

(* Check if two terms are equivalent (domain = "bool" or "int").
   Returns Equivalent if unsat, CounterExample if sat, Unknown on error. *)
let check_equiv (domain : string) vars t1 t2 =
  let decls = List.map (fun (n, s) -> 
    (n, match s with "bool" -> mk_bool n | _ -> mk_int n))
    vars
  in
  let vars_map = List.map (fun (n, e) -> (n, e)) decls in
  let encode = if domain = "bool" then encode_bool else encode_int in
  let e1 = encode vars_map t1 in
  let e2 = encode vars_map t2 in
  let diff = Boolean.mk_not ctx (Boolean.mk_eq ctx e1 e2) in
  let s = solver () in
  Solver.add s [diff];
  match Solver.check s [] with
  | Solver.UNSATISFIABLE -> Equivalent
  | Solver.SATISFIABLE ->
    (match Solver.get_model s with
     | Some m ->
       let assigns = List.filter_map (fun (n, _) ->
         let e = List.assoc n vars_map in
         try
           let v = Model.eval m e true |> Option.get in
           let i = match domain with
             | "bool" -> if Boolean.is_true v then 1 else 0
             | _ -> int_of_string (Expr.to_string v)
           in Some (n, i)
         with _ -> None) decls
       in if assigns <> [] then CounterExample assigns else Unknown
     | None -> Unknown)
  | Solver.UNKNOWN -> Unknown
