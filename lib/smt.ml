open Z3

type result = Equivalent | CounterExample of (string * int) list | Unknown

let ctx = Z3.mk_context []

(* Per-query wall-clock cap (ms). Bitvector queries (multiplication,
   shifts) can make Z3 diverge for minutes on a single (lhs, rhs) pair;
   without a cap a single such query stalls the whole run. On timeout Z3
   returns UNKNOWN, which the algorithm handles by falling back to extra
   random sampling (see Algorithm.tier3_unknown_fallback) — so a tight
   cap costs at worst a probabilistic verdict, not a dropped rule. Keep
   it short by default. Override with RULE_ENUM_SMT_TIMEOUT_MS. *)
let timeout_ms =
  match Sys.getenv_opt "RULE_ENUM_SMT_TIMEOUT_MS" with
  | Some s -> (try max 0 (int_of_string s) with _ -> 1000)
  | None -> 1000

let solver () =
  let s = Solver.mk_solver ctx None in
  (if timeout_ms > 0 then begin
    let p = Z3.Params.mk_params ctx in
    Z3.Params.add_int p (Z3.Symbol.mk_string ctx "timeout") timeout_ms;
    Solver.set_parameters s p
  end);
  s

let rec encode (dom : ('s, 'a) Domain.t) vars = function
  | Types.Var v -> List.assoc (Types.var_name v) vars
  | Types.Hole n -> List.assoc (Types.hole_name n) vars
  | Types.Node (f, args) ->
    dom.Domain.encode_op ctx f (List.map (encode dom vars) args)

(* Extract an OCaml int from a Z3 model value. `Expr.to_string` is not
   safe to parse: Z3 prints integers as "5" / "(- 5)" and bitvectors as
   "#x0000000a" / "#b1010", neither of which `int_of_string` accepts.
   Use the typed numeral accessors, which return plain decimal strings
   (signed for integers, unsigned for bitvectors). *)
let int_of_model_value v =
  if Boolean.is_true v then Some 1
  else if Boolean.is_false v then Some 0
  else if BitVector.is_bv_numeral v then
    (try Some (int_of_string (BitVector.numeral_to_string v)) with _ -> None)
  else if Z3.Arithmetic.is_int v && Expr.is_numeral v then
    (try Some (int_of_string (Z3.Arithmetic.Integer.numeral_to_string v)) with _ -> None)
  else
    (try Some (int_of_string (Expr.to_string v)) with _ -> None)

(* `slot_names` should cover every Var/Hole name that may appear in
   either input term. With the new representation we pass both
   var_name 0..k-1 and hole_name 0..k-1. *)
let trace_slow =
  match Sys.getenv_opt "RULE_ENUM_SMT_TRACE_MS" with
  | Some s -> (try Some (int_of_string s) with _ -> None)
  | None -> None

let check_equiv (dom : ('s, 'a) Domain.t) slot_names t1 t2 =
  let decls = List.map (fun n ->
    (n, Z3.Expr.mk_const_s ctx n (dom.Domain.smt_sort ctx))) slot_names in
  let vars = List.map (fun (n, e) -> (n, e)) decls in
  let diff = Boolean.mk_not ctx (Boolean.mk_eq ctx (encode dom vars t1) (encode dom vars t2)) in
  let s = solver () in Solver.add s [diff];
  let t_start = match trace_slow with Some _ -> Unix.gettimeofday () | None -> 0.0 in
  let res = Solver.check s [] in
  (match trace_slow with
   | Some thresh_ms ->
     let dt = (Unix.gettimeofday () -. t_start) *. 1000.0 in
     if dt >= float_of_int thresh_ms then
       Printf.eprintf "[slow-smt %.0fms %s] %s  =?=  %s\n%!" dt
         (match res with Solver.UNSATISFIABLE -> "EQ"
          | Solver.SATISFIABLE -> "CE" | Solver.UNKNOWN -> "UNK")
         (Types.to_string dom.Domain.sym_to_string t1)
         (Types.to_string dom.Domain.sym_to_string t2)
   | None -> ());
  match res with
  | Solver.UNSATISFIABLE -> Equivalent
  | Solver.SATISFIABLE ->
    (match Solver.get_model s with Some m ->
      let assigns = List.filter_map (fun (n, e) ->
        match Model.eval m e true with
        | Some v -> Option.map (fun i -> (n, i)) (int_of_model_value v)
        | None -> None) vars
      in if assigns <> [] then CounterExample assigns else Unknown
     | None -> Unknown)
  | Solver.UNKNOWN -> Unknown
