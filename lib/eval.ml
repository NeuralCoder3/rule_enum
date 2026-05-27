type 'a input = (string * 'a) list

let lookup var input =
  match List.assoc_opt var input with Some v -> v
  | None -> failwith ("Unbound name: " ^ var)

(* Pre-compiled input: two arrays of length k each, indexed by var/hole id.
   Lookup becomes O(1) array access instead of O(k) list traversal with
   string compares. *)
type 'a compiled = { vars : 'a array; holes : 'a array }

let compile (inp : 'a input) : 'a compiled =
  (* Determine k by finding the highest var/hole index referenced in inp. *)
  let max_v = ref (-1) and max_h = ref (-1) in
  List.iter (fun (name, _) ->
    if String.length name = 1 then
      let c = name.[0] in
      if c >= 'a' && c <= 'z' then
        max_v := max !max_v (Char.code c - Char.code 'a')
      else if c >= 'A' && c <= 'Z' then
        max_h := max !max_h (Char.code c - Char.code 'A')) inp;
  let kv = !max_v + 1 and kh = !max_h + 1 in
  let vars = Array.make (max kv 1) (Obj.magic 0) in
  let holes = Array.make (max kh 1) (Obj.magic 0) in
  List.iter (fun (name, v) ->
    if String.length name = 1 then
      let c = name.[0] in
      if c >= 'a' && c <= 'z' then
        vars.(Char.code c - Char.code 'a') <- v
      else if c >= 'A' && c <= 'Z' then
        holes.(Char.code c - Char.code 'A') <- v) inp;
  { vars; holes }

let rec eval_compiled (dom : ('s, 'a) Domain.t) (inp : 'a compiled) = function
  | Types.Var v -> inp.vars.(v)
  | Types.Hole n -> inp.holes.(n)
  | Types.Node (f, args) ->
    dom.Domain.eval_op f (List.map (eval_compiled dom inp) args)

let rec eval (dom : ('s, 'a) Domain.t) (inp : 'a input) = function
  | Types.Var v -> lookup (Types.var_name v) inp
  | Types.Hole n -> lookup (Types.hole_name n) inp
  | Types.Node (f, args) ->
    dom.Domain.eval_op f (List.map (eval dom inp) args)

(* `behavior` uses precompiled inputs for hot-path speed. *)
let behavior dom inputs t =
  let compiled = List.map compile inputs in
  List.map (fun c -> eval_compiled dom c t) compiled

(* Faster path that takes pre-compiled inputs (compile once outside the
   hot loop, then reuse for each term). *)
let behavior_compiled dom compiled t =
  List.map (fun c -> eval_compiled dom c t) compiled

let make_examples dom inputs t =
  List.map (fun inp -> (inp, eval dom inp t)) inputs

(* True iff `t` evaluates to `expected` on every (input, expected) pair. *)
let evaluate_on_examples (dom : ('s, 'a) Domain.t) examples t =
  List.for_all (fun (inp, expected) ->
    dom.Domain.equal (eval dom inp t) expected) examples

let generate_inputs dom = dom.Domain.generate_inputs
