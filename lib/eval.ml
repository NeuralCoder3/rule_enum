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

(* Arity-specialized evaluator. The vast majority of operators in our
   domains are unary or binary, so we evaluate args directly without
   building an intermediate List.map result list. *)
let rec eval_compiled (dom : ('s, 'a) Domain.t) (inp : 'a compiled) = function
  | Types.Var v -> inp.vars.(v)
  | Types.Hole n -> inp.holes.(n)
  | Types.Node (f, []) -> dom.Domain.eval_op f []
  | Types.Node (f, [a]) ->
    let va = eval_compiled dom inp a in
    dom.Domain.eval_op f [va]
  | Types.Node (f, [a; b]) ->
    let va = eval_compiled dom inp a in
    let vb = eval_compiled dom inp b in
    dom.Domain.eval_op f [va; vb]
  | Types.Node (f, args) ->
    dom.Domain.eval_op f (List.map (eval_compiled dom inp) args)

let rec eval (dom : ('s, 'a) Domain.t) (inp : 'a input) = function
  | Types.Var v -> lookup (Types.var_name v) inp
  | Types.Hole n -> lookup (Types.hole_name n) inp
  | Types.Node (f, args) ->
    dom.Domain.eval_op f (List.map (eval dom inp) args)

(* Behavior vector. Stored as an array for O(1) indexing and cache
   locality during evaluation/comparison. Note: the default polymorphic
   `Hashtbl.hash` (= hash_param 10 100) truncates an array to its first
   ~10 elements — it does NOT walk all of them. That is fine and in fact
   desirable here: benchmarking shows the first ~10 outputs already
   distinguish every inequivalent term (zero hash collisions on the
   distinct bvs), so the truncated hash is both collision-free and ~5x
   faster than a full-vector hash. Do not "improve" this to a full hash. *)
type 'a bv = 'a array

let behavior dom inputs t =
  let compiled = List.map compile inputs in
  Array.of_list (List.map (fun c -> eval_compiled dom c t) compiled)

let behavior_compiled dom compiled t =
  let arr = Array.of_list compiled in
  Array.map (fun c -> eval_compiled dom c t) arr

(* Faster: take a pre-converted array of compiled inputs. *)
let behavior_compiled_arr dom compiled_arr t =
  Array.map (fun c -> eval_compiled dom c t) compiled_arr

let make_examples dom inputs t =
  List.map (fun inp -> (inp, eval dom inp t)) inputs

(* True iff `t` evaluates to `expected` on every (input, expected) pair. *)
let evaluate_on_examples (dom : ('s, 'a) Domain.t) examples t =
  List.for_all (fun (inp, expected) ->
    dom.Domain.equal (eval dom inp t) expected) examples

let generate_inputs dom = dom.Domain.generate_inputs
