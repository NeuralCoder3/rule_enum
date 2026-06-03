type ('s, 'a) t = {
  eval_op   : 's -> 'a list -> 'a;
  (* Custom per-value sampler: draws one random value from the domain's
     test distribution. Domains tune this — e.g. bv mixes small values
     (valid shift amounts) with full-range ones. The caller seeds Random
     (Random.init / self_init) so benchmarks stay reproducible. *)
  sample    : unit -> 'a;
  (* The complete finite value set, when small enough to enumerate (e.g.
     bv with a tiny width, or bool). `None` for infinite/large domains
     (int, wide bv). When present, equivalence of two terms over ≤ a few
     distinct leaves can be decided EXACTLY by exhaustive evaluation —
     faster than Z3 on small domains, and pure (so parallelizable). *)
  values    : 'a list option;
  generate_inputs : int -> int -> (string * 'a) list list;
  to_string : 'a -> string;
  equal     : 'a -> 'a -> bool;
  compare   : 'a -> 'a -> int;
  all_symbols : (string * int * 's) list;
  sym_to_string : 's -> string;
  sym_compare  : 's -> 's -> int;
  (* Term rendering / parsing for this domain. Round-trip:
     `term_of_string (term_to_string t) = t`. Both are wired from the
     shared grammar helpers (`Types.to_string`, `Parse.term_parser`) over
     `all_symbols`; keeping them on the domain lets callers (eval mode,
     report/rule output) print and parse terms without re-deriving a
     symbol decoder each time. *)
  term_to_string : 's Types.term -> string;
  term_of_string : string -> 's Types.term;
  int_to_val : int -> 'a;
  smt_sort  : Z3.context -> Z3.Sort.sort;
  encode_op : Z3.context -> 's -> Z3.Expr.expr list -> Z3.Expr.expr;
}

(* Build `num_inputs` assignment records from a per-value `sample`. Each
   record assigns an independently sampled value to every var slot
   (a, b, …) and hole slot (A, B, …) up to `k`. Domains define only their
   `sample`; this shared builder removes the duplicated name-construction
   that previously lived in every domain's generate_inputs. *)
let inputs_of_sampler (sample : unit -> 'a) num_inputs k
    : (string * 'a) list list =
  let var_names = List.init k (fun i ->
    String.make 1 (Char.chr (Char.code 'a' + i))) in
  let hole_names = List.init k (fun i ->
    String.make 1 (Char.chr (Char.code 'A' + i))) in
  let names = var_names @ hole_names in
  List.init num_inputs (fun _ ->
    List.map (fun v -> (v, sample ())) names)