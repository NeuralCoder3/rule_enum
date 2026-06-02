type symbol = 
  Not | Neg | Plus | Minus | Times | Shl | Shr | And | Or

let compare_symbol a b = compare a b

let all_symbols = [
  ("~", 1, Not); ("-", 1, Neg); ("+", 2, Plus); ("-", 2, Minus); ("*", 2, Times);
  ("<<", 2, Shl); (">>", 2, Shr); ("&", 2, And); ("|", 2, Or);
]

let string_of_symbol x = List.find (fun (_s, _, sym) -> sym = x) all_symbols |> fun (s, _, _) -> s
let symbol_of_string s = List.find (fun (s', _, _) -> s' = s) all_symbols |> fun (_, _, sym) -> sym

(* Bit width for both random evaluation AND SMT reasoning — they MUST
   match, or Tier-1 random bucketing and Tier-3 SMT confirmation use
   different semantics and the pipeline can emit unsound rules.

   32-bit multiplication bit-blasts into a huge SAT instance, so SMT
   queries mixing `*` with bitwise ops can make Z3 diverge (capped by
   the solver timeout in Smt). A smaller width is far faster for
   synthesis and still finds the width-agnostic algebraic identities.
   Override with RULE_ENUM_BV_WIDTH. Capped at 62 so `1 lsl bv_width`
   and the modular arithmetic below stay within OCaml's 63-bit int. *)
let bv_width =
  let w = match Sys.getenv_opt "RULE_ENUM_BV_WIDTH" with
    | Some s -> (try int_of_string s with _ -> 32)
    | None -> 32
  in max 1 (min 62 w)

(* Mask for the low `bv_width` bits. `(1 lsl 62) - 1` wraps to max_int,
   which is exactly 2^62 - 1, so the formula is correct up to w = 62. *)
let bv_mask = (1 lsl bv_width) - 1

(* Canonical unsigned residue in [0, 2^bv_width). OCaml int arithmetic is
   modular mod 2^63 and 2^w divides 2^63, so masking the low w bits after
   any +,-,*,lsl recovers the exact w-bit result even when the operation
   overflowed 63 bits — matching Z3's modular bitvector arithmetic. *)
let norm x = x land bv_mask

let bv_domain : (symbol, int) Domain.t = {
  Domain.eval_op = (fun sym args -> match sym, args with
  | Not, [a] -> norm (lnot a)
  | Neg, [a] -> norm (- a)
  | Plus, [a; b] -> norm (a + b)
  | Minus, [a; b] -> norm (a - b)
  | Times, [a; b] -> norm (a * b)
  (* Shift amount is itself a bitvector value; a shift >= width yields 0,
     matching SMT-LIB bvshl / bvlshr. Operands are already normalized
     (inputs and op results are in range), but we normalize defensively. *)
  | Shl, [a; b] -> let b = norm b in if b >= bv_width then 0 else norm (a lsl b)
  | Shr, [a; b] -> let b = norm b in if b >= bv_width then 0 else (norm a) lsr b
  | And, [a; b] -> (norm a) land (norm b)
  | Or, [a; b] -> norm (a lor b)
    | _ -> failwith "bad arity for bv op"
  );
  Domain.generate_inputs = (fun num_inputs k ->
    (* Caller is responsible for seeding (Random.self_init or Random.init);
       deterministic seeds enable reproducible benchmarks. Values span the
       full [0, 2^bv_width) range (built from 30-bit chunks since
       Random.int caps at 2^30) so random testing exercises the whole
       bitvector domain, not just small magnitudes. *)
    let rand_bv () =
      (Random.bits () lxor (Random.bits () lsl 30) lxor (Random.bits () lsl 60))
      land bv_mask
    in
    let var_names = List.init k (fun i ->
      String.make 1 (Char.chr (Char.code 'a' + i))) in
    let hole_names = List.init k (fun i ->
      String.make 1 (Char.chr (Char.code 'A' + i))) in
    let names = var_names @ hole_names in
    List.init num_inputs (fun _ ->
      List.map (fun v -> (v, rand_bv ())) names)
  );
  Domain.to_string = string_of_int;
  Domain.equal = Int.equal;
  Domain.compare = Int.compare;
  Domain.all_symbols = all_symbols;
  Domain.sym_to_string = string_of_symbol;
  Domain.sym_compare = compare_symbol;
  (* SMT counterexamples come back as unsigned bv numerals in [0, 2^w);
     normalize so they are canonical residues consistent with eval. *)
  Domain.int_to_val = (fun n -> norm n);
  Domain.smt_sort = (fun ctx -> Z3.BitVector.mk_sort ctx bv_width);
  Domain.encode_op = (fun ctx sym args -> match sym, args with
    | Not, [a] -> Z3.BitVector.mk_not ctx a
    | Neg, [a] -> Z3.BitVector.mk_neg ctx a
    | Plus, [a; b] -> Z3.BitVector.mk_add ctx a b
    | Minus, [a; b] -> Z3.BitVector.mk_sub ctx a b
    | Times, [a; b] -> Z3.BitVector.mk_mul ctx a b
    | Shl, [a; b] -> Z3.BitVector.mk_shl ctx a b
    | Shr, [a; b] -> Z3.BitVector.mk_lshr ctx a b
    | And, [a; b] -> Z3.BitVector.mk_and ctx a b
    | Or, [a; b] -> Z3.BitVector.mk_or ctx a b
    | _ -> failwith "bad arity for int SMT op"
  );
}
