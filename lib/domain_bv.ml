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

(* Full-range value in [0, 2^bv_width): built from 30-bit chunks since
   Random.bits caps at 30 bits. *)
let rand_bv () =
  (Random.bits () lxor (Random.bits () lsl 30) lxor (Random.bits () lsl 60))
  land bv_mask

(* Custom sampler: a MIXTURE of full-range and small values. Full-range
   alone almost never exercises shifts — a random `b` is < bv_width (a
   meaningful shift amount) with probability ~bv_width/2^bv_width, which is
   negligible at width 32, so `<<`/`>>` collapse to 0 on essentially every
   sample and their algebraic identities are never probed. We therefore
   draw, with probability 1/2, a small value in [0, 2*bv_width) (covers all
   valid shift amounts and small magnitudes; capped at the domain size for
   tiny widths), and otherwise a full-range value. 0/1/all-ones fall out of
   the small bucket and full range naturally. *)
let small_bound = min (bv_mask + 1) (2 * bv_width)
let sample () =
  if Random.bool () then Random.int small_bound else rand_bv ()

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
  Domain.sample = sample;
  (* Enumerable value set only for tiny widths, where exhaustive evaluation
     over a rule's distinct leaves is an exact, Z3-free equivalence oracle.
     Capped so the list itself stays small; the per-check combo budget
     (values^leaves) is enforced separately. *)
  Domain.values =
    (if bv_width <= 10 then Some (List.init (1 lsl bv_width) norm) else None);
  Domain.generate_inputs = Domain.inputs_of_sampler sample;
  Domain.to_string = string_of_int;
  Domain.equal = Int.equal;
  Domain.compare = Int.compare;
  Domain.all_symbols = all_symbols;
  Domain.sym_to_string = string_of_symbol;
  Domain.sym_compare = compare_symbol;
  Domain.term_to_string = Types.to_string string_of_symbol;
  Domain.term_of_string = Parse.term_parser all_symbols;
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
