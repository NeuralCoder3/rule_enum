(* Equivalence queries in SMT-LIB 2 against a persistent solver process. *)
type verdict = Equivalent | Different | Unknown

let encode op =
  let rec go = function
    | Term.Var i -> Printf.sprintf "v%d" i
    | Term.Uc i -> Printf.sprintf "u%d" i
    | Term.App (f, []) -> op f
    | Term.App (f, ts) -> Printf.sprintf "(%s %s)" (op f) (String.concat " " (List.map go ts))
  in
  go

let query { Theory.sort; op } s t =
  let declare l = Printf.sprintf "(declare-const %s %s)" (encode op l) sort in
  let leaves = List.sort_uniq compare (Term.leaves s @ Term.leaves t) in
  String.concat ""
    ((("(push)" :: List.map declare leaves))
    @ [ Printf.sprintf "(assert (not (= %s %s)))(check-sat)(pop)\n" (encode op s) (encode op t) ])

let prover ?(command = "z3 -in") ?(timeout_ms = 1000) smt =
  let session =
    lazy
      (let ic, oc = Unix.open_process command in
       Printf.fprintf oc "(set-option :timeout %d)\n%!" timeout_ms;
       (ic, oc))
  in
  fun s t ->
    let ic, oc = Lazy.force session in
    output_string oc (query smt s t);
    flush oc;
    let rec answer () =
      match String.trim (input_line ic) with
      | "unsat" -> Equivalent
      | "sat" -> Different
      | "unknown" -> Unknown
      | _ -> answer ()
    in
    try answer () with End_of_file -> failwith ("Smt: solver process failed: " ^ command)
