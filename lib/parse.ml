(* Parser for the human-readable term format produced by `Types.to_string`.
   Round-trips: `Types.to_string sym_str t |> Parse.parse_term decode = t`
   when `decode` inverts `sym_str` over the domain's symbol table.

   Grammar:
     expr  ::= VAR | HOLE | '(' inner ')'
     inner ::= unary_op expr            (* operator + 1 expr  ⇒ unary *)
            |  expr binary_op expr      (* expr + operator + 1 expr ⇒ binary *)
     VAR   ::= [a-z]                    (* renaming-equivalent schema vars *)
     HOLE  ::= [A-Z]                    (* fixed-identity constPs *)

   Disambiguation: after '(', if the next non-whitespace char is an
   operator symbol, treat as unary; otherwise binary. This works because
   the LHS of a binary expression always begins with a var, hole, or
   '(' (the start of a sub-expression). *)

let parse_term (decode : string -> int -> 's option) (input : string) : 's Types.term =
  let pos = ref 0 in
  let len = String.length input in
  let peek () = if !pos < len then Some input.[!pos] else None in
  let advance () = incr pos in
  let skip_ws () =
    while !pos < len &&
      (let c = input.[!pos] in c = ' ' || c = '\t' || c = '\n' || c = '\r')
    do incr pos done
  in
  let expect c =
    skip_ws ();
    match peek () with
    | Some c' when c' = c -> advance ()
    | Some c' -> failwith (Printf.sprintf
        "parse_term: expected '%c' at pos %d, got '%c' in: %s" c !pos c' input)
    | None -> failwith (Printf.sprintf
        "parse_term: expected '%c' at end of input: %s" c input)
  in
  let is_var_char c = c >= 'a' && c <= 'z' in
  let is_hole_char c = c >= 'A' && c <= 'Z' in
  let is_leaf_start c = is_var_char c || is_hole_char c || c = '(' in
  let rec parse () =
    skip_ws ();
    match peek () with
    | Some c when is_var_char c ->
      advance ();
      Types.Var (Char.code c - Char.code 'a')
    | Some c when is_hole_char c ->
      advance ();
      Types.Hole (Char.code c - Char.code 'A')
    | Some '(' ->
      advance ();
      skip_ws ();
      let head = match peek () with
        | Some c -> c
        | None -> failwith "parse_term: EOF after '('"
      in
      if is_leaf_start head then begin
        (* Binary: parse LHS, op, RHS, ')' *)
        let lhs = parse () in
        skip_ws ();
        let op = match peek () with
          | Some c -> advance (); c
          | None -> failwith "parse_term: EOF where binary op expected"
        in
        let op_str = String.make 1 op in
        let rhs = parse () in
        expect ')';
        (match decode op_str 2 with
         | Some sym -> Types.Node (sym, [lhs; rhs])
         | None -> failwith ("parse_term: unknown binary op: " ^ op_str))
      end else begin
        (* Unary: op is the head char itself. *)
        advance ();  (* consume the op char *)
        let op_str = String.make 1 head in
        let arg = parse () in
        expect ')';
        (match decode op_str 1 with
         | Some sym -> Types.Node (sym, [arg])
         | None -> failwith ("parse_term: unknown unary op: " ^ op_str))
      end
    | Some c -> failwith (Printf.sprintf
        "parse_term: unexpected '%c' at pos %d in: %s" c !pos input)
    | None -> failwith ("parse_term: unexpected end of input: " ^ input)
  in
  let result = parse () in
  skip_ws ();
  if !pos <> len then
    failwith (Printf.sprintf
      "parse_term: trailing input at pos %d in: %s" !pos input);
  result

let parse_rule decode line : 's Types.rule =
  (* Tolerates leading/trailing whitespace and 1+ spaces around the "->". *)
  let trimmed = String.trim line in
  let arrow = " -> " in
  match String.index_opt trimmed '-' with
  | None -> failwith ("parse_rule: no '->' in: " ^ line)
  | Some _ ->
    let len = String.length trimmed in
    let alen = String.length arrow in
    let rec find_arrow i =
      if i + alen > len then -1
      else if String.sub trimmed i alen = arrow then i
      else find_arrow (i + 1)
    in
    let i = find_arrow 0 in
    if i < 0 then
      (* Fall back: try "->" without spaces, or "  ->  " etc. *)
      let re = Str.regexp "[ \t]+->[ \t]+" in
      (match Str.search_forward re trimmed 0 with
       | exception Not_found ->
         failwith ("parse_rule: no '->' in: " ^ line)
       | j ->
         let l = String.sub trimmed 0 j in
         let r_start = Str.match_end () in
         let r = String.sub trimmed r_start (String.length trimmed - r_start) in
         (parse_term decode l, parse_term decode r))
    else
      let l = String.sub trimmed 0 i in
      let r = String.sub trimmed (i + alen) (len - i - alen) in
      (parse_term decode l, parse_term decode r)

(* Build a decoder from a domain's symbol table. *)
let decoder_of_symbols (all_symbols : (string * int * 's) list) =
  fun name arity ->
    List.find_map (fun (n, a, s) ->
      if n = name && a = arity then Some s else None) all_symbols

let load_rules decode path : 's Types.rule list =
  let ic = open_in path in
  let rules = ref [] in
  (try while true do
    let line = input_line ic in
    let s = String.trim line in
    if s <> "" && s.[0] <> '#' then
      rules := parse_rule decode line :: !rules
  done with End_of_file -> ());
  close_in ic;
  List.rev !rules

let load_terms decode path : 's Types.term list =
  let ic = open_in path in
  let terms = ref [] in
  (try while true do
    let line = input_line ic in
    let s = String.trim line in
    if s <> "" && s.[0] <> '#' then
      terms := parse_term decode s :: !terms
  done with End_of_file -> ());
  close_in ic;
  List.rev !terms

let save_rules sym_str path rules =
  let oc = open_out path in
  List.iter (fun (l, r) ->
    Printf.fprintf oc "%s -> %s\n"
      (Types.to_string sym_str l) (Types.to_string sym_str r)) rules;
  close_out oc

let save_terms sym_str path terms =
  let oc = open_out path in
  List.iter (fun t ->
    Printf.fprintf oc "%s\n" (Types.to_string sym_str t)) terms;
  close_out oc
