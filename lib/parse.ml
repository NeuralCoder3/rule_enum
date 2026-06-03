(* Parser for the human-readable term format produced by `Types.to_string`.
   Round-trips: `Types.to_string sym_str t |> Parse.parse_term decode = t`
   when `decode` inverts `sym_str` over the domain's symbol table.

   `Types.to_string` emits three node shapes:
     - single-char unary  f a   →  "(fa)"     e.g. "(~a)", "(-a)"
     - single-char binary a f b →  "(afb)"    e.g. "(a+b)"
     - everything else (multi-char op like "<<", or any arity) is PREFIX:
                          f(a,b) →  "f(a,b)"   e.g. "<<(a,b)", ">>(a,b)"

   Grammar:
     expr  ::= VAR | HOLE | '(' paren ')' | OP '(' args ')'
     paren ::= OPCH expr            (* '(' then op char ⇒ unary  *)
            |  expr OPCH expr       (* '(' then a leaf  ⇒ binary *)
     args  ::= expr (',' expr)*
     VAR   ::= [a-z]   HOLE ::= [A-Z]   OP/OPCH ::= run of operator chars

   Disambiguation: VAR/HOLE are letters; an operator name is a run of
   non-letter, non-paren, non-comma, non-space chars; `(` starts the
   single-char infix/unary form. After `(`, a leading op char means
   unary, a leading leaf means binary. *)

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
  (* Operator-name chars: anything that isn't a leaf-start, paren, comma
     or whitespace — e.g. + - * < > & | ~ ^. *)
  let is_op_char c =
    not (is_var_char c || is_hole_char c)
    && c <> '(' && c <> ')' && c <> ',' && c <> ' '
    && c <> '\t' && c <> '\n' && c <> '\r'
  in
  let read_op () =
    let start = !pos in
    while (match peek () with Some c -> is_op_char c | None -> false) do advance () done;
    String.sub input start (!pos - start)
  in
  let decode_node op_str args =
    match decode op_str (List.length args) with
    | Some sym -> Types.Node (sym, args)
    | None -> failwith (Printf.sprintf
        "parse_term: unknown op %s/%d" op_str (List.length args))
  in
  let read_op_char () = match peek () with
    | Some c -> advance (); String.make 1 c
    | None -> failwith "parse_term: EOF where operator expected"
  in
  let rec parse () =
    skip_ws ();
    match peek () with
    | Some c when is_var_char c ->
      advance (); Types.Var (Char.code c - Char.code 'a')
    | Some c when is_hole_char c ->
      advance (); Types.Hole (Char.code c - Char.code 'A')
    | Some '(' ->
      advance (); skip_ws ();
      let head = match peek () with
        | Some c -> c | None -> failwith "parse_term: EOF after '('" in
      if is_leaf_start head then
        (* Binary infix with a leaf / parenthesized LHS: (lhs OP rhs). *)
        parse_binary (parse ())
      else if decode (String.make 1 head) 1 <> None then begin
        (* Leading char is a single-char unary op: `(~a)`, `(-(a-b))`,
           `(~<<(a,A))`. Consume just that char; its operand follows
           (and may itself be a prefix term, hence we don't read the
           whole op run — `~` and `<<` abut with no separator). *)
        advance ();
        let arg = parse () in expect ')';
        decode_node (String.make 1 head) [arg]
      end else
        (* A multi-char prefix term is the LHS of a binary, `(<<(a,b)|c)`. *)
        parse_binary (parse_prefix (read_op ()))
    | Some c when is_op_char c ->
      (* Top-level prefix term: OP '(' args ')'. *)
      parse_prefix (read_op ())
    | Some c -> failwith (Printf.sprintf
        "parse_term: unexpected '%c' at pos %d in: %s" c !pos input)
    | None -> failwith ("parse_term: unexpected end of input: " ^ input)
  (* Finish a binary infix `lhs OP rhs )` given the already-parsed LHS. *)
  and parse_binary lhs =
    skip_ws ();
    let op = read_op_char () in
    let rhs = parse () in
    expect ')';
    decode_node op [lhs; rhs]
  (* Parse `'(' arg (',' arg)* ')'` for a prefix op whose name was read. *)
  and parse_prefix op_str =
    expect '(';
    let args = ref [parse ()] in
    skip_ws ();
    while peek () = Some ',' do
      advance (); args := parse () :: !args; skip_ws ()
    done;
    expect ')';
    decode_node op_str (List.rev !args)
  in
  let result = parse () in
  skip_ws ();
  if !pos <> len then
    failwith (Printf.sprintf
      "parse_term: trailing input at pos %d in: %s" !pos input);
  result

(* Build a decoder from a domain's symbol table. *)
let decoder_of_symbols (all_symbols : (string * int * 's) list) =
  fun name arity ->
    List.find_map (fun (n, a, s) ->
      if n = name && a = arity then Some s else None) all_symbols

(* Standard term parser for a domain's symbol table — the inverse of
   `Types.to_string`. Domains wire `term_of_string` from this; callers go
   through the domain's `term_of_string`. *)
let term_parser all_symbols : string -> 's Types.term =
  parse_term (decoder_of_symbols all_symbols)

(* Rule I/O is generic in a per-term renderer/parser (the domain's
   `term_to_string` / `term_of_string`), so the line format and the
   "lhs -> rhs" arrow handling live in one place. *)
let rule_separator_re = Str.regexp "[ \t]+->[ \t]+"

let parse_rule (of_string : string -> 's Types.term) line : 's Types.rule =
  let trimmed = String.trim line in
  match Str.search_forward rule_separator_re trimmed 0 with
  | exception Not_found -> failwith ("parse_rule: no '->' in: " ^ line)
  | j ->
    let l = String.sub trimmed 0 j in
    let r_start = Str.match_end () in
    let r = String.sub trimmed r_start (String.length trimmed - r_start) in
    (of_string l, of_string r)

let format_rule to_string (l, r) = to_string l ^ " -> " ^ to_string r

(* Read non-blank, non-comment lines from `path` and apply `parse_line`. *)
let load_lines parse_line path =
  let ic = open_in path in
  let acc = ref [] in
  (try while true do
    let line = input_line ic in
    let s = String.trim line in
    if s <> "" && s.[0] <> '#' then acc := parse_line line :: !acc
  done with End_of_file -> ());
  close_in ic;
  List.rev !acc

let load_rules of_string path : 's Types.rule list = load_lines (parse_rule of_string) path
let load_terms of_string path : 's Types.term list = load_lines of_string path

(* Write each item as a line produced by `format`. *)
let save_lines format path items =
  let oc = open_out path in
  List.iter (fun x -> output_string oc (format x); output_char oc '\n') items;
  close_out oc

let save_rules to_string path rules = save_lines (format_rule to_string) path rules
let save_terms to_string path terms = save_lines to_string path terms
