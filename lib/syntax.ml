(* Textual format: Ucs a,b,.. and variables A,B,.. as in the paper, or the
   original tool's swapped convention with ~lower_vars; single-character
   operators infix (a&b) or prefix-unary (~a), all other operators
   applicative f(a,b), constants c(). *)

let leaf base i =
  if i < 26 then String.make 1 (Char.chr (Char.code base + i)) else Printf.sprintf "%c%d" base i

let var_base lower_vars = if lower_vars then 'a' else 'A'
let uc_base lower_vars = if lower_vars then 'A' else 'a'

let to_string ?(lower_vars = false) name =
  let rec show = function
    | Term.Var i -> leaf (var_base lower_vars) i
    | Term.Uc i -> leaf (uc_base lower_vars) i
    | Term.App (f, [ a ]) when String.length (name f) = 1 -> "(" ^ name f ^ show a ^ ")"
    | Term.App (f, [ a; b ]) when String.length (name f) = 1 -> "(" ^ show a ^ name f ^ show b ^ ")"
    | Term.App (f, ts) -> name f ^ "(" ^ String.concat "," (List.map show ts) ^ ")"
  in
  show

let is_space c = c = ' ' || c = '\t' || c = '\r' || c = '\n'
let is_lower c = 'a' <= c && c <= 'z'
let is_upper c = 'A' <= c && c <= 'Z'
let is_digit c = '0' <= c && c <= '9'
let is_op c = not (is_lower c || is_upper c || is_digit c || is_space c || String.contains "()," c)

let parse ?(lower_vars = false) ~symbols ~name input =
  let len = String.length input and pos = ref 0 in
  let fail msg = failwith (Printf.sprintf "parse error at %d in %S: %s" !pos input msg) in
  let peek () = if !pos < len then Some input.[!pos] else None in
  let skip () = while !pos < len && is_space input.[!pos] do incr pos done in
  let take p =
    let start = !pos in
    while !pos < len && p input.[!pos] do incr pos done;
    String.sub input start (!pos - start)
  in
  let expect c =
    skip ();
    if peek () = Some c then incr pos else fail (Printf.sprintf "expected '%c'" c)
  in
  let lookup op k = List.find_map (fun (f, a) -> if a = k && name f = op then Some f else None) symbols in
  let symbol op k =
    match lookup op k with Some f -> f | None -> fail (Printf.sprintf "unknown operator %s/%d" op k)
  in
  let leaf c =
    incr pos;
    let digits = take is_digit in
    let base = if is_lower c then 'a' else 'A' in
    let i = if digits = "" then Char.code c - Char.code base else int_of_string digits in
    if base = var_base lower_vars then Term.Var i else Term.Uc i
  in
  let rec term () =
    skip ();
    match peek () with
    | Some c when is_lower c || is_upper c -> leaf c
    | Some '(' -> (
        incr pos;
        skip ();
        match peek () with
        | Some c when is_lower c || is_upper c || c = '(' -> infix (term ())
        | Some c when is_op c && lookup (String.make 1 c) 1 <> None ->
            incr pos;
            let a = term () in
            expect ')';
            Term.App (symbol (String.make 1 c) 1, [ a ])
        | Some c when is_op c || is_digit c -> infix (applied (take (fun c -> is_op c || is_digit c)))
        | _ -> fail "unexpected input after '('")
    | Some c when is_op c || is_digit c -> applied (take (fun c -> is_op c || is_digit c))
    | _ -> fail "unexpected input"
  and infix lhs =
    skip ();
    match peek () with
    | Some c when is_op c ->
        incr pos;
        let rhs = term () in
        expect ')';
        Term.App (symbol (String.make 1 c) 2, [ lhs; rhs ])
    | _ -> fail "expected infix operator"
  and applied op =
    expect '(';
    skip ();
    if peek () = Some ')' then (incr pos; Term.App (symbol op 0, []))
    else
      let rec args () =
        let a = term () in
        skip ();
        if peek () = Some ',' then (incr pos; a :: args ()) else (expect ')'; [ a ])
      in
      let ts = args () in
      Term.App (symbol op (List.length ts), ts)
  in
  let t = term () in
  skip ();
  if !pos <> len then fail "trailing input";
  t

let rule_to_string ?lower_vars name (lhs, rhs) =
  to_string ?lower_vars name lhs ^ " -> " ^ to_string ?lower_vars name rhs

let parse_rule ?lower_vars ~symbols ~name line =
  let n = String.length line in
  let rec arrow i =
    if i + 3 >= n then failwith ("no rule arrow in: " ^ line)
    else if is_space line.[i] && line.[i + 1] = '-' && line.[i + 2] = '>' && is_space line.[i + 3] then i
    else arrow (i + 1)
  in
  let i = arrow 0 in
  ( parse ?lower_vars ~symbols ~name (String.sub line 0 i),
    parse ?lower_vars ~symbols ~name (String.sub line (i + 3) (n - i - 3)) )

let read_lines path =
  In_channel.with_open_text path In_channel.input_all
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun l -> l <> "" && l.[0] <> '#')

let write_lines path lines =
  let tmp = path ^ ".tmp" in
  Out_channel.with_open_text tmp (fun oc ->
      List.iter (fun l -> output_string oc l; output_char oc '\n') lines);
  Sys.rename tmp path
