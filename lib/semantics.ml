module Make (T : Theory.THEORY) = struct
  type input = { vars : T.value array; ucs : T.value array }

  let rec eval_with valuation = function
    | Term.App (f, ts) -> T.eval f (List.map (eval_with valuation) ts)
    | leaf -> valuation leaf

  let eval input =
    eval_with (function
      | Term.Var i -> input.vars.(i)
      | Term.Uc i -> input.ucs.(i)
      | Term.App _ -> assert false)

  let assignments values n = Combi.product (List.init n (fun _ -> List.to_seq values))

  let feasible n limit =
    match T.universe with
    | Some u when Combi.power (List.length u) n <= limit -> Some u
    | _ -> None

  (* Exhaustive when the universe admits it, random samples otherwise;
     the flag tells whether agreement on the inputs proves equivalence. *)
  let inputs ?(exhaustive_limit = 256) ?(force_exhaustive = false) rng ~samples ~vars ~ucs =
    let limit = if force_exhaustive then max_int else exhaustive_limit in
    match feasible (vars + ucs) limit with
    | Some u ->
        let split a = { vars = Array.sub a 0 vars; ucs = Array.sub a vars ucs } in
        (Array.of_seq (Seq.map (fun vs -> split (Array.of_list vs)) (assignments u (vars + ucs))), true)
    | None when force_exhaustive -> invalid_arg "Semantics.inputs: universe not enumerable"
    | None ->
        let draw n = Array.init n (fun _ -> T.sample rng) in
        (Array.init samples (fun _ -> { vars = draw vars; ucs = draw ucs }), false)

  let exhaustively_equal ~limit s t =
    let leaves = List.sort_uniq compare (Term.leaves s @ Term.leaves t) in
    feasible (List.length leaves) limit
    |> Option.map (fun u ->
           assignments u (List.length leaves)
           |> Seq.for_all (fun values ->
                  let valuation leaf = List.assoc leaf (List.combine leaves values) in
                  eval_with valuation s = eval_with valuation t))

  let oracle ?prove ?(limit = 1 lsl 16) (inputs, complete) : T.sym Oracle.t =
    let vector t = Array.map (fun input -> eval input t) inputs in
    let decide s t =
      complete
      ||
      match exhaustively_equal ~limit s t with
      | Some b -> b
      | None -> Option.fold prove ~none:true ~some:(fun prove -> prove s t)
    in
    { fingerprint = (fun t -> Hashtbl.hash_param 1024 1024 (vector t));
      equivalent = (fun s t -> vector s = vector t && decide s t) }
end
