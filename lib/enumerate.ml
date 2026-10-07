(* Terms f(t1,..,tm) whose arguments are injective renamings of normal
   forms (Refinements 2 and 3): only the root can be a redex. Without AC
   symbols they are canonical by construction. *)
type caps = { vars : int; ucs : int; leaves : int }

let var_gluings cap counts =
  let rec inject next used k =
    if k = 0 then Seq.return ([], next)
    else
      let shared = Seq.filter (fun v -> not (List.mem v used)) (Seq.init next Fun.id) in
      let fresh = if next < cap then Seq.return next else Seq.empty in
      Seq.append shared fresh
      |> Seq.flat_map (fun v ->
             Seq.map (fun (vs, n) -> (v :: vs, n)) (inject (max next (v + 1)) (v :: used) (k - 1)))
  in
  let rec go next = function
    | [] -> Seq.return ([], next)
    | k :: ks ->
        inject next [] k
        |> Seq.flat_map (fun (m, next) -> Seq.map (fun (ms, n) -> (m :: ms, n)) (go next ks))
  in
  go 0 counts

let uc_gluings cap counts =
  let covers j maps = List.length (List.sort_uniq Int.compare (List.concat maps)) = j in
  let lo = List.fold_left max 0 counts and hi = min cap (List.fold_left ( + ) 0 counts) in
  Seq.init (max 0 (hi - lo + 1)) (( + ) lo)
  |> Seq.flat_map (fun j ->
         Combi.product (List.map (fun k -> Combi.increasing k j) counts) |> Seq.filter (covers j))

let glue caps args =
  let count ids = List.map (fun t -> List.length (ids t)) args in
  var_gluings (min caps.vars caps.leaves) (count Term.vars)
  |> Seq.flat_map (fun (vmaps, used) ->
         uc_gluings (min caps.ucs (caps.leaves - used)) (count Term.ucs)
         |> Seq.map (fun umaps ->
                List.map2
                  (fun t (vm, um) -> Term.rename ~var:(List.nth vm) ~uc:(List.nth um) t)
                  args (List.combine vmaps umaps)))

let leaves caps symbols =
  let leaf n t = if n > 0 && caps.leaves > 0 then [ t ] else [] in
  leaf caps.vars (Term.Var 0) @ leaf caps.ucs (Term.Uc 0)
  @ List.filter_map (fun (f, k) -> if k = 0 then Some (Term.App (f, [])) else None) symbols

let terms (syms : _ Term.symbols) caps symbols normal_forms n =
  let build =
    if List.exists (fun (f, _) -> syms.ac f) symbols then fun f args ->
      Term.normalize syms (Term.App (f, args))
    else fun f args -> Term.App (f, args)
  in
  if n = 1 then List.to_seq (leaves caps symbols)
  else
    List.to_seq symbols
    |> Seq.flat_map (fun (f, k) ->
           if k = 0 then Seq.empty
           else
             Combi.compositions (n - 1) k
             |> Seq.flat_map (fun sizes ->
                    Combi.product (List.map (fun s -> List.to_seq (normal_forms s)) sizes)
                    |> Seq.flat_map (fun args ->
                           Seq.map (build f) (glue caps args))))
