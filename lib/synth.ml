module IntMap = Map.Make (Int)

type 'f t = {
  rules : 'f Rewrite.rule list;
  index : 'f Rewrite.t;
  normal_forms : 'f Term.t list IntMap.t;
  by_fingerprint : 'f Term.t list IntMap.t;
}

type 'f step = {
  size : int;
  enumerated : int;
  candidates : int;
  new_rules : 'f Rewrite.rule list;
  new_normal_forms : 'f Term.t list;
  seconds : float;
}

let empty = { rules = []; index = Rewrite.empty; normal_forms = IntMap.empty; by_fingerprint = IntMap.empty }
let push key v m = IntMap.update key (fun vs -> Some (v :: Option.value vs ~default:[])) m
let find key m = Option.value (IntMap.find_opt key m) ~default:[]
let rules st = List.rev st.rules
let normal_forms st = List.concat_map (fun (_, ts) -> List.rev ts) (IntMap.bindings st.normal_forms)
let shrinking (lhs, rhs) = Term.size rhs < Term.size lhs

module Make (S : Theory.SIGNATURE) = struct
  let syms = { Term.precedence = S.compare; ac = S.ac }
  let max_arity = List.fold_left (fun m (_, k) -> max m k) 0 S.symbols

  (* No term of size [n] or larger can be built from the normal forms. *)
  let exhausted st n =
    n > 1 + (max_arity * Option.fold ~none:0 ~some:fst (IntMap.max_binding_opt st.normal_forms))

  (* getLeastEqual: the least known normal form, up to renaming, that is an
     admissible right-hand side for [t]. *)
  let least_equivalent (oracle : S.sym Oracle.t) known t =
    known
    |> List.filter (fun r -> Term.regular ~lhs:t r && Kbo.greater syms t r)
    |> List.sort (Term.compare syms)
    |> List.find_opt (oracle.equivalent t)

  let add_rule st rule = { st with rules = rule :: st.rules; index = Rewrite.add syms rule st.index }

  let instances (caps : Enumerate.caps) t =
    Seq.map (Term.normalize syms) (Term.instances ~vars:caps.vars ~ucs:caps.ucs t)

  let update_normal_form update caps (oracle : S.sym Oracle.t) st t =
    { st with
      normal_forms = update (Term.size t) t st.normal_forms;
      by_fingerprint =
        Seq.fold_left (fun m r -> update (oracle.fingerprint r) r m) st.by_fingerprint (instances caps t) }

  let add_normal_form = update_normal_form push
  let remove_normal_form =
    update_normal_form (fun key v -> IntMap.update key (Option.map (List.filter (( <> ) v))))

  (* Under AC, candidates may repeat known normal forms up to renaming. *)
  let classify caps oracle st t =
    let known = find (oracle.Oracle.fingerprint t) st.by_fingerprint in
    if Rewrite.reducible_at_root syms st.index t || List.mem t known then st
    else
      match least_equivalent oracle known t with
      | Some r -> add_rule st (t, r)
      | None -> add_normal_form caps oracle st t

  (* Extends the (AC-)KBO, so smaller equivalents of the same size are
     classified first. Fresh variables come first so that, without AC, a
     same-size pattern precedes the terms it matches; under AC, normal forms
     reduced by later same-size rules are pruned. *)
  let processing_order = Term.compare ~var:(Fun.flip Int.compare) syms

  let step caps oracle st n =
    let start = Unix.gettimeofday () in
    let enumerated, candidates =
      Enumerate.terms syms caps S.symbols (fun s -> find s st.normal_forms) n
      |> Seq.fold_left
           (fun (k, cs) t -> (k + 1, if Rewrite.reducible_at_root syms st.index t then cs else t :: cs))
           (0, [])
    in
    let candidates = List.sort_uniq processing_order candidates in
    let st' = List.fold_left (classify caps oracle) st candidates in
    let stale = List.filter (Rewrite.reducible_at_root syms st'.index) (find n st'.normal_forms) in
    let st' = List.fold_left (remove_normal_form caps oracle) st' stale in
    let fresh = List.length st'.rules - List.length st.rules in
    ( st',
      { size = n;
        enumerated;
        candidates = List.length candidates;
        new_rules = List.rev (List.filteri (fun i _ -> i < fresh) st'.rules);
        new_normal_forms = List.rev (find n st'.normal_forms);
        seconds = Unix.gettimeofday () -. start } )

  let run ?(on_step = fun _ _ -> ()) caps ~max_size oracle =
    let rec loop st n =
      if n > max_size || (n > 1 && exhausted st n) then st
      else
        let st, s = step caps oracle st n in
        on_step st s;
        loop st (n + 1)
    in
    loop empty 1
end
