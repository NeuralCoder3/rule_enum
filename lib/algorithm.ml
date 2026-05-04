module BH = Map.Make (struct
  type t = int list
  let compare a b =
    let rec go = function
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x :: xs, y :: ys ->
        (match compare x y with
         | 0 -> go (xs, ys)
         | c -> c)
    in
    go (a, b)
end)

type rule_sets = {
  mutable size_rules : Types.rule list;
  mutable kbo_rules : Types.rule list;
  mutable irreducible : Types.term list;
  mutable behaviors : (Types.term * int list) list;
  inputs : Eval.input list;
}

let create inputs = {
  size_rules = [];
  kbo_rules = [];
  irreducible = [];
  behaviors = [];
  inputs;
}

let all_rules rs = rs.size_rules @ rs.kbo_rules

let run_iteration (sig' : (string * int) list) (rs : rule_sets) (n : int) =
  let candidates = ref [] in
  let enumerated = Enum.enumerate_terms sig' rs.irreducible n in

  List.iter (fun t ->
    let all_r = all_rules rs in
    let simplified, size_reduced = Rewrite.normalize all_r t in
    if not size_reduced then (
      let bv = Eval.behavior rs.inputs simplified in
      let matched = ref false in
      List.iter (fun (irr, irr_bv) ->
        if (not !matched) && bv = irr_bv then (
          let irr_sz = Types.size irr in
          let t_sz = Types.size simplified in
          if irr_sz < t_sz then (
            rs.size_rules <- (simplified, irr) :: rs.size_rules;
            matched := true
          ) else if irr_sz = t_sz then (
            match Kbo.kbo_compare simplified irr with
            | 0 -> matched := true
            | c when c > 0 ->
              rs.kbo_rules <- (simplified, irr) :: rs.kbo_rules;
              matched := true
            | _ ->
              rs.kbo_rules <- (irr, simplified) :: rs.kbo_rules;
              let idx = ref (-1) in
              List.iteri (fun i (t', _) ->
                if Types.term_eq t' irr then idx := i
              ) rs.behaviors;
              if !idx >= 0 then (
                rs.behaviors <- List.mapi (fun i (t', b) ->
                  if i = !idx then (simplified, b) else (t', b)
                ) rs.behaviors;
                rs.irreducible <- List.mapi (fun i t' ->
                  if i = !idx then simplified else t'
                ) rs.irreducible
              );
              matched := true
          )
        )
      ) rs.behaviors;
      if not !matched then
        candidates := (simplified, bv) :: !candidates
    )
  ) enumerated;

  let by_behavior = ref BH.empty in
  List.iter (fun (t, bv) ->
    by_behavior := BH.update bv (function
      | None -> Some [t]
      | Some ts -> Some (t :: ts)
    ) !by_behavior
  ) !candidates;

  BH.iter (fun bv terms ->
    let best = Kbo.minimum terms in
    rs.irreducible <- best :: rs.irreducible;
    rs.behaviors <- (best, bv) :: rs.behaviors;
    List.iter (fun other ->
      if not (Types.term_eq other best) then
        rs.kbo_rules <- (other, best) :: rs.kbo_rules
    ) terms
  ) !by_behavior

let run (sig' : (string * int) list) (max_size : int) (num_inputs : int) =
  let inputs = Eval.generate_random_inputs num_inputs 10 in
  let rs = create inputs in

  for n = 1 to max_size do
    run_iteration sig' rs n
  done;
  rs
