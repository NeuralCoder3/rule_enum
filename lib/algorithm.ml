let rec list_compare elt_compare a b =
  match a, b with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
    (match elt_compare x y with
     | 0 -> list_compare elt_compare xs ys
     | c -> c)

let rec list_equal eq a b =
  match a, b with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && list_equal eq xs ys
  | _ -> false

type 'a rule_sets = {
  mutable size_rules : Types.rule list;
  mutable kbo_rules : Types.rule list;
  mutable irreducible : Types.term list;
  mutable behaviors : (Types.term * 'a list) list;
  inputs : 'a Eval.input list;
}

let create inputs = {
  size_rules = [];
  kbo_rules = [];
  irreducible = [];
  behaviors = [];
  inputs;
}

let all_rules rs = rs.size_rules @ rs.kbo_rules

let group_by cmp key_of_value values =
  let rec loop values acc =
    match values with
    | [] -> acc
    | v :: rest ->
      let k = key_of_value v in
      let rec insert = function
        | [] -> [(k, [v])]
        | (k', vs) :: tl when cmp k k' = 0 -> (k', v :: vs) :: tl
        | pair :: tl -> pair :: insert tl
      in
      loop rest (insert acc)
  in
  loop values []

type iter_summary = {
  size : int;
  enumerated : int;
  new_size_rules : Types.rule list;
  new_kbo_rules : Types.rule list;
  new_irreducibles : Types.term list;
}

let run_iteration (dom : 'a Domain.t) (rs : 'a rule_sets) (n : int) (max_vars : int)
    : iter_summary =
  let new_size_rules = ref [] in
  let new_kbo_rules = ref [] in
  let new_irreducibles = ref [] in
  let candidates = ref [] in
  let enumerated = Enum.enumerate_terms dom.Domain.signature rs.irreducible n max_vars in

  List.iter (fun t ->
    let all_r = all_rules rs in
    let simplified, size_reduced = Rewrite.normalize all_r t in
    if not size_reduced then (
      let bv = Eval.behavior dom rs.inputs simplified in
      let matched = ref false in
      List.iter (fun (irr, irr_bv) ->
        if (not !matched) && list_equal dom.Domain.equal bv irr_bv then (
          let irr_sz = Types.size irr in
          let t_sz = Types.size simplified in
          if irr_sz < t_sz then (
            let rule = (simplified, irr) in
            rs.size_rules <- rule :: rs.size_rules;
            new_size_rules := rule :: !new_size_rules;
            matched := true
          ) else if irr_sz = t_sz then (
            match Kbo.kbo_compare simplified irr with
            | 0 -> matched := true
            | c when c > 0 ->
              let rule = (simplified, irr) in
              rs.kbo_rules <- rule :: rs.kbo_rules;
              new_kbo_rules := rule :: !new_kbo_rules;
              matched := true
            | _ ->
              let rule = (irr, simplified) in
              rs.kbo_rules <- rule :: rs.kbo_rules;
              new_kbo_rules := rule :: !new_kbo_rules;
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

  let cmp = list_compare dom.Domain.compare in
  let by_behavior = group_by cmp snd !candidates in
  List.iter (fun (bv, term_pairs) ->
    let terms = List.map fst term_pairs in
    let best = Kbo.minimum terms in
    rs.irreducible <- best :: rs.irreducible;
    rs.behaviors <- (best, bv) :: rs.behaviors;
    new_irreducibles := best :: !new_irreducibles;
    List.iter (fun other ->
      if not (Types.term_eq other best) then (
        let rule = (other, best) in
        rs.kbo_rules <- rule :: rs.kbo_rules;
        new_kbo_rules := rule :: !new_kbo_rules
      )
    ) terms
  ) by_behavior;

  { size = n;
    enumerated = List.length enumerated;
    new_size_rules = List.rev !new_size_rules;
    new_kbo_rules = List.rev !new_kbo_rules;
    new_irreducibles = List.rev !new_irreducibles;
  }

let run ?max_size (dom : 'a Domain.t) (num_inputs : int) (max_vars : int) =
  let default_max = match max_size with Some m -> m | None -> 12 in
  let inputs = Eval.generate_inputs dom num_inputs max_vars in
  let rs = create inputs in
  let results = ref [] in
  let n = ref 1 in
  let continue = ref true in
  while !continue && !n <= default_max do
    let summary = run_iteration dom rs !n max_vars in
    if summary.new_size_rules = []
       && summary.new_kbo_rules = []
       && summary.new_irreducibles = [] then
      continue := false
    else begin
      results := summary :: !results;
      incr n
    end
  done;
  (rs, List.rev !results)
