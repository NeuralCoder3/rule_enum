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
  total_size_rules : int;
  total_kbo_rules : int;
  total_irreducible : int;
}

type 'a term_decision =
  | D_size_rule of Types.rule
  | D_kbo_rule of Types.rule
  | D_replace of Types.term * Types.term * 'a list
  | D_skip
  | D_candidate of Types.term * 'a list

let process_term (dom : 'a Domain.t) ~inputs ~all_rules ~behaviors t =
  let simplified, size_reduced = Rewrite.normalize all_rules t in
  if size_reduced then None
  else
    let bv = Eval.behavior dom inputs simplified in
    let rec search = function
      | [] -> Some (D_candidate (simplified, bv))
      | (irr, irr_bv) :: rest ->
        if list_equal dom.Domain.equal bv irr_bv then
          let irr_sz = Types.size irr in
          let t_sz = Types.size simplified in
          if irr_sz < t_sz then
            Some (D_size_rule (simplified, irr))
          else if irr_sz = t_sz then
            match Kbo.kbo_compare simplified irr with
            | 0 -> Some D_skip
            | c when c > 0 -> Some (D_kbo_rule (simplified, irr))
            | _ -> Some (D_replace (irr, simplified, bv))
          else
            None
        else
          search rest
    in
    search behaviors

let parallel_map ~num_domains f lst =
  let len = List.length lst in
  if num_domains <= 1 || len < num_domains * 2 then
    List.filter_map f lst
  else
    let chunk_size = (len + num_domains - 1) / num_domains in
    let rec take n acc = function
      | [] -> (List.rev acc, [])
      | rest when n <= 0 -> (List.rev acc, rest)
      | x :: xs -> take (n - 1) (x :: acc) xs
    in
    let rec split acc = function
      | [] -> List.rev acc
      | rest ->
        let chunk, remaining = take chunk_size [] rest in
        split (chunk :: acc) remaining
    in
    let chunks = split [] lst in
    let domains = List.map (fun chunk ->
      Stdlib.Domain.spawn (fun () -> List.filter_map f chunk)
    ) chunks in
    let results = List.map Stdlib.Domain.join domains in
    List.concat results

let apply_decisions (dom : 'a Domain.t) (rs : 'a rule_sets)
      (decisions : 'a term_decision list) =
  let new_size_rules = ref [] in
  let new_kbo_rules = ref [] in
  let candidates = ref [] in
  List.iter (function
    | D_size_rule rule ->
      rs.size_rules <- rule :: rs.size_rules;
      new_size_rules := rule :: !new_size_rules
    | D_kbo_rule rule ->
      rs.kbo_rules <- rule :: rs.kbo_rules;
      new_kbo_rules := rule :: !new_kbo_rules
    | D_replace (_old_irr, new_term, stored_bv) ->
      let idx = ref (-1) in
      List.iteri (fun i (_, b) ->
        if !idx = -1 && list_equal dom.Domain.equal stored_bv b then idx := i
      ) rs.behaviors;
      (match !idx with
       | -1 -> ()
       | i ->
         let current_irr = fst (List.nth rs.behaviors i) in
         match Kbo.kbo_compare new_term current_irr with
         | 0 -> ()
         | c when c > 0 ->
           rs.kbo_rules <- (new_term, current_irr) :: rs.kbo_rules;
           new_kbo_rules := (new_term, current_irr) :: !new_kbo_rules
         | _ ->
           let rule = (current_irr, new_term) in
           rs.kbo_rules <- rule :: rs.kbo_rules;
           new_kbo_rules := rule :: !new_kbo_rules;
           rs.behaviors <- List.mapi (fun j (t', b) ->
             if j = i then (new_term, b) else (t', b)
           ) rs.behaviors;
           rs.irreducible <- List.mapi (fun j t' ->
             if j = i then new_term else t'
           ) rs.irreducible)
    | D_skip -> ()
    | D_candidate (t, bv) ->
      candidates := (t, bv) :: !candidates
  ) decisions;
  (!new_size_rules, !new_kbo_rules, !candidates)

let run_iteration (dom : 'a Domain.t) (rs : 'a rule_sets) (n : int)
      (max_vars : int) ~num_domains : iter_summary =
  let enumerated = Enum.enumerate_terms dom.Domain.signature rs.irreducible n max_vars in

  let inputs = rs.inputs in
  let all_rules_list = all_rules rs in
  let behaviors = rs.behaviors in

  let decisions =
    parallel_map ~num_domains
      (process_term dom ~inputs ~all_rules:all_rules_list ~behaviors)
      enumerated
  in

  let new_kbo_rules = ref [] in
  let new_size_rules, _kbo_from_decisions, candidates = apply_decisions dom rs decisions in
  new_kbo_rules := _kbo_from_decisions;

  let new_irreducibles = ref [] in
  let cmp = list_compare dom.Domain.compare in
  let by_behavior = group_by cmp snd candidates in
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
    new_size_rules = List.rev new_size_rules;
    new_kbo_rules = List.rev !new_kbo_rules;
    new_irreducibles = List.rev !new_irreducibles;
    total_size_rules = List.length rs.size_rules;
    total_kbo_rules = List.length rs.kbo_rules;
    total_irreducible = List.length rs.irreducible;
  }

let run ?max_size ?(forced_inputs = []) ?(on_iteration = fun _ -> ())
      ?(num_domains = 0) (dom : 'a Domain.t) ~num_random_inputs ~max_vars =
  let default_max = match max_size with Some m -> m | None -> 12 in
  let random_inputs =
    if num_random_inputs > 0
    then Eval.generate_inputs dom num_random_inputs max_vars
    else [] in
  let inputs = forced_inputs @ random_inputs in
  if inputs = [] then
    failwith "Algorithm.run: no inputs provided (num_random_inputs=0 and no forced_inputs)";
  let rs = create inputs in
  let num_cpu = try Stdlib.Domain.recommended_domain_count () with _ -> 1 in
  let num_threads = if num_domains > 0 then num_domains else num_cpu in
  let results = ref [] in
  let n = ref 1 in
  let continue = ref true in
  while !continue && !n <= default_max do
    let summary = run_iteration dom rs !n max_vars ~num_domains:num_threads in
    if summary.new_size_rules = []
       && summary.new_kbo_rules = []
       && summary.new_irreducibles = [] then
      continue := false
    else begin
      on_iteration summary;
      results := summary :: !results;
      incr n
    end
  done;
  (rs, List.rev !results)
