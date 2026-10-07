let rec product = function
  | [] -> Seq.return []
  | xs :: rest -> Seq.flat_map (fun x -> Seq.map (List.cons x) (product rest)) xs

let increasing k n =
  let rec go k lo =
    if k = 0 then Seq.return []
    else
      Seq.init (max 0 (n - k - lo + 1)) (( + ) lo)
      |> Seq.flat_map (fun i -> Seq.map (List.cons i) (go (k - 1) (i + 1)))
  in
  go k 0

let injections k n =
  let rec go k used =
    if k = 0 then Seq.return []
    else
      Seq.init n Fun.id
      |> Seq.filter (fun i -> not (List.mem i used))
      |> Seq.flat_map (fun i -> Seq.map (List.cons i) (go (k - 1) (i :: used)))
  in
  go k []

let rec compositions total parts =
  if parts = 0 then if total = 0 then Seq.return [] else Seq.empty
  else
    Seq.init (max 0 (total - parts + 1)) succ
    |> Seq.flat_map (fun first ->
           Seq.map (List.cons first) (compositions (total - first) (parts - 1)))

let rec power base exp = if exp = 0 then 1 else base * power base (exp - 1)

let index_of x xs =
  let rec go i = function
    | [] -> raise Not_found
    | y :: ys -> if y = x then i else go (i + 1) ys
  in
  go 0 xs
