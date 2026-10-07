(* [fingerprint] must agree on equivalent terms; [equivalent] decides equivalence. *)
type 'f t = { fingerprint : 'f Term.t -> int; equivalent : 'f Term.t -> 'f Term.t -> bool }
