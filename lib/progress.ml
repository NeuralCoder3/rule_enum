(* Optional progress reporting for long iterations (--progress).

   An iteration has two phases that each take real time at large sizes:
   enumeration (building the term list — total unknown until it finishes)
   and processing (one `process_term` per enumerated term — total known).

   `start ~total:0` begins an INDETERMINATE phase (a spinner + live count,
   used for enumeration); `start ~total:n` begins a DETERMINATE phase (a
   percentage bar, used for processing). `tick` is called once per unit of
   work and redraws a `\r` line on stderr at most ~10×/sec, guarded by a
   non-blocking try_lock so parallel workers never stall on it.

   Only active on a TTY, so redirected/piped output is never spammed.
   `active` is set by `start` (before any parallel section) and cleared by
   `finish`, so worker domains only ever read it. *)

let enabled = ref false
let active = ref false
let count = Atomic.make 0
let total = ref 0          (* exact denominator (processing phase) *)
let estimate = ref 0       (* approximate denominator (enumeration phase) *)
let label = ref ""
let mutex = Mutex.create ()
let last = ref 0.0
let spin = ref 0
let is_tty = lazy (try Unix.isatty Unix.stderr with _ -> false)

let spinner = [| '|'; '/'; '-'; '\\' |]

let render () =
  let done_ = Atomic.get count in
  if !total > 0 then begin
    (* Determinate: exact percentage bar. *)
    let tot = !total in
    let d = min done_ tot in
    let frac = float_of_int d /. float_of_int tot in
    let w = 30 in
    let f = int_of_float (frac *. float_of_int w) in
    Printf.eprintf "\r  [%s%s] %3d%%  %d/%d  %s%!"
      (String.make f '#') (String.make (max 0 (w - f)) '.')
      (int_of_float (frac *. 100.0)) d tot !label
  end else begin
    (* Indeterminate (total unknown): spinner + live count, plus an
       approximate percentage when an estimate is available. *)
    spin := (!spin + 1) land 3;
    if !estimate > 0 then
      Printf.eprintf "\r  [%c] %s  %d (~%d%%)%!"
        spinner.(!spin) !label done_ (min 99 (done_ * 100 / !estimate))
    else
      Printf.eprintf "\r  [%c] %s  %d%!" spinner.(!spin) !label done_
  end

(* Begin a phase. total=0 ⇒ indeterminate (spinner, with optional ~est
   for an approximate %); total>0 ⇒ exact bar. *)
let start ?(est = 0) ~label:l ~total:t () =
  if !enabled && Lazy.force is_tty then begin
    Atomic.set count 0; total := t; estimate := est; label := l;
    last := 0.0; active := true;
    render ()
  end

(* One unit of work. On the hot path this is a single boolean read when
   progress is inactive. *)
let tick () =
  if !active then begin
    Atomic.incr count;
    if Mutex.try_lock mutex then begin
      let now = Unix.gettimeofday () in
      if now -. !last > 0.1 then (last := now; render ());
      Mutex.unlock mutex
    end
  end

let finish () =
  if !active then begin
    active := false;
    Printf.eprintf "\r%*s\r%!" 72 ""   (* clear the line *)
  end
