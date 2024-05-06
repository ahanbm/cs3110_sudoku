type t = {
  mutable start_time : float;
  mutable end_time : float;
}

(* Start the timer *)
let start () =
  let t = { start_time = 0.0; end_time = 0.0 } in
  t.start_time <- Unix.gettimeofday ();
  t

(* Stop the timer *)
let stop t =
  t.end_time <- Unix.gettimeofday ();
  t

(* Get the elapsed time in seconds *)
let elapsed_seconds t = t.end_time -. t.start_time

(* Get the elapsed time in milliseconds *)
let elapsed_ms t = int_of_float ((t.end_time -. t.start_time) *. 1000.0)

(** Open AI. "Implement an OCaml timer - Chat Conversation". Chat GPT. May 2024 *)
