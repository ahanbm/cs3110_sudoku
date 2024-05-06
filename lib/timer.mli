(* timer.mli *)

(** Timer module for measuring elapsed time *)

type t
(** Type representing a timer *)

val start : unit -> t
(** [start ()] starts the timer *)

val stop : t -> t
(** [stop t] stops the timer *)

val elapsed_seconds : t -> float
(** [elapsed_seconds t] returns the elapsed time in seconds *)

val elapsed_ms : t -> int
(** [elapsed_ms t] returns the elapsed time in milliseconds *)

(** Open AI. "Write the mli file for this code - Chat Conversation". Chat GPT.
    May 2024 *)
