(** Module for tracking statistics in a Sudoku game *)

type statistics = {
  mutable games_played : int;  (** Number of games played *)
  mutable total_time : float;  (** Number of games won *)
}
(** Type representing game statistics *)

val reset_statistics : unit -> statistics
(** [reset_stats stats] resets all statistics to their initial values *)

val update_statistics : statistics -> float -> unit
(** [update_stats stats won moves] updates the statistics based on game outcome *)

val display_statistics : statistics -> unit
(** [display_stats stats] prints the statistics to the console *)

(** Open AI. "Write the mli file for this code - Chat Conversation". Chat GPT.
    May 2024 *)
