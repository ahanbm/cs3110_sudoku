type statistics = {
  mutable games_played : int;
  mutable total_time : float;
}

let reset_statistics () = { games_played = 0; total_time = 0. }

let update_statistics stats game_duration =
  stats.games_played <- stats.games_played + 1;
  stats.total_time <- stats.total_time +. game_duration

let display_statistics stats =
  Printf.printf "Statistics:\n";
  Printf.printf "Games played: %d\n" stats.games_played;
  Printf.printf "Total time played: %.2f seconds.\n" stats.total_time;
  if stats.games_played > 0 then
    let average_time_per_game = stats.total_time /. float stats.games_played in
    Printf.printf "Average time per game: %.2f seconds.\n\n" average_time_per_game

(** Open AI. "Implement sudoku statistics - Chat Conversation". Chat GPT. May
    2024 *)
