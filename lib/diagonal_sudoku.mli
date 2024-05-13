open Statistics
(** UI/PRINTING METHODS **)

val read_lines : string -> string list
(** Function to read lines from a file into a list *)

val print_string_list : string list -> unit
(** Print each line from a list of strings *)

(** DATA REPRESENTATION **)

(** Type for the set of pairs of ints we use to check whether a cell's value can
    be changed *)
module Pair : sig
  type t = int * int

  val compare : t -> t -> int
end

module PairSet : Set.S with type elt = Pair.t

val cardinality_of_pair_set : PairSet.t -> int

(** Type for the set of ints we use to check whether a row/column is erroneous *)
module Integer : sig
  type t = int

  val compare : t -> t -> int
end

module IntegerSet : Set.S with type elt = Integer.t

val cardinality_of_int_set : IntegerSet.t -> int

val preset_of_csv : string -> int array array * PairSet.t
(** Function to process each cell of the CSV *)

(** GAME-RUNNING METHODS **)

val clear_line : unit -> unit
(** Clears the current line *)

val get_input : PairSet.t -> int * int * int
(** Gets input from the user in the desired format and forbids changing of
    immutable blue cells *)

val get_play_option : unit -> int
(** Gets input from the user in the desired format as an integer 1-4, clearing
    the line and re-prompting otherwise *)

val check_all_rows : int array array -> IntegerSet.t * IntegerSet.t
(** Function to check each row of the sudoku grid and update their color if
    necessary *)

val check_all_cols : int array array -> IntegerSet.t * IntegerSet.t
(** Function to check each column of the sudoku grid and update their color if
    necessary *)

val check_all_boxes : int array array -> PairSet.t * PairSet.t
(** Function to check each box of the sudoku grid and update their color if
    necessary *)

val check_left_diagonal : int array array -> bool * bool
(** Function to check the left diagonal of the sudoku grid and update its color
    if necessary *)

val check_right_diagonal : int array array -> bool * bool
(** Function to check the right diagonal of the sudoku grid and update its color
    if necessary *)

val print_sudoku_grid_d :
  int array array ->
  IntegerSet.t ->
  IntegerSet.t ->
  PairSet.t ->
  bool ->
  bool ->
  IntegerSet.t ->
  IntegerSet.t ->
  PairSet.t ->
  bool ->
  bool ->
  PairSet.t ->
  unit
(** Function to print the sudoku grid *)

val is_valid_move : 'a array array -> int -> int -> 'a -> bool
(** Function to check if a move is valid *)

val find_valid_move : int array array -> int -> int -> (int * int * int) option
(** Function to find a valid move *)

val run_game_d :
  int array array -> PairSet.t -> int -> float -> statistics -> unit
(** Runs the game *)

(** Open AI. "Write the mli file for this code - Chat Conversation". Chat GPT.
    May 2024 *)
