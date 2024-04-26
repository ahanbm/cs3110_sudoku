(**@authors: Peter Favero pmf66*)
(*To compile, run: 
   dune exec ./bin/main.exe 
 in the terminal *)

open Diagonal_sudoku

let input_path = "data/initial.csv"

(* Initialize the set of cells and grid *)
let sudoku_grid, immutable_cells = preset_of_csv input_path

let () = welcome_user_d 
let () = help_user_d
let () = run_game_d sudoku_grid immutable_cells false 1
