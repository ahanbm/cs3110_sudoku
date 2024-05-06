(**@authors: Peter Favero pmf66, Ahan Mishra abm247 *)
(*To compile, run: dune exec ./bin/main.exe in the terminal *)

open Cs3110_sudoku
include Diagonal_sudoku

let initialize () =
  let welcome_user_d_path = "data/private/welcome_user_d.txt" in
  let welcome_user_d () =
    let lines = read_lines welcome_user_d_path in
    print_string_list lines
  in

  let help_user_d_path = "data/private/help_user_d.txt" in
  let help_user_d () =
    let lines = read_lines help_user_d_path in
    print_string_list lines
  in

  (welcome_user_d, help_user_d)

let sudoku () =
  let welcome_user_d, help_user_d = initialize () in
  let input_path = "data/initial.csv" in

  (* Initialize the set of cells and grid *)
  let sudoku_grid, immutable_cells = preset_of_csv input_path in

  let () = welcome_user_d () in
  let () = help_user_d () in
  let () = run_game_d sudoku_grid immutable_cells false 1 in
  ()

let () = sudoku ()
