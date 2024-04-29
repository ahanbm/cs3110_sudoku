(**@authors: Peter Favero pmf66*)
(*To compile, run: dune exec ./bin/main.exe in the terminal *)

open Cs3110_sudoku
include Diagonal_sudoku
include Connect_four

let connect4 () =
  let welcome_user_c_path = "data/private/welcome_user_c.txt" in
  let welcome_user_c =
    let lines = read_lines welcome_user_c_path in
    print_string_list lines
  in

  let help_user_c_path = "data/private/help_user_c.txt" in
  let help_user_c =
    let lines = read_lines help_user_c_path in
    print_string_list lines
  in

  let () = welcome_user_c in
  let () = help_user_c in
  connect_four ()

let sudoku () =
  let welcome_user_d_path = "data/private/welcome_user_d.txt" in
  let welcome_user_d =
    let lines = read_lines welcome_user_d_path in
    print_string_list lines
  in

  let help_user_d_path = "data/private/help_user_d.txt" in
  let help_user_d =
    let lines = read_lines help_user_d_path in
    print_string_list lines
  in

  let input_path = "data/initial.csv" in

  (* Initialize the set of cells and grid *)
  let sudoku_grid, immutable_cells = preset_of_csv input_path in

  let () = welcome_user_d in
  let () = help_user_d in
  let () = run_game_d sudoku_grid immutable_cells false 1 in
  ()

let args = Sys.argv
let len = Array.length args

let () =
  if len = 1 then
    print_endline
      "No arguments given. Please enter either 'sudoku' or 'connect4' to play \
       the respective game."
  else if len > 2 then
    print_endline
      "Too many arguments given. Please enter either 'sudoku' or 'connect4' to \
       play the respective game."
  else
    let game = args.(1) in
    if game = "sudoku" then sudoku ()
    else if game = "connect4" then connect4 ()
    else
      print_endline
        "Invalid argument. Please enter either 'sudoku' or 'connect4' to play \
         the respective game."
