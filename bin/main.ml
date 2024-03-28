(**@author: Peter Favero pmf66*)
(*To compile, run:
   dune exec ./bin/main.exe 
  in the terminal *)

(* Initialize a 9x9 array with zeros *)
let sudoku_grid = Array.make_matrix 9 9 0;;

let welcome_user = print_endline 
"
Welcome to CS3110_sudoku!
This program allows you can play sudoku conveniently and easily in your terminal.
This game comes preloaded with several classic sudoku puzzles.
You can changing the file \"data/initial.txt\" for a custom puzzle of your choice.
";;

let help_user = print_endline
"
The game will prompt you to enter values into the sudoku board whenever you are ready.
The game keeps track of how many times you've entered a value as a 'move count.' 
Challenge yourself to solve the sudoku as efficently as possible with a low move count!
You'll enter input in the format \'<row> <col> <number>\' to update the board.
You should enter <row>, <col>, and <number> as digits 1-9.
The first row is the top row, and the first column is the leftmost column. 
If there is a formatting error, the game will help you correct it but won't count that move. 
You can enter \'help\' to recieve helpful formatting tips at any time.
Good luck! You've got this! Happy sudoku-solving!
";;

let clear_line () =
  output_string stdout "\027[A\027[2K";
  flush stdout

let rec get_input () = 
  let input_string = read_line () in
  let regex = Str.regexp "^\\([1-9]\\) \\([1-9]\\) \\([1-9]\\)$" in
  if Str.string_match regex input_string 0 then
    let row = int_of_string (Str.matched_group 1 input_string) in
    let col = int_of_string (Str.matched_group 2 input_string) in
    let number = int_of_string (Str.matched_group 3 input_string) in
    let () = clear_line () in
    let () = print_endline ("Move : " ^ input_string ^ "\n") in 
    (row, col, number)
  else let () = clear_line () in get_input ()

(* This code was written using information from GPT-4 *)
(* Function to print the sudoku grid *)
let print_sudoku_grid grid =
  for i = 0 to 8 do
    (* Print horizontal dividers *)
    if i mod 3 = 0 then print_endline "-------------------------";
    for j = 0 to 8 do
      (* Print vertical dividers *)
      if j mod 3 = 0 then print_string "| ";
      (* Print cell value or space for zero *)
      if grid.(i).(j) = 0 then print_string ". "
      else Printf.printf "%d " grid.(i).(j);
    done;
    print_endline "|";
  done;
  print_endline "-------------------------";;

let rec run_game (sudoku_grid : int array array) (grid_solved : bool) (move_count : int) =
  match grid_solved with 
  | true -> print_endline ("Congrats, you won!\nYou solved this sudoku puzzle in " ^ (string_of_int move_count) ^ " moves!\nGreat job! We encourage you to play another exciting game of sudoku.\n");
  | false -> 
    print_endline ("Sudoku Board | Move " ^ (string_of_int move_count) ^ " :");
    print_sudoku_grid sudoku_grid;
    let (row, col, number) = get_input () in
    sudoku_grid.(row-1).(col-1) <- number;
    run_game sudoku_grid (row+col == number) (move_count+1);

(* Execute the print function to display the grid *)
welcome_user;;
help_user;;
run_game sudoku_grid false 1;;