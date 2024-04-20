(**@authors: Peter Favero pmf66*)
(*To compile, run:
   dune exec ./bin/main.exe 
  in the terminal *)

  exception Break

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
The cells with blue numbers in them are immutable, as part the initial condition of specifying puzzle of the sudoku board.
If you try to change the immutable blue cells, the game erase your input it but won't count that as a move. 
If there is a formatting error, the game erase your input it but won't count that as a move. 
You can enter \'help\' to recieve helpful formatting tips at any time, which also won't count towards your moves.
Good luck! You've got this! Happy sudoku-solving!
";;


(* Type for the set of cells we use to check wether a value can be changed *)
module Cell = struct
  type t = int * int
  let compare = compare
end

module CellSet = Set.Make(Cell);;

(* Initialize the set of cells and grid with one immutable value for testing *)
let immutable_cells = CellSet.empty;;
let immutable_cells = CellSet.add (2,2) immutable_cells;;

let sudoku_grid = Array.make_matrix 9 9 (0, "none");;
sudoku_grid.(2).(2) <- (9, "blue")

(* Clears the current line *)
let clear_line () =
  output_string stdout "\027[A\027[2K";
  flush stdout

(* Gets input from the user in the desired format and forbids changing of immutable blue cells *)
let rec get_input immutable_cells = 
  let input_string = read_line () in
  let regex = Str.regexp "^\\([1-9]\\) \\([1-9]\\) \\([1-9]\\)$" in
  if Str.string_match regex input_string 0 then
    let row = int_of_string (Str.matched_group 1 input_string) in
    let col = int_of_string (Str.matched_group 2 input_string) in
    let number = int_of_string (Str.matched_group 3 input_string) in
    if CellSet.mem (row-1, col-1) immutable_cells then 
      let () = clear_line () in get_input immutable_cells
    else 
      let () = clear_line () in
      let () = print_endline ("Move : " ^ input_string ^ "\n") in 
      (row, col, number)
  else let () = clear_line () in get_input immutable_cells

(* Function to check each row of the sudoku grid and update their color if necessary *)
  let check_all_rows sudoku_grid =
    let erroneous_rows = ref [] in
    let completed_rows = ref [] in
    for row = 0 to 8 do (* Start row loop *)
      let seen = Hashtbl.create 9 in
      begin (* Start col loop *)
        try 
          for col = 0 to 8 do
            let value = sudoku_grid.(row).(col) in
            if value <> 0 then
              if Hashtbl.mem seen value then
                begin
                  erroneous_rows := row :: !erroneous_rows;
                  raise Break  (* break the col loop *)
                end
              else
                Hashtbl.add seen value true
          done
        with Break -> ()  (* Catching the Break to exit the inner loop *)
      end; (* End col loop *)
      if (Hashtbl.length seen = 9) then completed_rows := row :: !completed_rows
    done; (* End row loop *)
    (!erroneous_rows, !completed_rows) (* return values *)

(* This code was written using information from GPT-4 *)
(* Function to print the sudoku grid *)
let print_sudoku_grid grid =
  for i = 0 to 8 do
    if i mod 3 = 0 then print_endline "-------------------------";
    for j = 0 to 8 do
      if j mod 3 = 0 then print_string "| ";
      let (value, color) = grid.(i).(j) in
      match color with
      | "red" -> (* error in the current group *)
          if value = 0 then 
            Printf.printf "\027[31m. \027[0m"
          else 
            Printf.printf "\027[31m%d \027[0m" value
      | "green" -> (* current group is full and without error *)
          if value = 0 then 
            Printf.printf "\027[32m. \027[0m"
          else 
            Printf.printf "\027[32m%d \027[0m" value
      | "blue" -> (* current value is immutable *)
          if value = 0 then 
            Printf.printf "\027[34m. \027[0m"
          else 
            Printf.printf "\027[34m%d \027[0m" value
      | _ ->  (* Treat all other colors as "none", but only "none" should be used. *)
          if value = 0 then 
            print_string ". "
          else 
            Printf.printf "%d " value
    done;
    print_endline "|";
  done;
  print_endline "-------------------------";;

(* Runs game *)
let rec run_game (sudoku_grid : (int*string) array array) (grid_solved : bool) (move_count : int) =
  match grid_solved with 
  | true -> print_endline ("Congrats, you won!\nYou solved this sudoku puzzle in " ^ (string_of_int move_count) ^ " moves!\nGreat job! We encourage you to play another exciting game of sudoku.\n");
  | false -> 
    print_endline ("Sudoku Board | Move " ^ (string_of_int (move_count-1)) ^ " :");
    print_sudoku_grid sudoku_grid;
    let (row, col, number) = get_input immutable_cells in
    sudoku_grid.(row-1).(col-1) <- (number, (snd sudoku_grid.(row-1).(col-1)));
    run_game sudoku_grid (row+col == number) (move_count+1);

(* Execute the print function to display the grid *)
welcome_user;;
help_user;;
run_game sudoku_grid false 1;;