(**@authors: Peter Favero pmf66*)
(*To compile, run:
   dune exec ./bin/main.exe 
  in the terminal *)



(* INSTRUCTION PRINTING *)

let welcome_user_d = print_endline 
"
Welcome to CS3110_diagonal_sudoku!
This program allows you to play diagonal sudoku conveniently and easily in your terminal.

This game comes preloaded with several classic diagonal sudoku puzzles.
You can change the file \"data/initial.txt\" for a custom puzzle of your choice.";;

let help_user_d = print_endline
"
In diagonal sudoku, each row, column, and 3x3 grid must contain the numbers 1-9 with no repeats, in accordance with the standard sudoku rules.
Further, the two main diagonals (from top left to bottom right and from top right to bottom left) must also contain the numbers 1-9 with no repeats.

The game will prompt you to enter values into the sudoku board whenever you are ready.

The game keeps track of how many times you've entered a value as a 'move count.' 
Challenge yourself to solve the sudoku as efficently as possible with a low move count!

You'll enter input in the format \'<row> <col> <number>\' to update the board.
You should enter <row> and <col> as digits 1-9, and <number> as a digit 0-9. <number> being zero means you are clearing your input for that cell.
The first row is the top row, and the first column is the leftmost column. 
If there is a violation of the sudoku rules present in a row, column, diagonal or 3x3 box, the game will color that row, column, diagonal or 3x3 box red.
Similarly, if you complete a row, column, or 3x3 box with all numbers 1...9 with no errors, the game will color that row, column, diagonal, or 3x3 box green.
The cells with blue numbers in them are immutable, as part the initial condition of specifying puzzle of the sudoku board.
If you try to change the immutable blue cells, the game erase your input it but won't count that as a move. 
If there is a formatting error, the game erase your input it but won't count that as a move. 

You can enter \'help\' to recieve these formatting tips at any time, which also won't count towards your moves.
Good luck! You've got this! Happy sudoku-solving!
";;



(* DATA REPRESENTATION *)

(* Type for the set of pairs of ints we use to check wether a cell's value can be changed *)
module Pair = struct
  type t = int * int
  let compare = compare
end
module PairSet = Set.Make(Pair)
let cardinality_of_pair_set set =
  PairSet.fold (fun _ acc -> acc + 1) set 0

(* Type for the set of ints we use to check wether a row/coll is erroneous *)
module Integer = struct 
  type t = int
  let compare = compare
end
module IntegerSet = Set.Make(Integer)
let cardinality_of_int_set set =
  IntegerSet.fold (fun _ acc -> acc + 1) set 0


(* Function to process each cell of the CSV *)
let preset_of_csv filename =
  let sudoku_grid = Array.make_matrix 9 9 0 in
  let immutable_cells = ref PairSet.empty in
  (* Load the CSV file *)
  let data = Csv.load filename in
  (* Iterate over rows with their indices *)
  List.iteri (fun row_index row ->
    (* Iterate over columns with their indices *)
    List.iteri (fun col_index cell ->
      try
        let value = int_of_string cell in
        sudoku_grid.(row_index).(col_index) <- value;
        if value <> 0 then
          immutable_cells := PairSet.add (row_index, col_index) !immutable_cells
      with
      | Failure _ -> Printf.eprintf "Error at Row: %d, Col: %d, Cell value: %s\n" row_index col_index cell
    ) row
  ) data;
  (sudoku_grid, !immutable_cells)

let input_path = "data/initial.txt"

(* Initialize the set of cells and grid *)
let sudoku_grid, immutable_cells = preset_of_csv input_path



(* GAME-RUNNING METHODS *)

(* Clears the current line *)
let clear_line () =
  output_string stdout "\027[A\027[2K";
  flush stdout

(* Gets input from the user in the desired format and forbids changing of immutable blue cells *)
let rec get_input immutable_cells = 
  let input_string = read_line () in
  let regex = Str.regexp "^\\([1-9]\\) \\([1-9]\\) \\([0-9]\\)$" in
  if Str.string_match regex input_string 0 then
    let row = int_of_string (Str.matched_group 1 input_string) in
    let col = int_of_string (Str.matched_group 2 input_string) in
    let number = int_of_string (Str.matched_group 3 input_string) in
    if PairSet.mem (row-1, col-1) immutable_cells then 
      let () = clear_line () in get_input immutable_cells
    else 
      let () = clear_line () in
      let () = print_endline ("Move : " ^ input_string ^ "\n") in 
      (row, col, number)
  else let () = clear_line () in get_input immutable_cells

(* Function to check each row of the sudoku grid and update their color if necessary *)
let check_all_rows sudoku_grid =
  let erroneous_rows = ref IntegerSet.empty in
  let completed_rows = ref IntegerSet.empty in
  for row = 0 to 8 do (* Start row loop *)
    let seen = Hashtbl.create 9 in
    begin (* Start col loop *)
      try 
        for col = 0 to 8 do
          let value = sudoku_grid.(row).(col) in
          if value <> 0 then
            if Hashtbl.mem seen value then
              begin
                erroneous_rows := IntegerSet.add row !erroneous_rows;
                raise Exit  (* break the col loop using Exit *)
              end
            else
              Hashtbl.add seen value true
        done;
      if Hashtbl.length seen = 9 then completed_rows := IntegerSet.add row !completed_rows
      with Exit -> ()  (* Catching the Exit to exit the inner loop *)
    end; (* End col loop *)
  done; (* End row loop *)
  (!erroneous_rows, !completed_rows) (* return values *)

(* Function to check each col of the sudoku grid and update their color if necessary *)
let check_all_cols sudoku_grid =
  let erroneous_cols = ref IntegerSet.empty in
  let completed_cols = ref IntegerSet.empty in
  for col = 0 to 8 do (* Start row loop *)
    let seen = Hashtbl.create 9 in
    begin (* Start col loop *)
      try 
        for row = 0 to 8 do
          let value = sudoku_grid.(row).(col) in
          if value <> 0 then
            if Hashtbl.mem seen value then
              begin
                erroneous_cols := IntegerSet.add col !erroneous_cols;
                raise Exit  (* break the col loop using Exit *)
              end
            else
              Hashtbl.add seen value true
        done;
      if Hashtbl.length seen = 9 then completed_cols := IntegerSet.add col !completed_cols
      with Exit -> ()  (* Catching the Exit to exit the inner loop *)
    end; (* End col loop *)
  done; (* End row loop *)
  (!erroneous_cols, !completed_cols) (* return values *)

(* Function to check each box of the sudoku grid and update their color if necessary *)
let check_all_boxes sudoku_grid =
  let erroneous_boxes = ref PairSet.empty in
  let completed_boxes = ref PairSet.empty in
  for top_row = 0 to 2 do (* Start top_row loop *)
    for top_col = 0 to 2 do (* start top_col loop *)
      let seen = Hashtbl.create 9 in
      begin
        let base_row = top_row * 3 in
        let base_col = top_col * 3 in
        try 
          for row = base_row to base_row+2 do
            for col = base_col to base_col+2 do
              let value = sudoku_grid.(row).(col) in
              if value <> 0 then 
                if Hashtbl.mem seen value then
                  begin
                    erroneous_boxes := PairSet.add (top_row,top_col) !erroneous_boxes;
                    raise Exit  (* break the col loop using Exit *)
                  end
                else
                  Hashtbl.add seen value true
            done;
          done;
        if Hashtbl.length seen = 9 then completed_boxes := PairSet.add (top_row,top_col) !completed_boxes
        with Exit -> ()  (* Catching the Exit to exit the inner loop *)
      end
    done; (* start top_col loop *)
  done; (* End top_row loop *)
  (!erroneous_boxes, !completed_boxes) (* return values *)

(* Function to check the left diagonal of the sudoku grid and update its color if necessary *)
let check_left_diagonal sudoku_grid =
  let erroneous = ref false in
  let seen = Hashtbl.create 9 in
  for row_and_col = 0 to 8 do (* Start row and col loop *)
    begin (* Start col loop *)
      try 
          let value = sudoku_grid.(row_and_col).(row_and_col) in
          if value <> 0 then
            if Hashtbl.mem seen value then
              begin
                erroneous := false;
                raise Exit  (* break the col loop using Exit *)
              end
            else
              Hashtbl.add seen value true;
      with Exit -> ()  (* Catching the Exit to exit the inner loop *)
    end;
  done; (* End row and col loop *)
  (!erroneous, (Hashtbl.length seen = 9))

(* Function to check the left diagonal of the sudoku grid and update its color if necessary *)
let check_right_diagonal sudoku_grid =
  let erroneous = ref false in
  let seen = Hashtbl.create 9 in
  for row_and_col = 0 to 8 do (* Start row and col loop *)
    begin (* Start col loop *)
      try 
          let value = sudoku_grid.(row_and_col).(8-row_and_col) in
          if value <> 0 then
            if Hashtbl.mem seen value then
              begin
                erroneous := false;
                raise Exit  (* break the col loop using Exit *)
              end
            else
              Hashtbl.add seen value true;
      with Exit -> ()  (* Catching the Exit to exit the inner loop *)
    end;
  done; (* End row and col loop *)
  (!erroneous, (Hashtbl.length seen = 9))
  
(* This code was written using information from GPT-4 *)
(* Function to print the sudoku grid *)
let print_sudoku_grid_d grid erroneous_rows erroneous_cols erroneous_boxes ld_erroneous rd_erroneous completed_rows completed_cols completed_boxes ld_complete rd_complete immutable_cells =
  for row = 0 to 8 do
    if row mod 3 = 0 then print_endline "-------------------------";
    for col = 0 to 8 do
      if col mod 3 = 0 then print_string "| ";
      let value = grid.(row).(col) in
      let color = 
        if PairSet.mem (row,col) immutable_cells then "blue" 
        else if IntegerSet.mem row erroneous_rows then "red"
        else if IntegerSet.mem col erroneous_cols then "red" 
        else if PairSet.mem (row/3, col/3) erroneous_boxes then "red"
        else if (row = col && ld_erroneous) then "red" 
        else if (row = 8-col && rd_erroneous) then "red"
        else if IntegerSet.mem row completed_rows then "green" 
        else if IntegerSet.mem col completed_cols then "green" 
        else if (row = col && ld_complete) then "green" 
        else if (row = 8-col && rd_complete) then "green"
        else if PairSet.mem (row/3, col/3) completed_boxes then "green"
        else "none"
      in
      match color with
      | "blue" -> (* current value is immutable *)
          if value = 0 then 
            Printf.printf "\027[34m. \027[0m"
          else 
            Printf.printf "\027[34m%d \027[0m" value
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
let rec run_game_d (sudoku_grid : int array array) (immutable_cells : PairSet.t) (grid_solved : bool) (move_count : int) =
  match grid_solved with 
  | true -> print_endline ("Congrats, you won!\nYou solved this sudoku puzzle in " ^ (string_of_int move_count) ^ " moves!\nGreat job! We encourage you to play another exciting game of sudoku.\n");
  | false -> 
    print_endline ("Sudoku Board | Move " ^ (string_of_int (move_count-1)) ^ " :");
    let (erroneous_rows, completed_rows) = check_all_rows sudoku_grid in 
    let (erroneous_cols, completed_cols) = check_all_cols sudoku_grid in 
    let (erroneous_boxes, completed_boxes) = check_all_boxes sudoku_grid in
    let (ld_erroneous, ld_complete) = check_left_diagonal sudoku_grid in
    let (rd_erroneous, rd_complete) = check_right_diagonal sudoku_grid in
    print_sudoku_grid_d sudoku_grid erroneous_rows erroneous_cols erroneous_boxes ld_erroneous rd_erroneous completed_rows completed_cols completed_boxes ld_complete rd_complete immutable_cells;
    let (row, col, number) = get_input immutable_cells in
    sudoku_grid.(row-1).(col-1) <- number;
    run_game_d sudoku_grid immutable_cells ((cardinality_of_int_set completed_rows = 9) && (cardinality_of_int_set completed_cols = 9) && (cardinality_of_pair_set completed_boxes = 9) && ld_complete && rd_complete) (move_count+1);
   

(* MAIN METHOD *)    

welcome_user_d;;
help_user_d;;
run_game_d sudoku_grid immutable_cells false 1;;