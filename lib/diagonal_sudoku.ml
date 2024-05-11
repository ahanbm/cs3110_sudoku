(*To run the main method, use: dune exec ./bin/main.exe in the terminal *)

include Statistics

let read_lines filename =
  let in_channel = open_in filename in
  let rec read_lines_helper acc =
    try
      let line = input_line in_channel in
      read_lines_helper (line :: acc)
    with End_of_file ->
      close_in in_channel;
      List.rev acc
  in
  read_lines_helper []

let print_string_list lst = List.iter print_endline lst

module Pair = struct
  type t = int * int

  let compare = compare
end

module PairSet = Set.Make (Pair)

let cardinality_of_pair_set set = PairSet.fold (fun _ acc -> acc + 1) set 0

module Integer = struct
  type t = int

  let compare = compare
end

module IntegerSet = Set.Make (Integer)

let cardinality_of_int_set set = IntegerSet.fold (fun _ acc -> acc + 1) set 0

let preset_of_csv filename =
  let sudoku_grid = Array.make_matrix 9 9 0 in
  let immutable_cells = ref PairSet.empty in
  let data = Csv.load filename in
  List.iteri
    (fun row_index row ->
      List.iteri
        (fun col_index cell ->
          try
            let value = int_of_string cell in
            sudoku_grid.(row_index).(col_index) <- value;
            if value <> 0 then
              immutable_cells :=
                PairSet.add (row_index, col_index) !immutable_cells
          with Failure _ ->
            Printf.eprintf "Error at Row: %d, Col: %d, Cell value: %s\n"
              row_index col_index cell)
        row)
    data;
  (sudoku_grid, !immutable_cells)

let clear_line () =
  output_string stdout "\027[A\027[2K";
  flush stdout

let rec get_input immutable_cells =
  let input_string = read_line () in
  let regex = Str.regexp "^\\([1-9]\\) \\([1-9]\\) \\([0-9]\\)$" in
  if Str.string_match regex input_string 0 then
    let row = int_of_string (Str.matched_group 1 input_string) in
    let col = int_of_string (Str.matched_group 2 input_string) in
    let number = int_of_string (Str.matched_group 3 input_string) in
    if PairSet.mem (row - 1, col - 1) immutable_cells then
      let () = clear_line () in
      get_input immutable_cells
    else
      let () = clear_line () in
      let () = print_endline ("Move : " ^ input_string ^ "\n") in
      (row, col, number)
  else
    let () = clear_line () in
    get_input immutable_cells

let rec get_play_option () =
  let input_string = read_line () in
  let regex = Str.regexp "^[1-4]$" in
  if Str.string_match regex input_string 0 then
    let number = int_of_string input_string in
    let () = clear_line () in
    let () = print_endline ("Input: (" ^ input_string ^ ")") in
    number
  else
    let () = clear_line () in
    get_play_option ()

(* this method was written using information from GPT-4, accessed Friday, April
   26th, 2024 *)
let check_all_rows sudoku_grid =
  let erroneous_rows = ref IntegerSet.empty in
  let completed_rows = ref IntegerSet.empty in
  (* Iterate over each row as an individual int array *)
  Array.iteri
    (fun row_index row_array ->
      let seen = Hashtbl.create 9 in
      begin
        try
          (* Iterate over each element in the row *)
          Array.iteri
            (fun _ value ->
              if value <> 0 then
                if Hashtbl.mem seen value then begin
                  erroneous_rows := IntegerSet.add row_index !erroneous_rows;
                  raise Exit (* Break the iteration using Exit *)
                end
                else Hashtbl.add seen value true)
            row_array;
          (* Check if the row is complete *)
          if Hashtbl.length seen = 9 then
            completed_rows := IntegerSet.add row_index !completed_rows
        with Exit -> () (* Catching the Exit to exit the inner loop *)
      end)
    sudoku_grid;

  (!erroneous_rows, !completed_rows)
(* return values *)

let check_all_cols sudoku_grid =
  let erroneous_cols = ref IntegerSet.empty in
  let completed_cols = ref IntegerSet.empty in
  for col = 0 to 8 do
    (* Start row loop *)
    let seen = Hashtbl.create 9 in
    begin
      (* Start col loop *)
      try
        for row = 0 to 8 do
          let value = sudoku_grid.(row).(col) in
          if value <> 0 then
            if Hashtbl.mem seen value then begin
              erroneous_cols := IntegerSet.add col !erroneous_cols;
              raise Exit (* break the col loop using Exit *)
            end
            else Hashtbl.add seen value true
        done;
        if Hashtbl.length seen = 9 then
          completed_cols := IntegerSet.add col !completed_cols
      with Exit -> () (* Catching the Exit to exit the inner loop *)
    end
    (* End col loop *)
  done;
  (* End row loop *)
  (!erroneous_cols, !completed_cols)
(* return values *)

let check_all_boxes sudoku_grid =
  let erroneous_boxes = ref PairSet.empty in
  let completed_boxes = ref PairSet.empty in
  for top_row = 0 to 2 do
    (* Start top_row loop *)
    for top_col = 0 to 2 do
      (* start top_col loop *)
      let seen = Hashtbl.create 9 in
      begin
        let base_row = top_row * 3 in
        let base_col = top_col * 3 in
        try
          for row = base_row to base_row + 2 do
            for col = base_col to base_col + 2 do
              let value = sudoku_grid.(row).(col) in
              if value <> 0 then
                if Hashtbl.mem seen value then begin
                  erroneous_boxes :=
                    PairSet.add (top_row, top_col) !erroneous_boxes;
                  raise Exit (* break the col loop using Exit *)
                end
                else Hashtbl.add seen value true
            done
          done;
          if Hashtbl.length seen = 9 then
            completed_boxes := PairSet.add (top_row, top_col) !completed_boxes
        with Exit -> () (* Catching the Exit to exit the inner loop *)
      end
    done
    (* start top_col loop *)
  done;
  (* End top_row loop *)
  (!erroneous_boxes, !completed_boxes)
(* return values *)

let check_diagonal sudoku_grid checking_left_diagonal =
  let erroneous = ref false in
  let seen = Hashtbl.create 9 in
  for row_and_col = 0 to 8 do
    (* Start row and col loop *)
    begin
      (* Start col loop *)
      try
        let value =
          sudoku_grid.(row_and_col).(if checking_left_diagonal then row_and_col
                                     else 8 - row_and_col)
        in
        if value <> 0 then
          if Hashtbl.mem seen value then begin
            erroneous := true;
            raise Exit (* break the col loop using Exit *)
          end
          else Hashtbl.add seen value true
      with Exit -> () (* Catching the Exit to exit the inner loop *)
    end
  done;
  (* End row and col loop *)
  (!erroneous, Hashtbl.length seen = 9)

let check_left_diagonal sudoku_grid = check_diagonal sudoku_grid true
let check_right_diagonal sudoku_grid = check_diagonal sudoku_grid false

(* This code was written using information from GPT-4 *)
let print_sudoku_grid_d grid erroneous_rows erroneous_cols erroneous_boxes
    ld_erroneous rd_erroneous completed_rows completed_cols completed_boxes
    ld_complete rd_complete immutable_cells =
  for row = 0 to 8 do
    if row mod 3 = 0 then print_endline "-------------------------";
    for col = 0 to 8 do
      if col mod 3 = 0 then print_string "| ";
      let value = grid.(row).(col) in
      let color =
        if PairSet.mem (row, col) immutable_cells then "blue"
        else if IntegerSet.mem row erroneous_rows then "red"
        else if IntegerSet.mem col erroneous_cols then "red"
        else if PairSet.mem (row / 3, col / 3) erroneous_boxes then "red"
        else if row = col && ld_erroneous then "red"
        else if row = 8 - col && rd_erroneous then "red"
        else if IntegerSet.mem row completed_rows then "green"
        else if IntegerSet.mem col completed_cols then "green"
        else if row = col && ld_complete then "green"
        else if row = 8 - col && rd_complete then "green"
        else if PairSet.mem (row / 3, col / 3) completed_boxes then "green"
        else "none"
      in
      match color with
      | "blue" ->
          (* current value is immutable *)
          if value = 0 then Printf.printf "\027[34m. \027[0m"
          else Printf.printf "\027[34m%d \027[0m" value
      | "red" ->
          (* error in the current group *)
          if value = 0 then Printf.printf "\027[31m. \027[0m"
          else Printf.printf "\027[31m%d \027[0m" value
      | "green" ->
          (* current group is full and without error *)
          if value = 0 then Printf.printf "\027[32m. \027[0m"
          else Printf.printf "\027[32m%d \027[0m" value
      | _ ->
          (* Treat all other colors as "none", but only "none" should be
             used. *)
          if value = 0 then print_string ". " else Printf.printf "%d " value
    done;
    print_endline "|"
  done;
  print_endline "-------------------------"

(* Function to check if a move is valid in a Sudoku grid *)
let is_valid_move sudoku_grid row col number =
  (* Check if the number is already present in the same row *)
  let row_valid =
    not (Array.exists (fun value -> value = number) sudoku_grid.(row))
  in
  (* Check if the number is already present in the same column *)
  let col_valid =
    not (Array.exists (fun row_array -> row_array.(col) = number) sudoku_grid)
  in
  (* Check if the number is already present in the same 3x3 box *)
  let box_valid =
    let top_row = row / 3 * 3 in
    let top_col = col / 3 * 3 in
    let rec check_box r c =
      if r = top_row + 3 then true
      else if c = top_col + 3 then check_box (r + 1) top_col
      else if sudoku_grid.(r).(c) <> number then check_box r (c + 1)
      else false
    in
    check_box top_row top_col
  in
  row_valid && col_valid && box_valid

(* Function to find a valid move for the next cell *)
let rec find_valid_move sudoku_grid row col =
  if row = 9 then None (* Reached end of grid, no valid move found *)
  else if sudoku_grid.(row).(col) = 0 then
    (* Cell is empty, check if any valid move exists *)
    let rec find_valid_number number =
      if number > 9 then None (* No valid number found *)
      else if is_valid_move sudoku_grid row col number then
        Some (row, col, number) (* Found a valid move *)
      else find_valid_number (number + 1)
    in
    find_valid_number 1
  else if col = 8 then find_valid_move sudoku_grid (row + 1) 0
  else find_valid_move sudoku_grid row (col + 1)

(* Function to provide a hint to the user *)
let hint sudoku_grid =
  match find_valid_move sudoku_grid 0 0 with
  | None -> print_endline "Hint : No valid moves available.\n\n"
  | Some (row, col, number) ->
      Printf.printf "Hint : Try placing %d in row %d, column %d.\n\n" number
        (row + 1) (col + 1)

let rec run_game_d sudoku_grid immutable_cells grid_solved move_count start_time
    statistics =
  match grid_solved with
  | true ->
      let end_time = Unix.time () in
      let solve_time = end_time -. start_time in
      print_endline
        ("Congrats, you won!\nYou solved this sudoku puzzle in "
       ^ string_of_int move_count ^ " moves!\n" ^ "Total time taken: "
       ^ string_of_float solve_time ^ " seconds.\n"
       ^ "Great job! We encourage you to play another exciting game of \
          diagonal sudoku.\n");
      update_statistics statistics solve_time;
      display_statistics statistics
  | false -> (
      print_endline
        ("Sudoku Board | Move " ^ string_of_int (move_count - 1) ^ " :");
      let erroneous_rows, completed_rows = check_all_rows sudoku_grid in
      let erroneous_cols, completed_cols = check_all_cols sudoku_grid in
      let erroneous_boxes, completed_boxes = check_all_boxes sudoku_grid in
      let ld_erroneous, ld_complete = check_left_diagonal sudoku_grid in
      let rd_erroneous, rd_complete = check_right_diagonal sudoku_grid in
      print_sudoku_grid_d sudoku_grid erroneous_rows erroneous_cols
        erroneous_boxes ld_erroneous rd_erroneous completed_rows completed_cols
        completed_boxes ld_complete rd_complete immutable_cells;
      print_endline
        "Options: (1) Enter move, (2) Get hint, (3) Get help, (4) Quit.";
      let choice = get_play_option () in
      match choice with
      | 1 ->
          let row, col, number = get_input immutable_cells in
          sudoku_grid.(row - 1).(col - 1) <- number;
          let grid_solved' =
            cardinality_of_int_set completed_rows = 9
            && cardinality_of_int_set completed_cols = 9
            && cardinality_of_pair_set completed_boxes = 9
            && ld_complete && rd_complete
          in
          run_game_d sudoku_grid immutable_cells grid_solved' (move_count + 1)
            start_time statistics
      | 2 ->
          hint sudoku_grid;
          (* Provide a hint to the user *)
          run_game_d sudoku_grid immutable_cells grid_solved move_count
            start_time statistics
      | 3 ->
          let help_user_d_path = "data/private/help_user_d.txt" in
          let lines = read_lines help_user_d_path in
          let () = print_string_list lines in
          (* Provide help to the user *)
          run_game_d sudoku_grid immutable_cells grid_solved move_count
            start_time statistics
      | 4 -> ()
      | _ ->
          print_endline "Invalid choice. Please try again.";
          run_game_d sudoku_grid immutable_cells grid_solved move_count
            start_time statistics)

(** Open AI. "Create a sudoku game in ocaml - Chat Conversation". Chat GPT.
    April 2024 *)

(* Talk about parameters, update diagonal_sudoku.mli *)
