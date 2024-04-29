(* Connect Four game implementation in OCaml with array of arrays *)

(* Type definitions *)
type player =
  | Red
  | Yellow

let string_of_player = function
  | Some Red -> "Red"
  | Some Yellow -> "Yellow"
  | None -> "_"

(* Type definition for the board *)
type board = player option array array

(* Function to initialize an empty board *)
let init_board () : board = Array.make_matrix 7 6 None

let transpose (matrix : 'a array array) : 'a array array =
  if Array.length matrix = 0 then matrix
  else
    let num_rows = Array.length matrix in
    let num_cols = Array.length matrix.(0) in
    let transposed = Array.make_matrix num_cols num_rows matrix.(0).(0) in
    for i = 0 to num_rows - 1 do
      for j = 0 to num_cols - 1 do
        transposed.(j).(i) <- matrix.(i).(j)
      done
    done;
    transposed

(* Function to print the board *)
let print_board (board : board) : unit =
  let print_player_cell cell =
    match cell with
    | Some Red -> print_string "R "
    | Some Yellow -> print_string "Y "
    | None -> print_string "_ "
  in
  let print_row row =
    Array.iter
      (fun cell ->
        print_player_cell cell;
        print_string " ")
      row;
    print_endline ""
  in
  let transposed_board = transpose board in
  Array.iter print_row transposed_board

(* Function to check if the board is full *)
let is_board_full (board : board) : bool =
  Array.for_all (Array.for_all (fun cell -> cell <> None)) board

(* Function to check if a column is full *)
let is_column_full (col : player option array) : bool =
  Array.for_all (fun cell -> cell <> None) col

(* Function to drop a token into a column *)
let drop_token (player : player) (board : board) (column_number : int) : bool =
  let rec find_empty_index col idx =
    if idx < 0 then -1
    else if idx < Array.length col && col.(idx) = None then idx
    else find_empty_index col (idx - 1)
  in
  let empty_index =
    find_empty_index board.(column_number)
      (Array.length board.(column_number) - 1)
  in
  if empty_index <> -1 then begin
    board.(column_number).(empty_index) <- Some player;
    true
  end
  else false

(* Function to get the column number from user input *)
let rec get_column_number () : int =
  print_string "Enter column number (1-7): ";
  try
    let column = read_int () in
    if column >= 1 && column <= 7 then
      column - 1 (* Adjusting for zero-based indexing *)
    else begin
      print_endline
        "Invalid column number. Please enter a number between 1 and 7.";
      get_column_number ()
    end
  with Failure _ ->
    print_endline "Invalid input. Please enter a number.";
    get_column_number ()

(* Function to check if a player has won *)
let check_winner (board : board) (player : player) : bool =
  let width = Array.length board in
  let height = Array.length board.(0) in

  (* Helper function to check for four consecutive tokens in a list *)
  let rec check_consecutive count = function
    | [] -> count >= 4
    | h :: t ->
        if count >= 4 then true
        else if h = Some player then check_consecutive (count + 1) t
        else check_consecutive 0 t
  in

  (* Helper function to check for four consecutive tokens in a column *)
  let check_rows () =
    let transpose_board = transpose board in
    Array.exists
      (fun col ->
        let row = Array.to_list col in
        check_consecutive 0 row)
      transpose_board
  in

  (* Helper function to check for four consecutive tokens in a row *)
  let check_columns () =
    Array.exists
      (fun col ->
        let row = Array.to_list col in
        check_consecutive 1 row)
      board
  in

  (* Helper function to check for four consecutive tokens in diagonals *)
  let check_diagonals () =
    let rec up_diag i j acc =
      if i >= width || j >= height then acc
      else up_diag (i + 1) (j + 1) (board.(i).(j) :: acc)
    in
    let rec down_diag i j acc =
      if i >= width || j < 0 then acc
      else down_diag (i + 1) (j - 1) (board.(i).(j) :: acc)
    in
    let up_diagonals =
      let rec generate_ups i j acc =
        if j < height then begin
          if i < width then generate_ups (i + 1) j (up_diag i j [] :: acc)
          else generate_ups 0 (j + 1) acc
        end
        else acc
      in
      generate_ups 0 0 []
    in
    let down_diagonals =
      let rec generate_downs i j acc =
        if j < height then begin
          if i < width then generate_downs (i + 1) j (down_diag i j [] :: acc)
          else generate_downs 0 (j + 1) acc
        end
        else acc
      in
      generate_downs 0 0 []
    in
    List.exists (fun diagonal -> check_consecutive 0 diagonal) up_diagonals
    || List.exists (fun diagonal -> check_consecutive 0 diagonal) down_diagonals
  in

  (* Check rows, columns, and diagonals *)
  check_rows () || check_columns () || check_diagonals ()

(* Function to make a move for a player *)
let rec make_move (board : board) (player : player) : unit =
  print_string (if player = Red then "Red" else "Yellow");
  print_endline "'s turn";
  print_board board;
  let column_number = get_column_number () in
  let drop = drop_token player board column_number in
  if not drop then begin
    print_endline "Column is full. Please choose another column.";
    make_move board player
  end
  else ()

let rec play_game board player =
  if is_board_full board then (
    print_board board;
    print_endline "It's a draw!")
  else begin
    let () = make_move board player in
    if check_winner board player then
      print_endline
        (if player = Red then (
           print_board board;
           "Red wins!")
         else (
           print_board board;
           "Yellow wins!"))
    else play_game board (if player = Red then Yellow else Red)
  end

(* Main function to start the game *)
let connect_four () =
  let board = init_board () in
  play_game board Red

(* OpenAI. "Write OCaml logic for implementing Connect4 - ChatGPT." Chat
   conversation, April 28, 2024. *)
