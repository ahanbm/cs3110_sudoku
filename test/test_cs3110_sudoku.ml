(**@authors: Peter Favero pmf66, Ahan Mishra abm247, Peter Zheng pcz3 *)
(*To compile, run: dune test in the terminal *)

open OUnit2
open Cs3110_sudoku
include Diagonal_sudoku
include Statistics
include Timer

let grid_rows_then_cols_test sudoku_grid erroneous_rows_count
    completed_rows_count erroneous_cols_count completed_cols_count _ =
  let erroneous_rows, completed_rows = check_all_rows sudoku_grid in
  let erroneous_cols, completed_cols = check_all_cols sudoku_grid in
  assert (
    cardinality_of_int_set erroneous_rows = erroneous_rows_count
    && cardinality_of_int_set completed_rows = completed_rows_count
    && cardinality_of_int_set erroneous_cols = erroneous_cols_count
    && cardinality_of_int_set completed_cols = completed_cols_count)

let grid_boxes_test sudoku_grid erroneous_boxes_count completed_boxes_count _ =
  let erroneous_boxes, completed_boxes = check_all_boxes sudoku_grid in
  assert (
    cardinality_of_pair_set erroneous_boxes = erroneous_boxes_count
    && cardinality_of_pair_set completed_boxes = completed_boxes_count)

let grid_left_then_right_diagonals_test sudoku_grid ld_e_expected ld_c_expected
    rd_e_expected rd_c_expected _ =
  let ld_erroneous, ld_complete = check_left_diagonal sudoku_grid in
  let rd_erroneous, rd_complete = check_right_diagonal sudoku_grid in
  assert (
    ld_complete = ld_c_expected
    && ld_erroneous = ld_e_expected
    && rd_complete = rd_c_expected
    && rd_erroneous = rd_e_expected)

let test_check_erroneous_completed_rows _ =
  let filename = "test_data/2_erroneous_row.csv" in
  let sudoku_grid, _ = preset_of_csv filename in
  let erroneous_rows, completed_rows = check_all_rows sudoku_grid in
  (* Assert that the erroneous rows set contains the correct rows *)
  assert_bool "Row 0 should be erroneous" (IntegerSet.mem 0 erroneous_rows);
  assert_bool "Row 8 should be erroneous" (IntegerSet.mem 8 erroneous_rows);
  (* Assert that the completed rows set contains the correct rows *)
  assert_bool "Row 1 should be completed" (IntegerSet.mem 1 completed_rows);
  assert_bool "Row 2 should be completed" (IntegerSet.mem 2 completed_rows);
  assert_bool "Row 3 should be completed" (IntegerSet.mem 3 completed_rows);
  assert_bool "Row 4 should be completed" (IntegerSet.mem 4 completed_rows);
  assert_bool "Row 5 should be completed" (IntegerSet.mem 5 completed_rows);
  assert_bool "Row 6 should be completed" (IntegerSet.mem 6 completed_rows);
  assert_bool "Row 7 should be completed" (IntegerSet.mem 7 completed_rows)

let test_check_erroneous_completed_cols _ =
  let filename = "test_data/2_erroneous_row.csv" in
  let sudoku_grid, _ = preset_of_csv filename in
  let erroneous_cols, completed_cols = check_all_cols sudoku_grid in
  (* Assert that the erroneous cols set contains the correct cols *)
  assert_bool "Column 1 should be erroneous" (IntegerSet.mem 1 erroneous_cols);
  assert_bool "Column 8 should be erroneous" (IntegerSet.mem 8 erroneous_cols);
  (* Assert that the completed cols set contains the correct cols *)
  assert_bool "Column 0 should be completed" (IntegerSet.mem 0 completed_cols);
  assert_bool "Column 2 should be completed" (IntegerSet.mem 2 completed_cols);
  assert_bool "Column 3 should be completed" (IntegerSet.mem 3 completed_cols);
  assert_bool "Column 4 should be completed" (IntegerSet.mem 4 completed_cols);
  assert_bool "Column 5 should be completed" (IntegerSet.mem 5 completed_cols);
  assert_bool "Column 6 should be completed" (IntegerSet.mem 6 completed_cols);
  assert_bool "Column 7 should be completed" (IntegerSet.mem 7 completed_cols)

let test_check_erroneous_completed_boxes _ =
  let filename = "test_data/2_erroneous_row.csv" in
  let sudoku_grid, _ = preset_of_csv filename in
  let erroneous_boxes, completed_boxes = check_all_boxes sudoku_grid in
  (* Assert that the erroneous boxes set contains the correct boxes *)
  assert_bool "Box 0 should be erroneous" (PairSet.mem (0, 0) erroneous_boxes);
  assert_bool "Box 8 should be erroneous" (PairSet.mem (2, 2) erroneous_boxes);
  (* Assert that the completed boxes set contains the correct boxes *)
  assert_bool "Box 1 should be completed" (PairSet.mem (0, 1) completed_boxes);
  assert_bool "Box 2 should be completed" (PairSet.mem (0, 2) completed_boxes);
  assert_bool "Box 3 should be completed" (PairSet.mem (1, 0) completed_boxes);
  assert_bool "Box 4 should be completed" (PairSet.mem (1, 1) completed_boxes);
  assert_bool "Box 5 should be completed" (PairSet.mem (1, 2) completed_boxes);
  assert_bool "Box 6 should be completed" (PairSet.mem (2, 0) completed_boxes);
  assert_bool "Box 7 should be completed" (PairSet.mem (2, 1) completed_boxes)

let test_check_erroneous_completed_left_diag _ =
  let filename = "test_data/2_erroneous_row.csv" in
  let sudoku_grid, _ = preset_of_csv filename in
  let erroneous, correct_length = check_left_diagonal sudoku_grid in
  (* Assert that the erroneous rows set contains the correct rows *)
  assert_bool "Left diagonal should be erroneous" erroneous;
  assert_bool "Left diagonal should not be the correct length"
    (not correct_length)

let test_check_erroneous_completed_right_diag _ =
  let filename = "test_data/2_erroneous_row.csv" in
  let sudoku_grid, _ = preset_of_csv filename in
  let erroneous, correct_length = check_right_diagonal sudoku_grid in
  (* Assert that the erroneous rows set contains the correct rows *)
  assert_bool "Right diagonal should not be erroneous" (not erroneous);
  assert_bool "Right diagonal should be the correct length" correct_length

(* Note: this function is implemented in here mirroring the logic in run_game
   because using a function like this in run_game's implementation, each of
   check_all_rows, check_all_cols, check_all_boxes, etc. would be computed
   twice, reducing efficiency. *)
let is_solved sudoku_grid input_bool _ =
  let _, completed_rows = check_all_rows sudoku_grid in
  let _, completed_cols = check_all_cols sudoku_grid in
  let _, completed_boxes = check_all_boxes sudoku_grid in
  let _, ld_complete = check_left_diagonal sudoku_grid in
  let _, rd_complete = check_right_diagonal sudoku_grid in
  assert (
    (cardinality_of_int_set completed_rows = 9
    && cardinality_of_int_set completed_cols = 9
    && cardinality_of_pair_set completed_boxes = 9
    && ld_complete && rd_complete)
    = input_bool)

let test_is_valid_move _ =
  let sudoku_grid = Array.make_matrix 9 9 0 in
  assert_equal true (is_valid_move sudoku_grid 0 0 1);
  (* Empty grid, any move is valid *)
  sudoku_grid.(0).(0) <- 1;
  assert_equal false (is_valid_move sudoku_grid 0 0 1);
  (* Same cell, invalid move *)
  assert_equal false (is_valid_move sudoku_grid 0 1 1);
  (* Same row, invalid move *)
  assert_equal false (is_valid_move sudoku_grid 1 0 1);
  (* Same column, invalid move *)
  sudoku_grid.(0).(0) <- 0;
  sudoku_grid.(1).(1) <- 1;
  assert_equal false (is_valid_move sudoku_grid 0 0 1);
  (* Same box, invalid move *)
  assert_equal true (is_valid_move sudoku_grid 0 0 2)
(* Different number, valid move *)

let test_find_valid_move _ =
  let empty_grid = Array.make_matrix 9 9 0 in
  let result = find_valid_move empty_grid 0 0 in
  print_endline
    ("Result: "
    ^
    match result with
    | Some (x, y, z) ->
        "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", " ^ string_of_int z
        ^ ")"
    | None -> "None");
  assert_equal (Some (0, 0, 1)) result;

  let grid_with_first_row_filled = Array.make_matrix 9 9 0 in
  Array.iteri
    (fun i _ -> grid_with_first_row_filled.(0).(i) <- i + 1)
    grid_with_first_row_filled.(0);
  let result = find_valid_move grid_with_first_row_filled 0 0 in
  print_endline
    ("Result: "
    ^
    match result with
    | Some (x, y, z) ->
        "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", " ^ string_of_int z
        ^ ")"
    | None -> "None");
  assert_equal (Some (1, 0, 4)) result;

  let grid_with_first_cell_filled = Array.make_matrix 9 9 0 in
  grid_with_first_cell_filled.(0).(0) <- 1;
  let result = find_valid_move grid_with_first_cell_filled 0 0 in
  print_endline
    ("Result: "
    ^
    match result with
    | Some (x, y, z) ->
        "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", " ^ string_of_int z
        ^ ")"
    | None -> "None");
  assert_equal (Some (0, 1, 2)) result;

  let full_grid = Array.make_matrix 9 9 1 in
  let result = find_valid_move full_grid 0 0 in
  print_endline
    ("Result: "
    ^
    match result with
    | Some (x, y, z) ->
        "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", " ^ string_of_int z
        ^ ")"
    | None -> "None");
  assert_equal None result

let empty_grid = Array.make_matrix 9 9 0
let homogenous_grid = Array.make_matrix 9 9 1
let solved_grid_path = "test_data/solved.csv"
let solved_grid = fst (preset_of_csv solved_grid_path)

let solved_excpet_for_last_row_grid_path =
  "test_data/almost_solved_missing_row.csv"

let solved_excpet_for_last_row_grid =
  fst (preset_of_csv solved_excpet_for_last_row_grid_path)

let solved_except_for_topleft_cell_path =
  "test_data/almost_solved_missing_topleft_cell.csv"

let solved_except_for_topleft_cell =
  fst (preset_of_csv solved_except_for_topleft_cell_path)

let test_cardinality_of_pair_set_empty _ =
  let empty_set = PairSet.empty in
  assert_equal 0
    (cardinality_of_pair_set empty_set)
    ~msg:"Empty set should have cardinality 0"

let test_cardinality_of_pair_set_singleton _ =
  let singleton_set = PairSet.singleton (1, 1) in
  assert_equal 1
    (cardinality_of_pair_set singleton_set)
    ~msg:"Singleton set should have cardinality 1"

let test_cardinality_of_pair_set_large _ =
  let set = PairSet.of_list [ (1, 1); (2, 2); (3, 3); (4, 4); (5, 5) ] in
  assert_equal 5
    (cardinality_of_pair_set set)
    ~msg:"Set with multiple elements should have correct cardinality"

let test_cardinality_of_pair_set_duplicate_elements _ =
  let set = PairSet.of_list [ (1, 1); (1, 1); (2, 2); (3, 3); (3, 3) ] in
  assert_equal 3
    (cardinality_of_pair_set set)
    ~msg:"Set with duplicate elements should have correct cardinality"

let test_cardinality_of_pair_set_empty_set _ =
  let set = PairSet.of_list [] in
  assert_equal 0
    (cardinality_of_pair_set set)
    ~msg:"Set with no elements should have cardinality 0"

let test_cardinality_of_pair_set_mixed_elements _ =
  let set = PairSet.of_list [ (1, 1); (2, 3); (4, 4); (5, 5) ] in
  assert_equal 4
    (cardinality_of_pair_set set)
    ~msg:"Set with mixed elements should have correct cardinality"

let test_cardinality_of_pair_set_negative_elements _ =
  let set = PairSet.of_list [ (1, -1); (-2, 3); (-4, -4); (-5, 5) ] in
  assert_equal 4
    (cardinality_of_pair_set set)
    ~msg:"Set with negative elements should have correct cardinality"

let test_cardinality_of_pair_set_repeated_elements _ =
  let set = PairSet.of_list [ (1, 1); (1, 1); (1, 1); (1, 1) ] in
  assert_equal 1
    (cardinality_of_pair_set set)
    ~msg:"Set with repeated elements should have cardinality 1"

let test_cardinality_of_pair_set_random_elements _ =
  let set = PairSet.of_list [ (2, 3); (5, 7); (3, 1); (2, 3); (5, 7) ] in
  assert_equal 3
    (cardinality_of_pair_set set)
    ~msg:"Set with random elements should have correct cardinality"

let test_cardinality_of_int_set_empty _ =
  let set = IntegerSet.empty in
  let result = cardinality_of_int_set set in
  assert_equal 0 result ~msg:"Empty set should have cardinality 0."

let test_cardinality_of_int_set_singleton _ =
  let set = IntegerSet.singleton 42 in
  let result = cardinality_of_int_set set in
  assert_equal 1 result ~msg:"Singleton set should have cardinality 1."

let test_cardinality_of_int_set_middle _ =
  let set =
    List.fold_left
      (fun acc x -> IntegerSet.add x acc)
      IntegerSet.empty
      (List.init 300 (fun x -> x))
  in
  let result = cardinality_of_int_set set in
  assert_equal 300 result ~msg:"Middling set should have cardinality 300."

let test_cardinality_of_int_set_large _ =
  let set =
    List.fold_left
      (fun acc x -> IntegerSet.add x acc)
      IntegerSet.empty
      (List.init 1000 (fun x -> x))
  in
  let result = cardinality_of_int_set set in
  assert_equal 1000 result ~msg:"Large set should have cardinality 1000."

(* Additional tests *)
let test_cardinality_of_int_set_duplicate _ =
  let set = IntegerSet.of_list [ 1; 2; 3; 1; 2; 3 ] in
  let result = cardinality_of_int_set set in
  assert_equal 3 result ~msg:"Set with duplicates should have unique elements."

let test_cardinality_of_int_set_zero _ =
  let set = IntegerSet.of_list [ 0 ] in
  let result = cardinality_of_int_set set in
  assert_equal 1 result ~msg:"Set with zero should have cardinality 1."

let test_cardinality_of_int_set_negative _ =
  let set = IntegerSet.of_list [ -1; -2; -3 ] in
  let result = cardinality_of_int_set set in
  assert_equal 3 result
    ~msg:"Set with negative numbers should have cardinality 3."

let test_cardinality_of_int_set_mixed _ =
  let set = IntegerSet.of_list [ -3; 1; 5; -3; 0; 5; 1 ] in
  let result = cardinality_of_int_set set in
  assert_equal 4 result ~msg:"Set with mixed numbers should have cardinality 4."

let test_cardinality_of_int_set_repeated _ =
  let set = IntegerSet.of_list [ 1; 1; 1; 1; 1 ] in
  let result = cardinality_of_int_set set in
  assert_equal 1 result
    ~msg:"Set with repeated elements should have cardinality 1."

let test_preset_of_csv _ =
  let filename = "test_data/almost_solved_missing_topleft_cell.csv" in
  let sudoku_grid, immutable_cells = preset_of_csv filename in
  (* Assert dimensions of the sudoku grid *)
  assert_equal 9 (Array.length sudoku_grid)
    ~msg:"Sudoku grid should have 9 rows";
  assert_equal 9
    (Array.length sudoku_grid.(0))
    ~msg:"Sudoku grid should have 9 columns";
  (* Assert specific values in the grid *)
  assert_equal 0 sudoku_grid.(0).(0) ~msg:"First cell should have value 0";
  assert_equal 2 sudoku_grid.(0).(1) ~msg:"Second cell should have value 2";
  assert_equal 5 sudoku_grid.(0).(2) ~msg:"Third cell should have value 5";
  assert_equal 5
    sudoku_grid.(8).(7)
    ~msg:"Second to last cell in the last row should have value 5";
  assert_equal 3 sudoku_grid.(8).(8) ~msg:"Last cell should have value 3";
  (* Assert immutable cells *)
  assert_bool "(0, 1) should be an immutable cell"
    (PairSet.mem (0, 1) immutable_cells);
  assert_bool "(4, 6) should be an immutable cell"
    (PairSet.mem (4, 6) immutable_cells);
  (* Assert cells that should not be immutable *)
  assert_bool "(0, 0) should not be an immutable cell"
    (not (PairSet.mem (0, 0) immutable_cells));
  (* Assert specific cells that should be immutable *)
  assert_bool "(1, 3) should be an immutable cell"
    (PairSet.mem (1, 3) immutable_cells);
  assert_bool "(4, 1) should be an immutable cell"
    (PairSet.mem (4, 1) immutable_cells);
  assert_bool "(3, 5) should be an immutable cell"
    (PairSet.mem (3, 5) immutable_cells);
  assert_bool "(5, 7) should be an immutable cell"
    (PairSet.mem (5, 7) immutable_cells);
  assert_bool "(6, 6) should be an immutable cell"
    (PairSet.mem (6, 6) immutable_cells);
  assert_bool "(7, 8) should be an immutable cell"
    (PairSet.mem (7, 8) immutable_cells);
  assert_bool "(8, 8) should be an immutable cell"
    (PairSet.mem (8, 8) immutable_cells);
  (* Continue assertions for immutable cells *)
  ()

let test_preset_of_csv_2 _ =
  let filename = "test_data/almost_solved_missing_row.csv" in
  let sudoku_grid, immutable_cells = preset_of_csv filename in
  assert_equal 9 (Array.length sudoku_grid)
    ~msg:"Sudoku grid should have 9\n\n   rows";
  assert_equal 9
    (Array.length sudoku_grid.(0))
    ~msg:"Sudoku grid\n   should\n have 9 columns";
  (* Check specific cells *)
  assert_equal 9 sudoku_grid.(0).(0) ~msg:"Value at row 1, column 1 should be 9";
  assert_equal 2 sudoku_grid.(0).(1) ~msg:"Value at row 1, column 2 should be 2";
  assert_equal 5 sudoku_grid.(0).(2) ~msg:"Value at row 1, column 3 should be 5";
  assert_equal 3 sudoku_grid.(1).(0) ~msg:"Value at row 2, column 1 should be 3";
  assert_equal 7 sudoku_grid.(2).(0) ~msg:"Value at row 3, column 1 should be 7";
  assert_equal 1 sudoku_grid.(0).(8) ~msg:"Value at row 1, column 9 should be 1";
  assert_equal 0 sudoku_grid.(8).(0) ~msg:"Value at row 9, column 1 should be 0";
  assert_equal 0 sudoku_grid.(8).(8) ~msg:"Value at row 9, column 9 should be 0";
  (* Check immutable cells *)
  assert_bool "Top left cell should be immutable"
    (PairSet.mem (0, 0) immutable_cells);
  assert_bool "Top right cell should be immutable"
    (PairSet.mem (0, 8) immutable_cells);
  assert_bool "Bottom right cell should not be immutable"
    (not (PairSet.mem (8, 8) immutable_cells));
  assert_bool "Cell at row 2, column 3 should be immutable"
    (PairSet.mem (1, 2) immutable_cells);
  assert_bool "Cell at row 4, column 5 should be immutable"
    (PairSet.mem (3, 4) immutable_cells);
  assert_bool "Cell at row 8, column 7 should be immutable"
    (PairSet.mem (7, 6) immutable_cells);
  (* Check some other cells *)
  assert_equal 8 sudoku_grid.(1).(1) ~msg:"Value at row 2, column 2 should be 8";
  assert_equal 1 sudoku_grid.(3).(4) ~msg:"Value at row 4, column 5 should be 1";
  assert_equal 4 sudoku_grid.(7).(6) ~msg:"Value at row 8, column 7 should be 4";
  assert_equal 8 sudoku_grid.(5).(0) ~msg:"Value at row 6, column 1 should be 8";
  assert_equal 3 sudoku_grid.(4).(7) ~msg:"Value at row 5, column 8 should be 0";
  (* Check more immutable and non-immutable cells *)
  assert_bool "Cell at row 3, column 2 should be immutable"
    (PairSet.mem (2, 1) immutable_cells);
  assert_bool "Cell at row 6, column 5 should be immutable"
    (PairSet.mem (5, 4) immutable_cells);
  assert_bool "Cell at row 9, column 3 should not be immutable"
    (not (PairSet.mem (8, 2) immutable_cells))

let test_preset_of_csv_3 _ =
  let filename = "test_data/solved.csv" in
  let sudoku_grid, immutable_cells = preset_of_csv filename in
  (* Check specific cells *)
  assert_equal 9 sudoku_grid.(0).(0) ~msg:"Value at row 1, column 1 should be 9";
  assert_equal 1 sudoku_grid.(0).(8) ~msg:"Value at row 1, column 9 should be 1";
  assert_equal 3 sudoku_grid.(8).(8) ~msg:"Value at row 9, column 9 should be 3";
  (* Check immutable cells *)
  assert_bool "Top left cell should be immutable"
    (PairSet.mem (0, 0) immutable_cells);
  assert_bool "Top right cell should be immutable"
    (PairSet.mem (0, 8) immutable_cells);
  assert_bool "Bottom right cell should be immutable"
    (PairSet.mem (8, 8) immutable_cells);
  assert_bool "Cell at row 3, column 2 should be immutable"
    (PairSet.mem (2, 1) immutable_cells);
  assert_bool "Cell at row 4, column 5 should be immutable"
    (PairSet.mem (3, 4) immutable_cells);
  assert_bool "Cell at row 7, column 7 should be immutable"
    (PairSet.mem (6, 6) immutable_cells);
  (* Check some other cells *)
  assert_equal 4 sudoku_grid.(2).(2) ~msg:"Value at row 3, column 3 should be 4";
  assert_equal 1 sudoku_grid.(3).(4) ~msg:"Value at row 4, column 5 should be 1";
  assert_equal 1 sudoku_grid.(6).(6) ~msg:"Value at row 7, column 7 should be 1";
  assert_equal 8 sudoku_grid.(5).(0) ~msg:"Value at row 6, column 1 should be 8";
  assert_equal 3 sudoku_grid.(4).(7) ~msg:"Value at row 5, column 8 should be 3";
  assert_equal 9 sudoku_grid.(7).(2) ~msg:"Value at row 8, column 3 should be 9";
  (* Check non-immutable cells *)
  assert_bool "Cell at row 2, column 3 should be immutable"
    (PairSet.mem (1, 2) immutable_cells);
  assert_bool "Cell at row 5, column 4 should be immutable"
    (PairSet.mem (4, 3) immutable_cells);
  assert_bool "Cell at row 8, column 2 should be immutable"
    (PairSet.mem (7, 1) immutable_cells)

let test_preset_of_csv_erroneous_row _ =
  let filename = "test_data/2_erroneous_row.csv" in
  let sudoku_grid, immutable_cells = preset_of_csv filename in
  (* Check specific cells *)
  assert_equal 2 sudoku_grid.(0).(0) ~msg:"Value at row 1, column 1 should be 2";
  assert_equal 2 sudoku_grid.(0).(1) ~msg:"Value at row 1, column 2 should be 2";
  (* Check immutable cells *)
  assert_bool "Top left cell should be immutable"
    (PairSet.mem (0, 0) immutable_cells);
  assert_bool "Top right cell should be immutable"
    (PairSet.mem (0, 8) immutable_cells);
  assert_bool "Bottom right cell should be immutable"
    (PairSet.mem (8, 8) immutable_cells);
  (* Check some other cells *)
  assert_equal 4 sudoku_grid.(0).(2) ~msg:"Value at row 1, column 3 should be 4";
  assert_equal 3 sudoku_grid.(0).(3) ~msg:"Value at row 1, column 4 should be 3";
  assert_equal 5 sudoku_grid.(0).(4) ~msg:"Value at row 1, column 5 should be 5"

let test_preset_of_csv_empty_cells _ =
  let filename = "test_data/almost_solved_missing_topleft_cell.csv" in
  let _, immutable_cells = preset_of_csv filename in
  (* Assert that a cell with 0 value is not in the immutable cells set *)
  assert_bool "(0, 0) should not be an immutable cell"
    (not (PairSet.mem (0, 0) immutable_cells));
  assert_bool "(1, 1) should be an immutable cell"
    (PairSet.mem (1, 1) immutable_cells);
  assert_bool "(2, 2) should be an immutable cell"
    (PairSet.mem (2, 2) immutable_cells);
  assert_bool "(3, 3) should be an immutable cell"
    (PairSet.mem (3, 3) immutable_cells);
  assert_bool "(4, 4) should be an immutable cell"
    (PairSet.mem (4, 4) immutable_cells);
  assert_bool "(5, 5) should be an immutable cell"
    (PairSet.mem (5, 5) immutable_cells);
  assert_bool "(6, 6) should be an immutable cell"
    (PairSet.mem (6, 7) immutable_cells);
  assert_bool "(7, 7) should be an immutable cell"
    (PairSet.mem (7, 7) immutable_cells);
  assert_bool "(8, 8) should be an immutable cell"
    (PairSet.mem (8, 8) immutable_cells)

let test_preset_of_csv_empty_cells2 _ =
  let filename = "test_data/almost_solved_missing_row.csv" in
  let _, immutable_cells = preset_of_csv filename in
  (* Assert that a cell with 0 value is not in the immutable cells set *)
  assert_bool "(8, 0) should not be an immutable cell"
    (not (PairSet.mem (8, 0) immutable_cells));
  assert_bool "(8, 1) should not be an immutable cell"
    (not (PairSet.mem (8, 1) immutable_cells));
  assert_bool "(8, 2) should not be an immutable cell"
    (not (PairSet.mem (8, 2) immutable_cells));
  assert_bool "(8, 3) should not be an immutable cell"
    (not (PairSet.mem (8, 3) immutable_cells));
  assert_bool "(8, 4) should not be an immutable cell"
    (not (PairSet.mem (8, 4) immutable_cells));
  assert_bool "(8, 5) should not be an immutable cell"
    (not (PairSet.mem (8, 5) immutable_cells));
  assert_bool "(8, 6) should not be an immutable cell"
    (not (PairSet.mem (8, 6) immutable_cells));
  assert_bool "(8, 7) should not be an immutable cell"
    (not (PairSet.mem (8, 7) immutable_cells));
  assert_bool "(8, 8) should not be an immutable cell"
    (not (PairSet.mem (8, 8) immutable_cells));
  assert_bool "(0, 0) should be an immutable cell"
    (PairSet.mem (0, 0) immutable_cells);
  assert_bool "(1, 1) should be an immutable cell"
    (PairSet.mem (1, 1) immutable_cells);
  assert_bool "(2, 2) should be an immutable cell"
    (PairSet.mem (2, 2) immutable_cells);
  assert_bool "(3, 3) should be an immutable cell"
    (PairSet.mem (3, 3) immutable_cells);
  assert_bool "(4, 4) should be an immutable cell"
    (PairSet.mem (4, 4) immutable_cells);
  assert_bool "(5, 5) should be an immutable cell"
    (PairSet.mem (5, 5) immutable_cells);
  assert_bool "(6, 6) should be an immutable cell"
    (PairSet.mem (6, 6) immutable_cells);
  assert_bool "(7, 7) should be an immutable cell"
    (PairSet.mem (7, 7) immutable_cells)

let test_preset_of_csv_all_filled _ =
  let filename = "test_data/solved.csv" in
  let _, immutable_cells = preset_of_csv filename in
  (* Assert that all cells are in the immutable cells set *)
  for i = 0 to 8 do
    for j = 0 to 8 do
      assert_bool
        (Printf.sprintf "(%d, %d) should be an immutable cell" i j)
        (PairSet.mem (i, j) immutable_cells)
    done
  done

let test_preset_of_csv_immutable_cells _ =
  let filename = "test_data/almost_solved_missing_topleft_cell.csv" in
  let _, immutable_cells = preset_of_csv filename in
  (* Assert the number of immutable cells *)
  assert_equal 80
    (PairSet.cardinal immutable_cells)
    ~msg:"Immutable cells set\n   should have 80 elements."

let test_preset_of_csv_immutable_cells2 _ =
  let filename = "test_data/almost_solved_missing_row.csv" in
  let _, immutable_cells = preset_of_csv filename in
  (* Assert the number of immutable cells *)
  assert_equal 72
    (PairSet.cardinal immutable_cells)
    ~msg:"Immutable cells set\n   should have 72 elements."

let test_preset_of_csv_immutable_cells3 _ =
  let filename = "test_data/initial.csv" in
  let _, immutable_cells = preset_of_csv filename in
  (* Assert the number of immutable cells *)
  assert_equal 1
    (PairSet.cardinal immutable_cells)
    ~msg:"Immutable cells set\n   should have 1 element."

let test_preset_of_csv_immutable_cells4 _ =
  let filename = "test_data/solved.csv" in
  let _, immutable_cells = preset_of_csv filename in
  (* Assert the number of immutable cells *)
  assert_equal 81
    (PairSet.cardinal immutable_cells)
    ~msg:"Immutable cells set\n   should have 81 elements."

let load_sudoku_grid_from_csv filename =
  let ic = open_in filename in
  let sudoku_grid = Array.make_matrix 9 9 0 in
  try
    for row = 0 to 8 do
      let line = input_line ic in
      let values = String.split_on_char ',' line in
      List.iteri
        (fun col_index cell ->
          let value = int_of_string cell in
          sudoku_grid.(row).(col_index) <- value)
        values
    done;
    close_in ic;
    sudoku_grid
  with e ->
    close_in ic;
    raise e

let test_check_all_cols _ =
  (* Load the sudoku grid from the CSV file *)
  let sudoku_grid = load_sudoku_grid_from_csv "test_data/solved.csv" in

  (* Test the check_all_cols function *)
  let erroneous_cols, _ = check_all_cols sudoku_grid in

  (* Add assertions to check the results *)
  assert_equal IntegerSet.empty erroneous_cols
    ~msg:"No erroneous columns should be found"

let test_check_all_rows _ =
  let sudoku_grid = load_sudoku_grid_from_csv "test_data/solved.csv" in
  let erroneous_rows, completed_rows = check_all_rows sudoku_grid in
  (* Check erroneous rows *)
  assert_bool "No rows should be erroneous" (IntegerSet.is_empty erroneous_rows);
  (* Check completed rows *)
  assert_equal 9
    (IntegerSet.cardinal completed_rows)
    ~msg:"All rows should be completed"

let test_check_all_boxes _ =
  (* Load the sudoku grid from the CSV file *)
  let sudoku_grid = load_sudoku_grid_from_csv "test_data/solved.csv" in

  (* Test the check_all_boxes function *)
  let erroneous_boxes, _ = check_all_boxes sudoku_grid in

  (* Add assertions to check the results *)
  assert_equal PairSet.empty erroneous_boxes
    ~msg:"No erroneous boxes should be found"

let test_preset_of_csv_initial _ =
  (* Load the sudoku grid from the CSV file *)
  let sudoku_grid = load_sudoku_grid_from_csv "test_data/initial.csv" in

  (* Call the preset_of_csv function *)
  let preset_result = preset_of_csv "test_data/initial.csv" in

  (* Check if the result is of the correct type *)
  assert_equal (Array.length sudoku_grid)
    (Array.length (fst preset_result))
    ~msg:"The number of rows in the loaded grid should match the result";

  assert_equal
    (Array.length sudoku_grid.(0))
    (Array.length (fst preset_result).(0))
    ~msg:"The number of columns in the loaded grid should match the result";

  (* Check if the values are correctly loaded *)
  for i = 0 to 8 do
    for j = 0 to 8 do
      assert_equal
        sudoku_grid.(i).(j)
        (fst preset_result).(i).(j)
        ~msg:"The values in the loaded grid should match the result"
    done
  done

let test_reset_statistics _ =
  let stats = reset_statistics () in
  assert_equal 0 stats.games_played;
  assert_equal 0. stats.total_time;
  (* Check that resetting the statistics again still results in zero values *)
  let stats = reset_statistics () in
  assert_equal 0 stats.games_played;
  assert_equal 0. stats.total_time;
  (* Check that resetting the statistics after updating them results in zero
     values *)
  update_statistics stats 10.;
  let stats = reset_statistics () in
  assert_equal 0 stats.games_played;
  assert_equal 0. stats.total_time

let test_update_statistics _ =
  let stats = reset_statistics () in
  update_statistics stats 10.;
  assert_equal 1 stats.games_played;
  assert_equal 10. stats.total_time;
  update_statistics stats 20.;
  assert_equal 2 stats.games_played;
  assert_equal 30. stats.total_time;
  (* Check that updating the statistics with zero duration doesn't change the
     total time *)
  update_statistics stats 0.;
  assert_equal 3 stats.games_played;
  assert_equal 30. stats.total_time;
  (* Check that updating the statistics after resetting them works correctly *)
  let stats = reset_statistics () in
  update_statistics stats 15.;
  assert_equal 1 stats.games_played;
  assert_equal 15. stats.total_time

let test_display_statistics _ =
  let stats = reset_statistics () in
  update_statistics stats 10.;
  update_statistics stats 20.;
  (* Note: This test assumes that the display_statistics function prints the
     correct output, but it does not check the actual output. *)
  display_statistics stats;
  (* Check that displaying the statistics doesn't change the statistics *)
  assert_equal 2 stats.games_played;
  assert_equal 30. stats.total_time;
  (* Check that displaying the statistics after resetting them shows zero
     values *)
  let stats = reset_statistics () in
  display_statistics stats;
  assert_equal 0 stats.games_played;
  assert_equal 0. stats.total_time

let ounit2_tests =
  "ounit2 test suite"
  >::: [
         "Test check_all_boxes" >:: test_check_all_boxes;
         "Test check_all_rows" >:: test_check_all_rows;
         "Test check_all_cols" >:: test_check_all_cols;
         "Check erroneous and completed rows"
         >:: test_check_erroneous_completed_rows;
         "Check erroneous and completed columns"
         >:: test_check_erroneous_completed_cols;
         "Check erroneous and completed boxes"
         >:: test_check_erroneous_completed_boxes;
         "Check erroneous left diagonal"
         >:: test_check_erroneous_completed_left_diag;
         "Check erroneous right diagonal"
         >:: test_check_erroneous_completed_right_diag;
         "Preset of CSV" >:: test_preset_of_csv;
         "Preset of CSV 2" >:: test_preset_of_csv_2;
         "Preset of CSV 3" >:: test_preset_of_csv_3;
         "Preset of erroneous row CSV" >:: test_preset_of_csv_erroneous_row;
         "Preset of CSV Initial" >:: test_preset_of_csv_initial;
         "Immutable cells with missing cell" >:: test_preset_of_csv_empty_cells;
         "Immutable cells with missing row" >:: test_preset_of_csv_empty_cells2;
         "Immutable cells in completed game" >:: test_preset_of_csv_all_filled;
         "Number of immutable cells" >:: test_preset_of_csv_immutable_cells;
         "Number of immutable cells for missing row"
         >:: test_preset_of_csv_immutable_cells2;
         "Number of immutable cells for initial"
         >:: test_preset_of_csv_immutable_cells3;
         "Number of immutable cells for solved game"
         >:: test_preset_of_csv_immutable_cells4;
         "Valid moves" >:: test_is_valid_move;
         "Test find valid moves" >:: test_find_valid_move;
         "Empty Set" >:: test_cardinality_of_pair_set_empty;
         "Singleton Set" >:: test_cardinality_of_pair_set_singleton;
         "Large Set" >:: test_cardinality_of_pair_set_large;
         "Set with Duplicate Elements"
         >:: test_cardinality_of_pair_set_duplicate_elements;
         "Empty Set" >:: test_cardinality_of_pair_set_empty_set;
         "Set with Mixed Elements"
         >:: test_cardinality_of_pair_set_mixed_elements;
         "Set with Negative Elements"
         >:: test_cardinality_of_pair_set_negative_elements;
         "Set with Repeated Elements"
         >:: test_cardinality_of_pair_set_repeated_elements;
         "Set with Random Elements"
         >:: test_cardinality_of_pair_set_random_elements;
         "Empty Set" >:: test_cardinality_of_int_set_empty;
         "Singleton Set" >:: test_cardinality_of_int_set_singleton;
         "Large Set" >:: test_cardinality_of_int_set_large;
         "Middling Set" >:: test_cardinality_of_int_set_middle;
         "Set with Duplicates" >:: test_cardinality_of_int_set_duplicate;
         "Set with Zero" >:: test_cardinality_of_int_set_zero;
         "Set with Negative Numbers" >:: test_cardinality_of_int_set_negative;
         "Set with Mixed Numbers" >:: test_cardinality_of_int_set_mixed;
         "Set with Repeated Elements" >:: test_cardinality_of_int_set_repeated;
         "An empty grid should have no erroneous rows, no completed rows, no \
          erroneous cols, and no completed cols"
         >:: grid_rows_then_cols_test empty_grid 0 0 0 0;
         "An empty grid should have no erroneous boxes and no completed boxes"
         >:: grid_boxes_test empty_grid 0 0;
         "An empty grid should have no erroneous diagonals and no completed \
          diagonals"
         >:: grid_left_then_right_diagonals_test empty_grid false false false
               false;
         "An empty grid should not be flagged as solved by the logic in \
          run_game" >:: is_solved empty_grid false;
         "A homogenous grid should have 9 erroneous rows, no completed rows, 9 \
          erroneous cols, and no completed cols"
         >:: grid_rows_then_cols_test homogenous_grid 9 0 9 0;
         "A homogenous grid should have 9 erroneous boxes and no completed \
          boxes"
         >:: grid_boxes_test homogenous_grid 9 0;
         "A homogenous grid should have both diagonals erroneous and no \
          completed diagonals"
         >:: grid_left_then_right_diagonals_test homogenous_grid true false true
               false;
         "A homogenous grid should not be flagged as solved by the logic in \
          run_game"
         >:: is_solved homogenous_grid false;
         "A solved grid should have no erroneous rows, 9 completed rows, no \
          erroneous cols, and 9 completed cols"
         >:: grid_rows_then_cols_test solved_grid 0 9 0 9;
         "A solved grid should have 0 erroneous boxes and 9 completed boxes"
         >:: grid_boxes_test solved_grid 0 9;
         "A solved grid should have no erroneous diagonals and both diagonals \
          completed"
         >:: grid_left_then_right_diagonals_test solved_grid false true false
               true;
         "A homogenous grid should  be flagged as solved by the logic in \
          run_game" >:: is_solved solved_grid true;
         "An almost-solved-with-empty-last-row-grid should have no erroneous \
          rows, 8 completed rows, no erroneous cols, and no completed cols"
         >:: grid_rows_then_cols_test solved_excpet_for_last_row_grid 0 8 0 0;
         "An almost-solved-with-empty-last-row-grid should have 0 erroneous \
          boxes and 6 completed boxes"
         >:: grid_boxes_test solved_excpet_for_last_row_grid 0 6;
         "An almost-solved-with-empty-last-row-grid grid should have no \
          erroneous diagonals and both diagonals completed"
         >:: grid_left_then_right_diagonals_test solved_excpet_for_last_row_grid
               false false false false;
         "An almost-solved-with-empty-last-row-grid should not be flagged as \
          solved by the logic in run_game"
         >:: is_solved solved_excpet_for_last_row_grid false;
         "An almost-solved-with-empty-topleft-cell-grid should have 0 \
          erroneous rows, 8 completed rows, 0 erroneous cols, and 8 completed \
          cols"
         >:: grid_rows_then_cols_test solved_except_for_topleft_cell 0 8 0 8;
         "An almost-solved-with-empty-topleft-cell-grid should have 0 \
          erroneous boxes and 8 completed boxes"
         >:: grid_boxes_test solved_except_for_topleft_cell 0 8;
         "An almost-solved-with-empty-topleft-cell-grid grid should have no \
          erroneous diagonals and only the right diagonal completed"
         >:: grid_left_then_right_diagonals_test solved_except_for_topleft_cell
               false false false true;
         "An almost-solved-with-empty-topleft-cell-grid should not be flagged \
          as solved by the logic in run_game"
         >:: is_solved solved_except_for_topleft_cell false;
         "Reset statistics" >:: test_reset_statistics;
         "Update statistics" >:: test_update_statistics;
         "Display statistics" >:: test_display_statistics;
       ]

(* RUN TESTS *)

let () =
  print_endline "\nOUnit2 tests:";
  run_test_tt_main ounit2_tests

(** Open AI. "Write a test suite for this code - Chat Conversation". Chat GPT.
    May 2024 *)
