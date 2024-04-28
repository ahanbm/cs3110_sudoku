(**@authors: Peter Favero pmf66*)
(*To compile, run: dune test in the terminal *)

open OUnit2
open Cs3110_sudoku
include Diagonal_sudoku
include Connect_four

let trivial_test _ = assert (0 = 0)

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

let ounit2_tests =
  "ounit2 test suite"
  >::: [
         "a trivial test" >:: trivial_test;
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
       ]

(* RUN TESTS *)

let () =
  print_endline "\nOUnit2 tests:";
  run_test_tt_main ounit2_tests
