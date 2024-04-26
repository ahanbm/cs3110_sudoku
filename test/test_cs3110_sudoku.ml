(**@authors: Peter Favero pmf66*)
(*To compile, run: 
   dune test 
 in the terminal *)

open OUnit2
open Diagonal_sudoku

let trivial_test _ = assert (0 = 0)

let empty_grid_rows_and_cols_test _ =
  let sudoku_grid = Array.make_matrix 9 9 0 in
  let erroneous_rows, completed_rows = check_all_rows sudoku_grid in
  let erroneous_cols, completed_cols = check_all_cols sudoku_grid in
  assert (
    cardinality_of_int_set erroneous_rows = 0 
    && cardinality_of_int_set completed_rows = 0
    && cardinality_of_int_set erroneous_cols = 0
    && cardinality_of_int_set completed_cols = 0
  )

let empty_grid_boxes_test _ =
  let sudoku_grid = Array.make_matrix 9 9 0 in
  let erroneous_boxes, completed_boxes = check_all_boxes sudoku_grid in
  assert (
    cardinality_of_pair_set erroneous_boxes = 0 
    && cardinality_of_pair_set completed_boxes = 0
  )

let empty_grid_diagonals_test _ =
  let sudoku_grid = Array.make_matrix 9 9 0 in
  let ld_erroneous, ld_complete = check_left_diagonal sudoku_grid in
  let rd_erroneous, rd_complete = check_right_diagonal sudoku_grid in
  assert (
    not ld_complete  
    && not ld_erroneous
    && not rd_complete
    && not rd_erroneous
  )

let ounit2_tests = "ounit2 test suite" >::: [
  "a trivial test" >:: trivial_test;
  "An empty grid should have no erroneous rows and no completed rows" >:: empty_grid_rows_and_cols_test;
  "An empty grid should have no erroneous boxes and no completed boxes" >:: empty_grid_boxes_test;
  "An empty grid should have no erroneous diagonals and no completed diagonals" >:: empty_grid_diagonals_test;
]

(* RUN TESTS *)

let () =
  print_endline "\nOUnit2 tests:";
  run_test_tt_main ounit2_tests;