(**@author: Peter Favero pmf66*)
(*To compile, run:
   dune exec ./bin/main.exe 
  in the terminal *)


(* Initialize a 9x9 array with zeros *)
let sudoku_grid = Array.make_matrix 9 9 0;;

(* Change the values of a few cells *)
sudoku_grid.(0).(1) <- 5;
sudoku_grid.(0).(7) <- 6;
sudoku_grid.(1).(4) <- 3;
sudoku_grid.(1).(5) <- 8;
sudoku_grid.(2).(3) <- 1;
sudoku_grid.(3).(2) <- 6;
sudoku_grid.(4).(1) <- 9;
sudoku_grid.(4).(6) <- 2;
sudoku_grid.(5).(7) <- 8;
sudoku_grid.(6).(2) <- 3;
sudoku_grid.(7).(3) <- 4;
sudoku_grid.(7).(4) <- 5;
sudoku_grid.(8).(0) <- 7;
sudoku_grid.(8).(6) <- 9;;

(* This code was generated using information from GPT-4 *)
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

(* Execute the print function to display the grid *)
print_sudoku_grid sudoku_grid;;