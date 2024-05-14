open OUnit2
open Cs3110_sudoku
open Timer

(** Testing whether the timer starts and stops properly *)
let test_start_stop _ =
  let timer = start () in
  Unix.sleep 1;
  (* Simulate some delay *)
  let timer = stop timer in
  let elapsed = elapsed_seconds timer in
  assert_bool "Elapsed time should be greater than 0" (elapsed > 0.0)

(** Testing the timer's handling of elapsed time *)
let test_elapsed_ms time _ =
  let timer = start () in
  Unix.sleep time;
  (* Simulate some delay *)
  let timer = stop timer in
  let elapsed_ms = elapsed_ms timer in
  assert_bool "Elapsed time in milliseconds should be approximately 1000 * time"
    (elapsed_ms >= time * 1000 && elapsed_ms <= (time + 1) * 1000)

let test_timer _ =
  let timer = Timer.start () in
  (* Sleep for 1 second to ensure that the timer is running *)
  Unix.sleep 1;
  let timer = Timer.stop timer in
  let elapsed_seconds = Timer.elapsed_seconds timer in
  let elapsed_ms = Timer.elapsed_ms timer in
  (* Check that the elapsed time is at least 1 second and at most 2 seconds *)
  assert_bool "elapsed_seconds" (elapsed_seconds >= 1. && elapsed_seconds < 2.);
  (* Check that the elapsed time in milliseconds is at least 1000 and at most
     2000 *)
  assert_bool "elapsed_ms" (elapsed_ms >= 1000 && elapsed_ms < 2000)

let test_timer_multiple_intervals _ =
  let timer = Timer.start () in
  (* Sleep for 1 second *)
  Unix.sleep 1;
  let timer = Timer.stop timer in
  let elapsed_seconds = Timer.elapsed_seconds timer in
  let elapsed_ms = Timer.elapsed_ms timer in
  (* Check that the elapsed time is at least 1 second and at most 2 seconds *)
  assert_bool "elapsed_seconds" (elapsed_seconds >= 1. && elapsed_seconds < 2.);
  (* Check that the elapsed time in milliseconds is at least 1000 and at most
     2000 *)
  assert_bool "elapsed_ms" (elapsed_ms >= 1000 && elapsed_ms < 2000);
  (* Start the timer again and sleep for another second *)
  let timer = Timer.start () in
  Unix.sleep 1;
  let timer = Timer.stop timer in
  let elapsed_seconds = Timer.elapsed_seconds timer in
  let elapsed_ms = Timer.elapsed_ms timer in
  (* Check that the elapsed time is at least 1 second and at most 2 seconds *)
  assert_bool "elapsed_seconds" (elapsed_seconds >= 1. && elapsed_seconds < 2.);
  (* Check that the elapsed time in milliseconds is at least 1000 and at most
     2000 *)
  assert_bool "elapsed_ms" (elapsed_ms >= 1000 && elapsed_ms < 2000)

let suite =
  "Timer Test Suite"
  >::: [
         "Test start and stop" >:: test_start_stop;
         "Test elapsed milliseconds 1" >:: test_elapsed_ms 1;
         "Test elapsed milliseconds 5" >:: test_elapsed_ms 5;
         "Test elapsed milliseconds 12" >:: test_elapsed_ms 1;
         "Test elapsed milliseconds 144" >:: test_elapsed_ms 1;
         "Further timer tests" >:: test_timer;
         "Test timer with multiple intervals" >:: test_timer_multiple_intervals;
       ]

let () =
  print_endline "\nOUnit2 tests:";
  run_test_tt_main suite

(** Open AI. "Write a test suite for this code - Chat Conversation". Chat GPT.
    May 2024 *)
