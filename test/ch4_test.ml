open OUnit2
open Lib.Ch4

let make_test name expected_output fn input =
  name >:: (fun _ -> assert_equal expected_output (fn input))

let repeat_tests = "Test suite for repeat" >::: [
  make_test "none" 0 (repeat (fun x -> x + 1) 0) 0;
  make_test "once" 1 (repeat (fun x -> x + 1) 1) 0;
  make_test "twice" 2 (repeat (fun x -> x + 1) 2) 0;
  make_test "thrice" 3 (repeat (fun x -> x + 1) 3) 0;
]

let _ = run_test_tt_main repeat_tests

let product_left_tests = "Test suite for product_left" >::: [
  make_test "empty" 1.0 product_left [];
  make_test "6.0" 6.0 product_left [1.0; 2.0; 3.0];
]

let _ = run_test_tt_main product_left_tests

let product_right_tests = "Test suite for product_right" >::: [
  make_test "empty" 1.0 product_right [];
  make_test "6.0" 6.0 product_right [1.0; 2.0; 3.0];
]

let _ = run_test_tt_main product_right_tests

let sum_cube_odd_tests = "Test suite for sum_cube_odd" >::: [
  make_test "empty" 0 sum_cube_odd 0;
  make_test "one" 1 sum_cube_odd 1;
  make_test "two" 1 sum_cube_odd 2;
  make_test "three" 28 sum_cube_odd 3;
  make_test "four" 28 sum_cube_odd 4;
]

let _ = run_test_tt_main sum_cube_odd_tests