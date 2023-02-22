open OUnit2
open Lib.Ch3

let make_test name expected_output fn input =
  name >:: (fun _ -> assert_equal expected_output (fn input))

let product_tests = "Test product function" >::: [
  make_test "empty" 1 product [];
  make_test "one" 1 product [1];
  make_test "two" 2 product [1; 2];
]

let _ = run_test_tt_main product_tests
let has_bigred_tests = "Test suite has_bigred" >::: [
  make_test "empty" false has_bigred [];
  make_test "does not have" false has_bigred ["red"];
  make_test "has first" true has_bigred ["bigred"; "blue"];
  make_test "has second" false has_bigred ["red"; "big_red"];
]

let _ = run_test_tt_main has_bigred_tests

let has_two_or_four_tests = "Test suite for has_two_or_four" >::: [
  make_test "empty" false has_two_or_four [];
  make_test "one" false has_two_or_four [1];
  make_test "two" true has_two_or_four [1; 2];
  make_test "three" false has_two_or_four [1; 2; 3];
  make_test "four" true has_two_or_four [1; 2; 3; 4];
]

let _ = run_test_tt_main has_two_or_four_tests