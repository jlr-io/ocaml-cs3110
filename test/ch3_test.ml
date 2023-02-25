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
  make_test "has one" false has_two_or_four [1];
  make_test "has two" true has_two_or_four [1; 2];
  make_test "has three" false has_two_or_four [1; 2; 3];
  make_test "has four" true has_two_or_four [1; 2; 3; 4];
]

let _ = run_test_tt_main has_two_or_four_tests

let has_first_two_equal_tests = "Test suite for has_first_two_equal" >::: [
  make_test "empty" false has_first_two_equal [];
  make_test "one" false has_first_two_equal [1];
  make_test "first two equal" true has_first_two_equal [1; 1];
  make_test "first two unequal" false has_first_two_equal [1; 2; 3];
]

let _ = run_test_tt_main has_first_two_equal_tests

let return_5th_tests = "Test suite for return_5th" >::: [
  make_test "empty" 0 return_5th [];
  make_test "one" 0 return_5th [1];
  make_test "two" 0 return_5th [1; 2];
  make_test "three" 0 return_5th [1; 2; 3];
  make_test "four" 0 return_5th [1; 2; 3; 4];
  make_test "five" 5 return_5th [1; 2; 3; 4; 5];
  make_test "six" 5 return_5th [1; 2; 3; 4; 5; 6];
]

let _ = run_test_tt_main return_5th_tests

let sort_list_descending_tests = "Test suite for sort_list_descending" >::: [
  make_test "empty" [] sort_list_descending [];
  make_test "one" [1] sort_list_descending [1];
  make_test "two" [2; 1] sort_list_descending [1; 2];
  make_test "three" [3; 2; 1] sort_list_descending [1; 2; 3];
  make_test "four" [4; 3; 2; 1] sort_list_descending [1; 2; 3; 4];
  make_test "five" [5; 4; 3; 2; 1] sort_list_descending [1; 2; 3; 4; 5];
  make_test "six" [6; 5; 4; 3; 2; 1] sort_list_descending [1; 2; 3; 4; 5; 6];
]

let _ = run_test_tt_main sort_list_descending_tests

let last_element_of_list_tests = "Test suite for last_element_of_list" >::: [
  make_test "one" 1 last_element_of_list [1];
  make_test "two" 2 last_element_of_list [1; 2];
  make_test "three" 3 last_element_of_list [1; 2; 3];
  make_test "four" 4 last_element_of_list [1; 2; 3; 4];
  make_test "five" "five" last_element_of_list ["one"; "two"; "three"; "four"; "five"];
  make_test "six" 6. last_element_of_list [1.; 2.; 3.; 4.; 5.; 6.];
]

let _ = run_test_tt_main last_element_of_list_tests

let any_zeroes_tests = "Test suite for any_zeroes" >::: [
  make_test "empty" false any_zeroes [];
  make_test "no zero" false any_zeroes [1];
  make_test "one zero" true any_zeroes [0];
]

let _ = run_test_tt_main any_zeroes_tests

let take_tests = "Test suite for take" >::: [
  make_test "empty" [] (take 0) [];
  make_test "one" [1] (take 1) [1];
  make_test "two" [1; 2] (take 2) [1; 2];
  make_test "three" [1; 2; 3] (take 3) [1; 2; 3; 4; 5; 6];
]

let _ = run_test_tt_main take_tests

let drop_tests = "Test suite for drop" >::: [
  make_test "empty" [] (drop 0) [];
  make_test "one" [] (drop 1) [1];
  make_test "two" [] (drop 2) [1; 2];
  make_test "three" [4; 5; 6] (drop 3) [1; 2; 3; 4; 5; 6];
]

let _ = run_test_tt_main drop_tests

let safe_hd_tests = "Test suite for safe_hd" >::: [
  make_test "empty" None safe_hd [];
  make_test "some" (Some 1) safe_hd [1];
]

let _ = run_test_tt_main safe_hd_tests

let safe_tl_tests = "Test suite for safe_tl" >::: [
  make_test "empty" None safe_tl [];
  make_test "some" (Some [2; 3]) safe_tl [1; 2; 3];
]

let _ = run_test_tt_main safe_tl_tests

let t =
  Node(1,
    Leaf,
    Node(2,
      Leaf,
      Leaf
    )
  )

let node = Node("a", Node("b", Leaf, Leaf), Node("c", Leaf, Leaf))
let t2 =
  Node("a",
    Node("b",
    node,
    node
    ),
    Node("c",
      Leaf,
      Leaf
    )
  )
let depth_tests = "Test suite for depth" >::: [
  make_test "2" 2 depth t;
  make_test "4" 4 depth t2;
]

let _ = run_test_tt_main depth_tests

let same_shape_tests = "Test suite for same_shape" >::: [
  make_test "false" false (same_shape t) t2
]

let _ = run_test_tt_main same_shape_tests

let list_max_tests = "Test suite for list_max" >::: [
  make_test "10" 10 list_max [1; 2; 10;];
  make_test "5" 5 list_max [5; 4; 3; 2; 1];
  (* make_test "error" (failwith "list_max") list_max [] *)
]

let _ = run_test_tt_main list_max_tests