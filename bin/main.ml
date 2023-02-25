open Lib.Ch3
let int_list = [1; 2; 3; 4; 5]
let string_list = ["hello"; ", "; "world"; "!"]

let () = print_int (product int_list) 
let () = print_endline (concat string_list)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
let ( -- ) i j = from i j []

let long_list = 0 -- 100_000_000
let drop_test = drop 1000000 long_list

let () = print_int (List.length drop_test)