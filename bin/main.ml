open Lib.Ch3
let int_list = [1; 2; 3; 4; 5]
let string_list = ["hello"; ", "; "world"; "!"]

let () = print_int (product int_list) 
let () = print_endline (concat string_list)
