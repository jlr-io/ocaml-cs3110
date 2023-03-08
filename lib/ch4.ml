open Ch2
open Ch3

let rec repeat f n x = 
  if n = 0 then x else f (repeat f (n - 1) x)

(** [product_left list] computes the product of every element in [list] beginning from the left.  *)
let product_left list = List.fold_left ( *. ) 1.0 list

(** [product_right list] computes the product of every element in [list] beginning from the right.  *)
let product_right list = List.fold_right ( *. ) list 1.0

(** [sum_cube_odd n] computes the sum of the cubes of all the odd numbers from 0 to [n] inclusive. *)
let sum_cube_odd n =
  0 -- n
  |> List.filter (fun e -> e mod 2 = 1) 
  |> List.map (fun e -> (pow e 3))
  |> List.fold_left ( + ) 0 

