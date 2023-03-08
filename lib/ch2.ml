let _ = 7 * (1 + 2 + 3)

let _ = "CS " ^ string_of_int 3110

let _ = 42 * 10

let _ = 3.13 /. 2.0

(** [pow b p] computes [b] raises to the [p] *)
let rec pow b p = 
  match p with 
  | 0 -> 1
  | _ -> b * pow b (p-1)

(** [f_pow b p] computes the power of [b] to [p] *)
let rec f_pow b p = 
  match p with 
  | 0 -> 1. 
  | _ -> b *. f_pow b (p - 1)

let _ = f_pow 4.2 7

let _ = 42 = 42

let _ = "hi" = "hi"

let _ = "hi" == "hi"