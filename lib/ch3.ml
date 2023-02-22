let rec product = function
  | [] -> 1
  | h :: t -> h * product t

let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

(** [has_bigred list] returns true when the first element is "bigred" *)
let has_bigred = function
  | [] -> false
  | h :: _ -> h = "bigred"

(** [has_two_or_four list] returns true when the list has length 2 or 4 *) 
let has_two_or_four = function
  | [] -> false
  | [_; _;] -> true
  | [_; _; _; _;] -> true
  | _ :: _ -> false