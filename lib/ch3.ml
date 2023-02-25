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

  (** [has_first_two_equal list] returns true when the first two elements of the list are equal  *)
let has_first_two_equal = function
  | [] -> false
  | [_] -> false
  | h1 :: h2 :: _ -> h1 = h2

(** [return_5th list] returns the fifth element of a list or 0 if there are less than 5 elements *)
let return_5th list = 
  if List.length list >= 5 then List.nth list 4 else 0

(** [sort_list_descending (int list) list] sorts a list of ints in descending order *)
let sort_list_descending list = List.rev (List.sort compare list)

(** [last_element_of_list list] returns the last element of a list assumes the list is not empty *)
let last_element_of_list list = List.nth (List.rev list) 0

(** [any zeroes (list: int list): bool] returns true if the list contains a 0 otherwise it returns false*)
let any_zeroes list = List.exists (fun e -> e = 0) list

(* returns:  [take n lst] is the first [n] elements of [lst], or
 *   just [lst] if [lst] has fewer than [n] elements. 
 * requires: [n >= 0]
*)
let rec take n list = 
  if n = 0 then [] else match list with
    | [] -> []
    | h :: t -> h :: take (n-1) t

(* returns:  [drop n lst] is all but the first [n] elements of [lst],
 *   or just [[]] if [lst] has fewer than [n] elements.
 * requires: [n >= 0]
*)
let rec drop n list = 
  if n = 0 then list else match list with
    | [] -> []
    | _ :: t -> drop (n-1) t

    (* skipping some exercises *)

(* we are given this type *)
type student = { first_name : string ; last_name : string ; gpa : float }

(* expression with type [student] *)
let s = 
  { first_name = "Ezra"; last_name = "Cornell"; gpa = 4.3 }

(* expression with type [student -> string * string] *)
let get_full_name student = 
  student.first_name, student.last_name

(* expression with type [string -> string -> float -> student] *)
let make_student first last g = 
  { first_name = first; last_name = last; gpa=g }

type poketype = Normal | Fire | Water

type pokemon = { name: string; hp: int; ptype: poketype}

let charizard = { name="Charizard"; hp=78; ptype=Fire }

let squirtle = {name="Squirtle"; hp=44; ptype=Water}


let safe_hd = function
  | [] -> None
  | h :: _ -> Some (h)

let safe_tl = function
  | [] -> None
  | _ :: t -> Some (t)

let max_hp = function
  | [] -> 0
  | h :: t -> List.fold_left (fun acc e -> if e.hp > acc then e.hp else acc) h.hp t

(** [insert k v lst] is an association list that binds key [k] to value [v]
  and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [lookup k lst] is [Some v] if association list [lst] binds key [k] to
  value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t

let num_map = [] |> insert 1 "one" |> insert 2 "two" |> insert 3 "three"

let _ = lookup 2 num_map

let _ = lookup 4 num_map

type suit = Spades | Hearts | Diamonds | Clubs
type rank = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type card = { rank: rank; suit: suit;  }
let ace_of_clubs = { rank = Ace; suit = Clubs;  }
let two_of_diamonds = { rank = Two; suit = Diamonds; }
let seven_of_spades = { rank = Seven; suit = Spades; }

type quad = I | II | III | IIII
type sign = Neg | Zero | Pos

let sign (x: int): sign = 
  match x with 
    | 0 -> Zero
    | _ -> if x > 0 then Pos else Neg

let quadrant : int*int -> quad option = fun (x, y) ->
  match sign x, sign y with
    | Pos, Pos -> Some I
    | Neg, Pos -> Some II
    | Neg, Neg -> Some III
    | Pos, Neg -> Some IIII
    | _, _ -> None

let quadrant_when : int*int -> quad option = function 
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IIII
  | _, _ -> None

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec depth = function 
  | Leaf -> 0
  | Node (_, left, right) -> max (1 + depth left) (1 + depth right)

let rec same_shape t1 t2 =
  match t1, t2 with
  | Leaf, Leaf -> true
  | Node(_, l1, r1), Node(_, l2, r2) -> (same_shape l1 l2) && (same_shape r1 r2)
  | _ -> false

(** [list_max (list: int list): int] returns the max value in the list.
    Raises: [list_max] if [list] is empty  *)
let rec list_max = function
    | [] -> failwith "int_max"
    | h :: [] -> h
    | h :: t -> max h (list_max t)