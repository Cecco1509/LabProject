type prog = Main of string * string * c

and var = string 

and aexp =
  | Num of int
  | Var of string
  | Plus of aexp * aexp
  | Minus of aexp * aexp
  | Times of aexp * aexp

and bexp =
  | Bool of bool
  | And of bexp * bexp
  | Not of bexp
  | Less of aexp * aexp

and c = 
  | Skip
  | Assign of var * aexp
  | Seq of c * c
  | If of bexp * c * c
  | While of bexp * c
;;

let rec string_of_aexp (a: aexp) : string =
  match a with
  | Num n -> string_of_int n
  | Var v -> v
  | Plus (a1, a2) -> string_of_aexp a1 ^ " + " ^ string_of_aexp a2
  | Minus (a1, a2) -> string_of_aexp a1 ^ " - " ^ string_of_aexp a2
  | Times (a1, a2) -> string_of_aexp a1 ^ " * " ^ string_of_aexp a2
  
let rec string_of_bexp (b: bexp) : string =
  match b with
  | Bool b -> string_of_bool b
  | And (b1, b2) -> string_of_bexp b1 ^ " and " ^ string_of_bexp b2
  | Not b -> "not " ^ string_of_bexp b
  | Less (a1, a2) -> string_of_aexp a1 ^ " < " ^ string_of_aexp a2

(* The rest of the code remains unchanged *)

