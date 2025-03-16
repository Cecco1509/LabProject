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
  | Let of var * aexp
  | Seq of c * c
  | If of bexp * c * c
  | While of bexp * c

type env = var -> int option;;

val eval_prg : prog -> int -> int option