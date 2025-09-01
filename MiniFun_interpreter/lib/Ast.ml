type ide = string;;

type term =
  | TNum of int
  | TBool of bool
  | Var of ide
  | Plus of term * term
  | Minus of term * term
  | Times of term * term
  | And of term * term
  | Not of term
  | Less of term * term
  | Let of ide * term * term
  | IfThenElse of term * term * term
  | LetFun of ide * ide * term * term
  | Fun of ide * term
  | FunApp of term * term
;;

type env = ide -> envT

and envT = 
  | Int of int
  | Bool of bool
  | Closure of ide * term * env
  | RecClosure of ide * ide * term * env
  | UnBound
;;

let emptyenv : ide -> envT = fun _x -> UnBound;;

let bind (s: env) (x: ide) (v: envT) =
  function (i: ide) -> if (i = x) then v else (s i);;