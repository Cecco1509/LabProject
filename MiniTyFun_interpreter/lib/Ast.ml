type ide = string;;

type ty = 
  | TInt
  | TBool
  | TFunctional of ty * ty
  | UnBound
;;

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
  | LetFun of ide * ide * ty * term * term
  | Fun of ide * ty * term
  | FunApp of term * term
;;

(* Polimorph enviroment, it will be used for types and value *)
type 'a env = ide -> 'a
;;

type envT = 
  | Int of int
  | Bool of bool              (* Var -> value    Var -> type *)
  | Closure of ide * ty * term * (envT env) * (ty env)
  | RecClosure of ide * ide * ty * term * (envT env) * (ty env)
  | UnBound
;;

(* Empty env for type checking *)
let emptycxt : ty env = function _x -> UnBound;;

(* Empty env for term evaluation*)
let emptyenv : (ide -> envT) = function _x -> UnBound;;


let bind (s: 'a env) (x: ide) (v: 'a) =
  function (i: ide) -> if (i = x) then v else (s i);;

(* Helper function to print the value of a variable in the environment *)


let var_to_string (e: envT env) (x: ide) : string =
  match e x with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | (Closure _) | (RecClosure _) -> "Function"
  | UnBound -> "Variable not found"
;;

let rec type_to_string (t: ty option) : string =
  match t with
  | Some(TInt) -> "TInt"
  | Some(TBool) -> "TBool"
  | Some(TFunctional(t1, t2)) -> "TFunctional(" ^ (type_to_string (Some(t1))) ^ ", " ^ (type_to_string (Some(t2))) ^ ")"
  | _ -> "None"
;;

let value_to_string (v: envT) : string =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Closure (x, ty, _, _, _) -> "Fun (" ^ x ^ "): " ^ (type_to_string (Some ty))
  | RecClosure (f, x, ty, _, _, _) -> "RecFun " ^ f ^ " (" ^ x ^ "): " ^ (type_to_string (Some ty))
  | UnBound -> "UnBound"