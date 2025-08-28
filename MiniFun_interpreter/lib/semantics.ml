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
  | RecFunApp of term * term
;;

type env = ide -> envT

and envT = 
  | Int of int
  | Bool of bool
  | Closure of ide * term * env
  | RecClosure of ide * ide * term * env
  | UnBound
;;

let emptyenv = function _x -> UnBound;;

(* Helper function to print the value of a variable in the environment *)
let _print_var (e: env) (var: ide) : unit =
  match e var with
  | Int value -> Printf.printf "Variable %s: %d\n" var value
  | Bool value -> Printf.printf "Variable %s: %b\n" var value
  | Closure _ | RecClosure _ -> Printf.printf "Variable %s: Function\n" var
  | UnBound -> Printf.printf "Variable %s not found in the environment\n" var
;;

let bind (s: env) (x: ide) (v: envT) =
  function (i: ide) -> 
    if (i = "show") then (
      Printf.printf "var: %s, value: %d\n" x (match v with 
                                                | Int n -> n 
                                                | Bool b -> if b then 1 else 0
                                                | Closure _ -> 2
                                                | RecClosure _ -> 3
                                                | _ -> -1);
      s "show"
    ) else if (i = x) then v else (s i);;

let rec eval_term (e: env) (t: term) : envT =
  match t with
  | TNum n -> Int n
  | TBool b -> Bool b
  | Var x -> (match e x with
    | UnBound -> failwith ("Variable " ^ x ^ " not found")
    | v -> v)
  | Fun (x, t) -> Closure(x, t, e)
  | Let (x, t1, t2) -> eval_term (bind e x (eval_term e t1)) t2
  | LetFun (f, x, t1, t2) -> eval_term (bind e f (RecClosure(f, x, t1, e))) t2
  | FunApp (t1, t2) -> (
    let t1' = eval_term e t1 in
    let t2' = eval_term e t2 in
    match t1' with
      | Closure (x, t, e') -> eval_term (bind e' x t2') t
      | RecClosure (f, x, t, e') -> eval_term (bind (bind e' f (RecClosure(f, x, t, e'))) x t2') t   (* Rec fun application *)
      | _ -> failwith "FunApp Type error"
    )
  | RecFunApp (t1, t2) -> (
    let t1' = eval_term e t1 in
    let t2' = eval_term e t2 in
    match t1' with
      | RecClosure
          (f, x, t, e') -> eval_term (bind (bind e' f (RecClosure(f, x, t, e'))) x t2') t
      | _ -> failwith "RecFunApp Type error"
    )
  | Plus (t1, t2) -> (
    match (eval_term e t1, eval_term e t2) with
      | (Int n1, Int n2) -> Int (n1 + n2)
      | _ -> failwith "Plus Type error"
    )
  | Minus (t1, t2) -> (
    match (eval_term e t1, eval_term e t2) with
      | (Int n1, Int n2) -> Int (n1 - n2)
      | _ -> failwith "Minus Type error"
    )
  | Times (t1, t2) -> (
    match (eval_term e t1, eval_term e t2) with
      | (Int n1, Int n2) -> Int (n1 * n2)
      | _ -> failwith "Times Type error"
    )
  | And (t1, t2) -> (
    match (eval_term e t1, eval_term e t2) with
      | (Bool b1, Bool b2) -> Bool (b1 && b2)
      | _ -> failwith "And Type error"
    )
  | Not t -> (  
    match eval_term e t with
      | Bool b -> Bool (not b)
      | _ -> failwith "Not Type error"
    )
  | Less (  t1, t2) -> (
    match (eval_term e t1, eval_term e t2) with
      | (Int n1, Int n2) -> Bool (n1 < n2)
      | _ -> failwith "Less Type error"
    )
  | IfThenElse (t1, t2, t3) -> (
    match eval_term e t1 with
      | Bool true -> eval_term e t2
      | Bool false -> eval_term e t3
      | _ -> failwith "IfThenElse Type error"
    )
;;

(* let eval (t: term) (input: int) : string =
  let t' = FunApp(t, TNum(input)) in
  match (eval_term emptyenv t') with
  | Int n -> string_of_int n
  | Bool b -> if b then "true" else "false"
  | Closure _ | RecClosure _ -> "function"
  | _ -> "Program did not return any function to run against the input value" *)

let eval (t: term) (input: int) : string =
  (*let t' = FunApp(t, TNum(input)) in*)
  let result : envT = (
    match (eval_term emptyenv t) with
    | Int n -> Int n
    | Closure (x, body, e') -> eval_term (bind e' x (Int input)) body
    | RecClosure (f, x, body, e') -> eval_term (bind (bind e' f (RecClosure(f, x, body, e'))) x (Int input)) body
    | _ -> failwith "term did not return any function to run against the input value nor an integer"
    ) in
  match result with
  | Int n -> string_of_int n
  | _ -> failwith "the evaluation of the term did not return an integer value"
(* test *)


(*

    match f with
      | Var(f) -> (
        match e f with
          | Closure (x, t', e') -> eval_term (bind e' x (eval_term e t)) t'
          | _ -> failwith ("Function " ^ f ^ " variable not found"))
      | Fun (x, t') -> eval_term (bind e x (eval_term e t)) t'
      | FunApp (_,_) | RecFunApp (_,_) -> let ef = eval_term e f in
        (match ef with
          | Closure (x, t', e') -> eval_term (bind e' x (eval_term e t)) t'
          | _ -> failwith "FunApp (_,_) Not a function"
        )
      | _ -> failwith "Not a function"

*)