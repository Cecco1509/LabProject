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

let emptyenv = function x -> UnBound;;

(* Helper function to print the value of a variable in the environment *)
let print_var (e: env) (var: ide) : unit =
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

let rec eval (e: env) (t: term) : envT =
  match t with
  | TNum n -> Int n
  | TBool b -> Bool b
  | Var x -> (match e x with
    | UnBound -> failwith ("Variable " ^ x ^ " not found")
    | v -> v)
  | Fun (x, t) -> Closure(x, t, e)
  | Let (x, t1, t2) -> eval (bind e x (eval e t1)) t2
  | LetFun (f, x, t1, t2) -> eval (bind e f (RecClosure(f, x, t1, e))) t2
  | FunApp (f, t) -> (
    match f with
      | Var(f) -> (
        match e f with
          | Closure (x, t', e') -> eval (bind e' x (eval e t)) t'
          | _ -> failwith ("Function " ^ f ^ " variable not found"))
      | Fun (x, t') -> eval (bind e x (eval e t)) t'
      | FunApp (_,_) | RecFunApp (_,_) -> let ef = eval e f in
        (match ef with
          | Closure (x, t', e') -> eval (bind e' x (eval e t)) t'
          | _ -> failwith "FunApp (_,_) Not a function"
        )
      | _ -> failwith "Not a function"
      )
  | RecFunApp (f, t) -> (
    match f with
      | Var(f) -> (
        match e f with
          | RecClosure (f, x, t', e') -> eval (bind (bind e' f (RecClosure(f, x, t', e'))) x (eval e t)) t'
          | _ -> failwith ("Recursive Function: " ^ f ^ " variable not found")
        )
      | _ -> failwith ("Recursive Function: first parameter is not a variable")
    )
  | Plus (t1, t2) -> (
    match (eval e t1, eval e t2) with
      | (Int n1, Int n2) -> Int (n1 + n2)
      | _ -> failwith "Plus Type error"
    )
  | Minus (t1, t2) -> (
    match (eval e t1, eval e t2) with
      | (Int n1, Int n2) -> Int (n1 - n2)
      | _ -> failwith "Minus Type error"
    )
  | Times (t1, t2) -> (
    match (eval e t1, eval e t2) with
      | (Int n1, Int n2) -> Int (n1 * n2)
      | _ -> failwith "Times Type error"
    )
  | And (t1, t2) -> (
    match (eval e t1, eval e t2) with
      | (Bool b1, Bool b2) -> Bool (b1 && b2)
      | _ -> failwith "And Type error"
    )
  | Not t -> (  
    match eval e t with
      | Bool b -> Bool (not b)
      | _ -> failwith "Not Type error"
    )
  | Less (  t1, t2) -> (
    match (eval e t1, eval e t2) with
      | (Int n1, Int n2) -> Bool (n1 < n2)
      | _ -> failwith "Less Type error"
    )
  | IfThenElse (t1, t2, t3) -> (
    match eval e t1 with
      | Bool true -> eval e t2
      | Bool false -> eval e t3
      | _ -> failwith "IfThenElse Type error"
    )
;;
(* test *)

let _ =
  let env0 = emptyenv in

  (* Test 35 - Complex nested functions and conditionals *)
  let t35 = LetFun ("complex_func", "n",
              IfThenElse (Less (Var "n", TNum 1), TNum 0,
              IfThenElse (Less (Var "n", TNum 2), TNum 1,
              Plus (RecFunApp (Var "complex_func", Minus (Var "n", TNum 1)),
                    RecFunApp (Var "complex_func", Minus (Var "n", TNum 2))))),
              Let ("a", TNum 5,
              Let ("b", TNum 3,
              Let ("c", TNum 2,
              Let ("d", RecFunApp (Var "complex_func", Var "a"),
              Let ("e", RecFunApp (Var "complex_func", Var "b"),
              Let ("f", RecFunApp (Var "complex_func", Var "c"),
              Plus (Plus (Var "d", Var "e"), Var "f")))))))) in
  let v35 = eval env0 t35 in
  let expected35 = 10 in (* complex_func(5) + complex_func(3) + complex_func(2) = 5 + 2 + 3 = 10 *)
  Printf.printf "Test 35 - Complex nested functions and conditionals: Expected: %d, Result: %d\n" expected35 (match v35 with Int n -> n | _ -> -1);
;;
