module Interpreter = struct

  open Ast

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
        | RecClosure (f, x, t, e') -> eval_term (bind (bind e' f (RecClosure(f, x, t, e'))) x t2') t
        | _ -> failwith "FunApp Type error"
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
  
end