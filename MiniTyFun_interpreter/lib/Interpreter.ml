module Interpreter = struct

  open Ast
  open TypeChecker

  let rec eval (e: envT env) (ty_e: ty env) (t: term) : envT =
    match t with
    | TNum n -> Int n
    | TBool b -> Bool b
    | Var x -> (match e x with
      | UnBound -> failwith ("Variable " ^ x ^ " not found")
      | v -> v)
    | Fun (x, ty, t) -> (
      match TypeChecker.check_type (bind ty_e x ty) t with
      | Some(ty'') -> Closure(x, TFunctional(ty, ty''), t, e, (bind ty_e x ty))
      | None -> failwith "Function body type error"
    )
    | Let (x, t1, t2) -> (
      let t1_type = TypeChecker.check_type ty_e t1 in
      match t1_type with
      | Some(ty) -> eval (bind e x (eval e ty_e t1)) (bind ty_e x ty) t2
      | None -> failwith "Let binding type error"
      )
    | LetFun (f, x, ty, t1, t2) -> (
      match ty with
      | TFunctional(ty', ty'') -> (
        let ty_e' = bind ty_e f ty in
        match TypeChecker.check_type (bind ty_e' x ty') t1 with
        | Some(result_type) when result_type = ty'' -> eval (bind e f (RecClosure(f, x, ty, t1, e, (bind ty_e' x ty')))) ty_e' t2
        | Some(_) -> failwith "LetFun body type error"
        | None -> failwith "LetFun type error"
      )
      | _ -> failwith "LetFun type error"
    )
    | FunApp (t1, t2) -> (
      let t1_type = TypeChecker.check_type ty_e t1 in
      let t2_type = TypeChecker.check_type ty_e t2 in
      match (t1_type, t2_type) with
        | (Some(TFunctional(ty, _)), Some(ty')) when ty = ty' -> (
          let t1' = eval e ty_e t1 in
          let t2' = eval e ty_e t2 in
           match t1' with
            | Closure (x, _, t, e', ty_e') -> 
              let ty_e'' = bind ty_e' x ty in
              let e''    = bind e' x t2' in
              eval e'' ty_e'' t
            | RecClosure (fn, x, fn_type, t, e', ty_e') ->
              let ty_e'' = bind (bind ty_e' fn fn_type) x ty in
              let e' = bind (bind e' fn (RecClosure(fn, x, fn_type, t, e', ty_e'))) x t2' in
              eval e' ty_e'' t
            | _ -> failwith "FunApp Type error"
        )
        | _ -> failwith "Function application type error"
    )
    | Plus (t1, t2) -> (
      let t1_type = TypeChecker.check_type ty_e t1 in
      let t2_type = TypeChecker.check_type ty_e t2 in
      match (t1_type, t2_type) with
        | (Some(TInt), Some(TInt)) -> (
            match (eval e ty_e t1, eval e ty_e t2) with
              | (Int n1, Int n2) -> Int (n1 + n2)
              | _ -> failwith "Plus Type error"
          )
        | _ -> failwith "Plus Type error"
      )
    | Minus (t1, t2) -> (
      let t1_type = TypeChecker.check_type ty_e t1 in
      let t2_type = TypeChecker.check_type ty_e t2 in
      match (t1_type, t2_type) with
        | (Some(TInt), Some(TInt)) -> (
            match (eval e ty_e t1, eval e ty_e t2) with
              | (Int n1, Int n2) -> Int (n1 - n2)
              | _ -> failwith "Minus Type error"
          )
        | _ -> failwith "Minus Type error"
      )
    | Times (t1, t2) -> (
      let t1_type =TypeChecker.check_type ty_e t1 in
      let t2_type =TypeChecker.check_type ty_e t2 in
      match (t1_type, t2_type) with
        | (Some(TInt), Some(TInt)) -> (
            match (eval e ty_e t1, eval e ty_e t2) with
              | (Int n1, Int n2) -> Int (n1 * n2)
              | _ -> failwith "Times Type error"
          )
        | _ -> failwith "Times Type error"
      )
    | And (t1, t2) -> (
      let t1_type =TypeChecker.check_type ty_e t1 in
      let t2_type =TypeChecker.check_type ty_e t2 in
      match (t1_type, t2_type) with
        | (Some(TBool), Some(TBool)) -> (
            match (eval e ty_e t1, eval e ty_e t2) with
              | (Bool b1, Bool b2) -> Bool (b1 && b2)
              | _ -> failwith "And Type error"
          )
        | _ -> failwith "And Type error"
      )
    | Not t -> (
      let t_type =TypeChecker.check_type ty_e t in
      match t_type with
        | Some(TBool) -> (
          match eval e ty_e t with
            | Bool b -> Bool (not b)
            | _ -> failwith "Not Type error"
        )
        | _ -> failwith "Not Type error"
      )
    | Less (t1, t2) -> (
      let t1_type =TypeChecker.check_type ty_e t1 in
      let t2_type =TypeChecker.check_type ty_e t2 in
      match (t1_type, t2_type) with
        | (Some(TInt), Some(TInt)) -> (
            match (eval e ty_e t1, eval e ty_e t2) with
              | (Int n1, Int n2) -> Bool (n1 < n2)
              | _ -> failwith "Less Type error"
          )
        | _ -> failwith "Less Type error"
      )
    | IfThenElse (t1, t2, t3) -> (
      let t1_type =TypeChecker.check_type ty_e t1 in
      let t2_type =TypeChecker.check_type ty_e t2 in
      let t3_type =TypeChecker.check_type ty_e t3 in
      match (t1_type, t2_type, t3_type) with
        | (Some(TBool), Some(t'), Some(t'')) when t' = t'' -> (
            match eval e ty_e t1 with
              | Bool true -> eval e ty_e t2
              | Bool false -> eval e ty_e t3
              | _ -> failwith "IfThenElse Branches Type error"
          )
        | _ -> failwith "IfThenElse Condition Type error"
      )
  ;;

  let print_var (e: envT env) (ty_e: ty env) (var: ide) : unit =
    match (e var, ty_e var) with
    | (Int value, _ty) -> Printf.printf "Variable %s: %d\n" var value
    | (Bool value, _ty) -> Printf.printf "Variable %s: %b\n" var value
    | ((Closure _, _) | (RecClosure _, _)) -> Printf.printf "Variable %s: Function\n" var
    | (UnBound, _ty) -> Printf.printf "Variable %s not found in the environment\n" var
  ;;

end