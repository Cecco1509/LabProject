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
  | RecFunApp of term * term
;;

(* Polimorph enviroment, it will be used for types and value *)
type 'a env = ide -> 'a
;;

type envT = 
  | Int of int
  | Bool of bool
  | Closure of ide * ty * term * (envT env)
  | RecClosure of ide * ide * ty * term * (envT env)
  | UnBound
;;

let emptyenv : (ide -> envT) = function x -> UnBound;;
let emptycxt : (ide -> ty)   = function x -> UnBound;;

(* Helper function to print the value of a variable in the environment *)
let print_var (e: envT env) (var: ide) : unit =
  match e var with
  | Int value -> Printf.printf "Variable %s: %d\n" var value
  | Bool value -> Printf.printf "Variable %s: %b\n" var value
  | Closure _ | RecClosure _ -> Printf.printf "Variable %s: Function\n" var
  | UnBound -> Printf.printf "Variable %s not found in the environment\n" var
;;

let var_to_string (e: envT env) (x: ide) : string =
  match e x with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Closure _ | RecClosure _ -> "Function"
  | UnBound -> "Variable not found"

let rec type_to_string (t: ty option) : string =
  match t with
  | Some(TInt) -> "TInt"
  | Some(TBool) -> "TBool"
  | Some(TFunctional(t1, t2)) -> "TFunctional(" ^ (type_to_string (Some(t1))) ^ ", " ^ (type_to_string (Some(t2))) ^ ")"
  | _ -> "None"
;;

(* Write a function that given a ctx and a list of ide prints all the variables and binded type in it*)
let print_ctx (e: ty env) : string =
  let rec print_ctx' (e: ty env) (l: ide list) : string =
    match l with
    | [] -> ""
    | x::xs -> x ^ ": " ^ (type_to_string (Some(e x))) ^ "\n" ^ (print_ctx' e xs)
  in print_ctx' e ["x"; "y"; "compose"; "power"; "h"; "increment"] (*"z"; "w"; "v"; "u"; "t"; "s"; "r"; "q"; "p"; "o"; "n"; "m"; "l"; "k"; "j"; "i"; "h"; "g"; "f"; "e"; "d"; "c"; "b"; "a"]*)
;;


let bind (s: 'a env) (x: ide) (v: 'a) =
  function (i: ide) -> 
    (* if (i = "show") then (
      Printf.printf "var: %s, value: %d\n" x (match v with 
                                                | Int n -> n 
                                                | Bool b -> if b then 1 else 0
                                                | Closure _ -> 2
                                                | RecClosure _ -> 3
                                                | _ -> -1);
      s "show"
    ) else*) if (i = x) then v else (s i);;


let rec check_type (e: ty env) (t: term) : ty option =
  match t with
  | TNum _ -> Some(TInt)
  | TBool _ -> Some(TBool)
  | Var x -> (match e x with
    | UnBound -> failwith ("Variable " ^ x ^ " not found")
    | t -> Some(t)
    )
  | Plus (t1, t2) | Times (t1, t2) -> (
    match (check_type e t1, check_type e t2) with
      | (Some(TInt), Some(TInt)) -> Some(TInt)
      | (Some(v), Some(v1)) -> failwith ("Arithmetic (+/*) operation type error " ^  (type_to_string (Some(v))) ^ ", " ^ (type_to_string (Some(v1))))
      | _ -> failwith "Arithmetic operation type error"
    )
  | Minus (t1, t2) -> (
    match (check_type e t1, check_type e t2) with
      | (Some(TInt), Some(TInt)) -> Some(TInt)
      | (Some(v), Some(v1)) -> failwith ("Arithmetic (-) operation type error " ^  (type_to_string (Some(v))) ^ ", " ^ (type_to_string (Some(v1))))
      | _ -> failwith "Arithmetic operation type error"
    )
  | And (t1, t2) -> (
    match (check_type e t1, check_type e t2) with
      | (Some(TBool), Some(TBool)) -> Some(TBool)
      | _ -> failwith "And operation type error"
    )
  | Not t -> (
    match check_type e t with
      | Some(TBool) -> Some(TBool)
      | _ -> failwith "Not operation type error"
    )
  | Less (t1, t2) -> (
    match (check_type e t1, check_type e t2) with
      | (Some(TInt), Some(TInt)) -> Some(TBool)
      | _ -> failwith "Less operation type error"
    )
  | Let (x, t1, t2) -> (
    let et1 : ty option = (check_type e t1) in
    match et1 with
      | Some(v) -> check_type (bind e x v) t2
      (* | Some(TBool) -> check_type (bind e x TBool) t2
      | Some(TFunctional(ty', ty'')) -> check_type (bind e x (TFunctional(ty', ty''))) t2 *)
      | _ -> failwith "Let type error"
  )
  | IfThenElse (t1, t2, t3) -> (
    match (check_type e t1, check_type e t2, check_type e t3) with
      | (Some(TBool), t2, t3) -> if t2 = t3 then t2 else failwith "IfThenElse type error"
      | _ -> failwith "IfThenElse type error"
    )
  | LetFun (f, x, ty, t1, t2) -> (
      match ty with
        | TFunctional(ty', ty'') -> (
          let e': ty env = bind e f ty in
          match (check_type (bind e' x ty') t1) with
            | Some(t'') -> check_type e' t2
            | _ -> failwith "LetFun body type error"
          )
        | _ -> failwith "LetFun type error"
    )
  | Fun (x, ty, t) -> (
      (* Printf.printf "Fun: %s\n" x; *)
      match check_type (bind e x ty) t with
        | Some(ty'') -> Some(TFunctional(ty, ty''))
        | _ -> failwith "Function body type error"
      )
  | FunApp (t1, t2) -> (
    (* Printf.printf "ctx: %s\n" (print_ctx e); *)
    match check_type e t1 with
      | Some(TFunctional(ty, ty1)) -> (
            match check_type e t2 with 
               | Some(ty') ->( 
                  if ty = ty' then 
                    Some(ty1)
                  else 
                    failwith ("Function application argument type error Expected: " ^ (type_to_string (Some(ty))) ^ " Got: " ^ (type_to_string (Some(ty'))))
               )
              | _ -> failwith ("Function application second type error Expected: " ^ type_to_string (Some(ty)))
            )
      | Some(t') -> failwith ("Function application first type error Got: " ^ (type_to_string (Some(t'))))
      | _ -> failwith "Function application type error"
      )
  | RecFunApp (t1, t2) -> (
    match t1 with
      | Var(f) -> (
        match e f with
          | TFunctional(ty, ty1) -> (
            match check_type e t2 with
              | Some(ty') -> if ty = ty' then Some(ty1) else failwith "Recursive function application argument type error"
              | _ -> failwith "Recursive function application type error"
            )
          | UnBound -> failwith ("Recursive function application: " ^ f ^ " variable not found")
          | _ -> failwith ("Recursive function application: " ^ f ^ " not a function")
          ) 
      | _ -> failwith "Recursive function application first parameter is not a variable"
    )
;;


let rec eval (e: envT env) (t: term) : envT =
  match t with
  | TNum n -> Int n
  | TBool b -> Bool b
  | Var x -> (match e x with
    | UnBound -> failwith ("Variable " ^ x ^ " not found")
    | v -> v)
  | Fun (x, ty, t) -> Closure(x, ty ,t, e) (* TODO: Check if the body type actually is the type that is given *)
  | Let (x, t1, t2) -> eval (bind e x (eval e t1)) t2
  | LetFun (f, x, ty, t1, t2) -> eval (bind e f (RecClosure(f, x, ty, t1, e))) t2
  | FunApp (f, t) -> (
    match f with
      | Var(f) -> (
        match e f with
          | Closure (x, ty, t', e') -> eval (bind e' x (eval e t)) t'
          | _ -> failwith ("Function " ^ f ^ " variable not found"))
      | Fun (x, ty, t') -> eval (bind e x (eval e t)) t'
      | FunApp (_,_) | RecFunApp (_,_) -> let ef = eval e f in
        (match ef with
          | Closure (x, ty, t', e') -> eval (bind e' x (eval e t)) t'
          | _ -> failwith "FunApp (_,_) Not a function"
        )
      | _ -> failwith "Not a function"
      )
  | RecFunApp (f, t) -> (
    match f with
      | Var(f) -> (
        match e f with
          | RecClosure (f, x, ty, t', e') -> eval (bind (bind e' f (RecClosure(f, x, ty, t', e'))) x (eval e t)) t'
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



let run_tests () =

  (* Test 1 - Simple addition *)
  let t1 = Plus (TNum 1, TNum 2) in
  let v1 = check_type emptycxt t1 in
  Printf.printf "Test 1 - Simple addition: Expected: TInt, Result: %s\n" (type_to_string v1);

  (* Test 2 - Boolean AND operation *)
  let t2 = And (TBool true, TBool false) in
  let v2 = check_type emptycxt t2 in
  Printf.printf "Test 2 - Boolean AND operation: Expected: TBool, Result: %s\n" (type_to_string v2);

  (* Test 3 - Variable not found *)
  let t3 = Var "x" in
  (try
     let _ = check_type emptycxt t3 in
     Printf.printf "Test 3 - Variable not found: Expected: Exception, Result: No exception\n"
   with Failure msg ->
     Printf.printf "Test 3 - Variable not found: Expected: Exception, Result: %s\n" msg);

  (* Test 4 - Let binding *)
  let t4 = Let ("x", TNum 5, Plus (Var "x", TNum 3)) in
  let v4 = check_type emptycxt t4 in
  Printf.printf "Test 4 - Let binding: Expected: TInt, Result: %s\n" (type_to_string v4);

  (* Test 5 - IfThenElse with matching branches *)
  let t5 = IfThenElse (TBool true, TNum 1, TNum 2) in
  let v5 = check_type emptycxt t5 in
  Printf.printf "Test 5 - IfThenElse with matching branches: Expected: TInt, Result: %s\n" (type_to_string v5);

  (* Test 6 - IfThenElse with mismatched branches *)
  let t6 = IfThenElse (TBool true, TNum 1, TBool false) in
  (try
     let _ = check_type emptycxt t6 in
     Printf.printf "Test 6 - IfThenElse with mismatched branches: Expected: Exception, Result: No exception\n"
   with Failure msg ->
     Printf.printf "Test 6 - IfThenElse with mismatched branches: Expected: Exception, Result: %s\n" msg);

  (* Test 7 - Function definition and application *)
  let t7 = FunApp (Fun ("x", TInt, Plus (Var "x", TNum 1)), TNum 2) in
  let v7 = check_type emptycxt t7 in
  Printf.printf "Test 7 - Function definition and application: Expected: TInt, Result: %s\n" (type_to_string v7);

  (* Test 8 - Recursive function application *)
  let t8 = LetFun ("factorial", "n", TFunctional (TInt, TInt),
                   IfThenElse (Less (Var "n", TNum 2), TNum 1,
                               Times (Var "n", RecFunApp (Var "factorial", Minus (Var "n", TNum 1)))),
                   RecFunApp (Var "factorial", TNum 5)) in
  let v8 = check_type emptycxt t8 in
  Printf.printf "Test 8 - Recursive function application: Expected: TInt, Result: %s\n" (type_to_string v8);

  (* Test 9 - Arithmetic operation with type error *)
  let t9 = Plus (TNum 1, TBool true) in
  (try
     let _ = check_type emptycxt t9 in
     Printf.printf "Test 9 - Arithmetic operation with type error: Expected: Exception, Result: No exception\n"
   with Failure msg ->
     Printf.printf "Test 9 - Arithmetic operation with type error: Expected: Exception, Result: %s\n" msg);

  (* Test 10 - Complex nested Let and RecFunApp *)
  let t10 = LetFun ("fib", "n", TFunctional (TInt, TInt),
                    IfThenElse (Less (Var "n", TNum 2), Var "n",
                                Plus (RecFunApp (Var "fib", Minus (Var "n", TNum 1)),
                                      RecFunApp (Var "fib", Minus (Var "n", TNum 2)))),
                    Let ("a", RecFunApp (Var "fib", TNum 5),
                         Let ("b", RecFunApp (Var "fib", TNum 3),
                              Plus (Var "a", Var "b")))) in
  let v10 = check_type emptycxt t10 in
  Printf.printf "Test 10 - Complex nested Let and RecFunApp: Expected: TInt, Result: %s\n" (type_to_string v10);

  (* Test 11 - Nested functions *)
  let t11 = Let ("outer", Fun("x", TInt,
                    Fun ("y", TFunctional(TInt, TInt), FunApp (Var "y", Plus (Var "x", TNum 1)))),
                    Let ("result", FunApp (FunApp (Var "outer", TNum 2), Fun ("z", TInt, Minus (Var "z", TNum 3))), 
                        Var "result")) in
  let v11 = check_type emptycxt t11 in
  Printf.printf "Test 11 - Nested functions: Expected: TInt, Result: %s\n" (type_to_string v11);

  (* translate test 11 to pseudo code
  // letFun outer x -> fun y -> y (x + 1) in
  // let result = outer 2 (fun z -> z + 3) in
  // result*)

  (* Test 12 - Higher-order function *)
  let t12 = LetFun ("apply_twice", "f", TFunctional (TFunctional (TInt, TInt), TFunctional (TInt, TInt)),
                    Fun ("x", TInt, FunApp (Var "f", FunApp (Var "f", Var "x"))),
                    Let ("double", Fun ("y", TInt, Times (Var "y", TNum 2)),
                        FunApp (FunApp (Var "apply_twice", Var "double"), TNum 3))) in
  let v12 = check_type emptycxt t12 in
  Printf.printf "Test 12 - Higher-order function: Expected: TInt, Result: %s\n" (type_to_string v12);

  (* write the test 13 in pseusocode
  // letFun compose f -> fun g -> fun x -> f (g x) in
  // letFun power n -> if n < 2 then fun h -> h else fun h -> compose h (power (n - 1)) in
  // let increment = fun z -> z + 1 in
  // (power 3 increment) 0 *)

  (* Test 14 - Multiple function composition chain *)
  let t14 = Let ("f1", Fun ("x", TInt, Plus (Var "x", TNum 1)),
              Let ("f2", Fun ("x", TInt, Times (Var "x", TNum 2)),
              Let ("f3", Fun ("x", TInt, Minus (Var "x", TNum 3)),
              Let ("compose", Fun ("f", TFunctional (TInt, TInt),
                            Fun ("g", TFunctional (TInt, TInt), 
                              Fun ("x", TInt, FunApp (Var "f", FunApp (Var "g", Var "x"))))),
                FunApp (FunApp (FunApp (Var "compose", Var "f1"), 
                        FunApp (FunApp (Var "compose", Var "f2"), Var "f3")), TNum 10))))) in
  let v14 = check_type emptycxt t14 in
  Printf.printf "Test 14 - Multiple function composition chain: Expected: TInt, Result: %s\n" (type_to_string v14);

  (* Test 15 - Function that returns complex function type *)
  let t15 = Let ("make_adder", Fun ("x", TInt, 
                   Fun ("y", TInt, Fun ("z", TInt, 
                     Plus (Plus (Var "x", Var "y"), Var "z")))),
                Let ("add5", FunApp (Var "make_adder", TNum 5),
                Let ("add5_10", FunApp (Var "add5", TNum 10),
                  FunApp (Var "add5_10", TNum 15)))) in
  let v15 = check_type emptycxt t15 in
  Printf.printf "Test 15 - Function that returns complex function type: Expected: TInt, Result: %s\n" (type_to_string v15);

  (* Test 16 - Deep nested conditional expressions *)
  let t16 = IfThenElse (
              Less (TNum 5, TNum 10),
              IfThenElse (
                And (TBool true, Not (Less (TNum 20, TNum 15))),
                IfThenElse (
                  Less (Plus (TNum 5, TNum 5), TNum 15),
                  TNum 1,
                  TNum 2
                ),
                TNum 3
              ),
              TNum 4
            ) in
  let v16 = check_type emptycxt t16 in
  Printf.printf "Test 16 - Deep nested conditional expressions: Expected: TInt, Result: %s\n" (type_to_string v16);

  (* Test 17 - Self-applying function *)
  let t17 = LetFun ("self_apply", "f", TFunctional (TFunctional (TInt, TInt), TInt),
                Fun ("x", TInt, FunApp (Var "f", Var "f")),
                Let ("inc", Fun ("x", TInt, Plus (Var "x", TNum 1)),
                  FunApp (RecFunApp (Var "self_apply", Var "inc"), TNum 5))) in
  (try
     let v17 = check_type emptycxt t17 in
     Printf.printf "Test 17 - Self-applying function: Expected: Type Error or TInt, Result: %s\n" (type_to_string v17)
   with Failure msg ->
     Printf.printf "Test 17 - Self-applying function: Expected: Type Error, Result: %s\n" msg);

  (* Test 18 - Complex numeric expression *)
  let t18 = Let ("a", TNum 5,
             Let ("b", TNum 10,
             Let ("c", IfThenElse (Less (Var "a", Var "b"), 
                      Times (Var "a", Var "b"),
                      Plus (Var "a", Var "b")),
             Let ("d", Plus (Var "c", Times (Var "a", TNum 2)),
               IfThenElse (Less (Var "d", TNum 100),
                 Minus (Var "d", TNum 10),
                 Plus (Var "d", TNum 10)))))) in
  let v18 = check_type emptycxt t18 in
  Printf.printf "Test 18 - Complex numeric expression: Expected: TInt, Result: %s\n" (type_to_string v18);

  (* Test 19 - Type error in deeply nested expression *)
  let t19 = Let ("outer", Fun ("x", TInt, 
                Let ("inner", Fun ("y", TBool, 
                  IfThenElse (Var "y", Var "x", TBool true)),
                  FunApp (Var "inner", TBool true))),
                FunApp (Var "outer", TNum 42)) in
  (try
     let v19 = check_type emptycxt t19 in
     Printf.printf "Test 19 - Type error in nested expression: Expected: Type Error, Result: %s\n" (type_to_string v19)
   with Failure msg ->
     Printf.printf "Test 19 - Type error in nested expression: Expected: Type Error, Result: %s\n" msg);

  (* Test 20 - Function selector based on condition *)
  let t20 = Let ("choose_func", Fun ("cond", TBool,
                  Fun ("f", TFunctional (TInt, TInt),
                  Fun ("g", TFunctional (TInt, TInt),
                  Fun ("x", TInt,
                    IfThenElse (Var "cond",
                      FunApp (Var "f", Var "x"),
                      FunApp (Var "g", Var "x")))))),
                Let ("double", Fun ("n", TInt, Times (Var "n", TNum 2)),
                Let ("triple", Fun ("n", TInt, Times (Var "n", TNum 3)),
                  FunApp (FunApp (FunApp (FunApp (Var "choose_func", 
                    Less (TNum 5, TNum 10)), Var "double"), Var "triple"), TNum 7)))) in
  let v20 = check_type emptycxt t20 in
  Printf.printf "Test 20 - Function selector based on condition: Expected: TInt, Result: %s\n" (type_to_string v20);

  (* Test 21 - Multiple levels of nested Let expressions *)
  let t21 = Let ("a", TNum 10,
             Let ("f", Fun ("x", TInt, Times (Var "x", Var "a")),
             Let ("b", FunApp (Var "f", TNum 5),
             Let ("g", Fun ("y", TInt, Plus (Var "y", Var "b")),
             Let ("c", FunApp (Var "g", TNum 15),
               Minus (Var "c", Var "a")))))) in
  let v21 = check_type emptycxt t21 in
  Printf.printf "Test 21 - Multiple levels of nested Let expressions: Expected: TInt, Result: %s\n" (type_to_string v21);

  (* Test 22 - Complex recursive function with branching *)
  let t22 = LetFun ("complex_rec", "n", TFunctional (TInt, TInt),
                IfThenElse (Less (Var "n", TNum 0), TNum 0,
                IfThenElse (Less (Var "n", TNum 2), TNum 1,
                  IfThenElse (And (Less (TNum 1, Var "n"), Less (Var "n", TNum 5)),
                    Plus (RecFunApp (Var "complex_rec", Minus (Var "n", TNum 1)), 
                          RecFunApp (Var "complex_rec", Minus (Var "n", TNum 2))),
                    Times (RecFunApp (Var "complex_rec", Minus (Var "n", TNum 1)), 
                           RecFunApp (Var "complex_rec", Minus (Var "n", TNum 3)))))),
                RecFunApp (Var "complex_rec", TNum 6)) in
  let v22 = check_type emptycxt t22 in
  Printf.printf "Test 22 - Complex recursive function with branching: Expected: TInt, Result: %s\n" (type_to_string v22);

  (* Test 23 - Higher-order function simulation of map *)
  let t23 = LetFun ("map", "f", TFunctional (TFunctional (TInt, TInt), TFunctional (TInt, TFunctional (TInt, TInt))),
                Fun ("start", TInt, 
                Fun ("end", TInt,
                  IfThenElse (Less (Var "end", Var "start"), TNum 0,
                    Plus (FunApp (Var "f", Var "start"),
                          FunApp (FunApp (RecFunApp (Var "map", Var "f"), 
                                         Plus (Var "start", TNum 1)), Var "end"))))),
                Let ("square", Fun ("x", TInt, Times (Var "x", Var "x")),
                  FunApp (FunApp (RecFunApp (Var "map", Var "square"), TNum 1), TNum 3))) in
  let v23 = check_type emptycxt t23 in
  Printf.printf "Test 23 - Higher-order function simulation of map: Expected: TInt, Result: %s\n" (type_to_string v23);

  (* Test 24 - Pipeline of function compositions *)
  let t24 = 
    Let ("compose", Fun ("f", TFunctional (TInt, TInt), 
                    Fun ("g", TFunctional (TInt, TInt),
                      Fun ("x", TInt, FunApp (Var "f", FunApp (Var "g", Var "x"))))),
    Let ("double", Fun ("x", TInt, Times (Var "x", TNum 2)),
    Let ("square", Fun ("x", TInt, Times (Var "x", Var "x")),
    Let ("add5", Fun ("x", TInt, Plus (Var "x", TNum 5)),
    Let ("pipeline", FunApp (FunApp (Var "compose", Var "add5"), 
                       FunApp (FunApp (Var "compose", Var "double"),
                       Var "square")),
      FunApp (Var "pipeline", TNum 3)))))) in
  let v24 = check_type emptycxt t24 in
  Printf.printf "Test 24 - Pipeline of function compositions: Expected: TInt, Result: %s\n" (type_to_string v24);

  (* Test 25 - Recursive composed functions *)
  let t25 = 
    LetFun ("factorial", "n", TFunctional (TInt, TInt),
           IfThenElse (Less (Var "n", TNum 2), TNum 1, 
                     Times (Var "n", RecFunApp (Var "factorial", Minus (Var "n", TNum 1)))),
           Let ("double", Fun ("x", TInt, Times (Var "x", TNum 2)),
                LetFun ("compose_rec", "f", TFunctional (TFunctional (TInt, TInt), 
                                      TFunctional (TFunctional (TInt, TInt),
                                        TFunctional (TInt, TInt))),
                      Fun ("g", TFunctional (TInt, TInt),
                        Fun ("n", TInt,
                          IfThenElse (Less (Var "n", TNum 1),
                            FunApp (Var "f", FunApp (Var "g", TNum 1)),
                            Plus (FunApp (Var "f", FunApp (Var "g", Var "n")),
                                 FunApp (FunApp (RecFunApp (Var "compose_rec", Var "f"), Var "g"), Minus (Var "n", TNum 1))
                                )
                          ))),
                      FunApp (FunApp (RecFunApp (Var "compose_rec", Var "double"), Var "factorial"), TNum 3)))) in
  let v25 = check_type emptycxt t25 in
  Printf.printf "Test 25 - Recursive composed functions: Expected: TInt, Result: %s\n" (type_to_string v25);
;;

(* Run all tests *)
let () = run_tests ();;

