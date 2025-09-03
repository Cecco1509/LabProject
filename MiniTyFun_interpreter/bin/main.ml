open MiniTyFun.Ast
open MiniTyFun.TypeChecker
open MiniTyFun.Interpreter

let () = 

  (* Run test for MiniTyFun type system *)

  (* Test 1 - Simple addition *)
  let t1 = Plus (TNum 1, TNum 2) in
  let v1 = TypeChecker.check_type emptycxt t1 in
  Printf.printf "Test 1 - Simple addition: Expected: TInt, Result: %s\n" (type_to_string v1);

  (* Test 2 - Boolean AND operation *)
  let t2 = And (TBool true, TBool false) in
  let v2 = TypeChecker.check_type emptycxt t2 in
  Printf.printf "Test 2 - Boolean AND operation: Expected: TBool, Result: %s\n" (type_to_string v2);

  (* Test 3 - Variable not found *)
  let t3 = Var "x" in
  let v3 = TypeChecker.check_type emptycxt t3 in
  Printf.printf "Test 3 - Variable not found: Expected: None, Result: %s\n" (type_to_string v3);

  (* Test 4 - Let binding *)
  let t4 = Let ("x", TNum 5, Plus (Var "x", TNum 3)) in
  let v4 = TypeChecker.check_type emptycxt t4 in
  Printf.printf "Test 4 - Let binding: Expected: TInt, Result: %s\n" (type_to_string v4);

  (* Test 5 - IfThenElse with matching branches *)
  let t5 = IfThenElse (TBool true, TNum 1, TNum 2) in
  let v5 = TypeChecker.check_type emptycxt t5 in
  Printf.printf "Test 5 - IfThenElse with matching branches: Expected: TInt, Result: %s\n" (type_to_string v5);

  (* Test 6 - IfThenElse with mismatched branches *)
  let t6 = IfThenElse (TBool true, TNum 1, TBool false) in
  let v6 = TypeChecker.check_type emptycxt t6 in
  Printf.printf "Test 6 - IfThenElse with mismatched branches: Expected: Exception, Result: %s\n" (type_to_string v6);

  (* Test 7 - Function definition and application *)
  let t7 = FunApp (Fun ("x", TInt, Plus (Var "x", TNum 1)), TNum 2) in
  let v7 = TypeChecker.check_type emptycxt t7 in
  Printf.printf "Test 7 - Function definition and application: Expected: TInt, Result: %s\n" (type_to_string v7);

  (* Test 8 - Recursive function application *)
  let t8 = LetFun ("factorial", "n", TRecFunctional (TInt, TInt),
                   IfThenElse (Less (Var "n", TNum 2), TNum 1,
                               Times (Var "n", FunApp (Var "factorial", Minus (Var "n", TNum 1)))),
                   FunApp (Var "factorial", TNum 5)) in
  let v8 = TypeChecker.check_type emptycxt t8 in
  Printf.printf "Test 8 - Recursive function application: Expected: TInt, Result: %s\n" (type_to_string v8);

  (* Test 9 - Arithmetic operation with type error *)
  let t9 = Plus (TNum 1, TBool true) in
  let v9 = TypeChecker.check_type emptycxt t9 in
  Printf.printf "Test 9 - Arithmetic operation with type error: Expected: Exception, Result: %s\n" (type_to_string v9);

  (* Test 10 - Complex nested Let and FunApp *)
  let t10 = LetFun ("fib", "n", TRecFunctional (TInt, TInt),
                    IfThenElse (Less (Var "n", TNum 2), Var "n",
                                Plus (FunApp (Var "fib", Minus (Var "n", TNum 1)),
                                      FunApp (Var "fib", Minus (Var "n", TNum 2)))),
                    Let ("a", FunApp (Var "fib", TNum 5),
                         Let ("b", FunApp (Var "fib", TNum 3),
                              Plus (Var "a", Var "b")))) in
  let v10 = TypeChecker.check_type emptycxt t10 in
  Printf.printf "Test 10 - Complex nested Let and FunApp: Expected: TInt, Result: %s\n" (type_to_string v10);

  (* Test 11 - Nested functions *)
  let t11 = Let ("outer", Fun("x", TInt,
                    Fun ("y", TFunctional(TInt, TInt), FunApp (Var "y", Plus (Var "x", TNum 1)))),
                    Let ("result", FunApp (FunApp (Var "outer", TNum 2), Fun ("z", TInt, Minus (Var "z", TNum 3))), 
                        Var "result")) in
  let v11 = TypeChecker.check_type emptycxt t11 in
  Printf.printf "Test 11 - Nested functions: Expected: TInt, Result: %s\n" (type_to_string v11);

  (* Test 12 - Higher-order function *)
  let t12 = LetFun ("apply_twice", "f", TRecFunctional (TFunctional (TInt, TInt), TFunctional (TInt, TInt)),
                    Fun ("x", TInt, FunApp (Var "f", FunApp (Var "f", Var "x"))),
                    Let ("double", Fun ("y", TInt, Times (Var "y", TNum 2)),
                        FunApp (FunApp (Var "apply_twice", Var "double"), TNum 3))) in
  let v12 = TypeChecker.check_type emptycxt t12 in
  Printf.printf "Test 12 - Higher-order function: Expected: TInt, Result: %s\n" (type_to_string v12);

  (* Test 14 - Multiple function composition chain *)
  let t14 = Let ("f1", Fun ("x", TInt, Plus (Var "x", TNum 1)),
              Let ("f2", Fun ("x", TInt, Times (Var "x", TNum 2)),
              Let ("f3", Fun ("x", TInt, Minus (Var "x", TNum 3)),
              Let ("compose", Fun ("f", TFunctional (TInt, TInt),
                            Fun ("g", TFunctional (TInt, TInt), 
                              Fun ("x", TInt, FunApp (Var "f", FunApp (Var "g", Var "x"))))),
                FunApp (FunApp (FunApp (Var "compose", Var "f1"), 
                        FunApp (FunApp (Var "compose", Var "f2"), Var "f3")), TNum 10))))) in
  let v14 = TypeChecker.check_type emptycxt t14 in
  Printf.printf "Test 14 - Multiple function composition chain: Expected: TInt, Result: %s\n" (type_to_string v14);

  (* Test 15 - Function that returns complex function type *)
  let t15 = Let ("make_adder", Fun ("x", TInt, 
                   Fun ("y", TInt, Fun ("z", TInt, 
                     Plus (Plus (Var "x", Var "y"), Var "z")))),
                Let ("add5", FunApp (Var "make_adder", TNum 5),
                Let ("add5_10", FunApp (Var "add5", TNum 10),
                  FunApp (Var "add5_10", TNum 15)))) in
  let v15 = TypeChecker.check_type emptycxt t15 in
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
  let v16 = TypeChecker.check_type emptycxt t16 in
  Printf.printf "Test 16 - Deep nested conditional expressions: Expected: TInt, Result: %s\n" (type_to_string v16);

  (* REWRITE THIS TEST CASE *)
  (* Test 17 - Self-applying function *)
  let t17 = LetFun ("self_apply", "f", TRecFunctional (TFunctional (TInt, TInt), TInt),
                FunApp (Var "f", TNum 5),
                FunApp (Var "self_apply", Fun ("x", TInt, Plus (Var "x",  TNum 5)))) in
  let v17 = TypeChecker.check_type emptycxt t17 in
  Printf.printf "Test 17 - Wrong let fun type function: Expected: None, Result: %s\n" (type_to_string v17);

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
  let v18 = TypeChecker.check_type emptycxt t18 in
  Printf.printf "Test 18 - Complex numeric expression: Expected: TInt, Result: %s\n" (type_to_string v18);

  (* Test 19 - Type error in deeply nested expression *)
  let t19 = Let ("outer", Fun ("x", TInt, 
                Let ("inner", Fun ("y", TBool, 
                  IfThenElse (Var "y", Var "x", TBool true)),
                  FunApp (Var "inner", TBool true))),
                FunApp (Var "outer", TNum 42)) in
  let v19 = TypeChecker.check_type emptycxt t19 in
  Printf.printf "Test 19 - Type error in nested expression: Expected: Type Error, Result: %s\n" (type_to_string v19);

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
  let v20 = TypeChecker.check_type emptycxt t20 in
  Printf.printf "Test 20 - Function selector based on condition: Expected: TInt, Result: %s\n" (type_to_string v20);

  (* Test 21 - Multiple levels of nested Let expressions *)
  let t21 = Let ("a", TNum 10,
             Let ("f", Fun ("x", TInt, Times (Var "x", Var "a")),
             Let ("b", FunApp (Var "f", TNum 5),
             Let ("g", Fun ("y", TInt, Plus (Var "y", Var "b")),
             Let ("c", FunApp (Var "g", TNum 15),
               Minus (Var "c", Var "a")))))) in
  let v21 = TypeChecker.check_type emptycxt t21 in
  Printf.printf "Test 21 - Multiple levels of nested Let expressions: Expected: TInt, Result: %s\n" (type_to_string v21);

  (* Test 22 - Complex recursive function with branching *)
  let t22 = LetFun ("complex_rec", "n", TRecFunctional (TInt, TInt),
                IfThenElse (Less (Var "n", TNum 0), TNum 0,
                IfThenElse (Less (Var "n", TNum 2), TNum 1,
                  IfThenElse (And (Less (TNum 1, Var "n"), Less (Var "n", TNum 5)),
                    Plus (FunApp (Var "complex_rec", Minus (Var "n", TNum 1)), 
                          FunApp (Var "complex_rec", Minus (Var "n", TNum 2))),
                    Times (FunApp (Var "complex_rec", Minus (Var "n", TNum 1)), 
                           FunApp (Var "complex_rec", Minus (Var "n", TNum 3)))))),
                FunApp (Var "complex_rec", TNum 6)) in
  let v22 = TypeChecker.check_type emptycxt t22 in
  Printf.printf "Test 22 - Complex recursive function with branching: Expected: TInt, Result: %s\n" (type_to_string v22);

  (* Test 23 - Higher-order function simulation of map *)
  let t23 = LetFun ("map", "f", TRecFunctional (TFunctional (TInt, TInt), TFunctional (TInt, TFunctional (TInt, TInt))),
                Fun ("start", TInt, 
                Fun ("end", TInt,
                  IfThenElse (Less (Var "end", Var "start"), TNum 0,
                    Plus (FunApp (Var "f", Var "start"),
                          FunApp (FunApp (FunApp (Var "map", Var "f"), 
                                         Plus (Var "start", TNum 1)), Var "end"))))),
                Let ("square", Fun ("x", TInt, Times (Var "x", Var "x")),
                  FunApp (FunApp (FunApp (Var "map", Var "square"), TNum 1), TNum 3))) in
  let v23 = TypeChecker.check_type emptycxt t23 in
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
  let v24 = TypeChecker.check_type emptycxt t24 in
  Printf.printf "Test 24 - Pipeline of function compositions: Expected: TInt, Result: %s\n" (type_to_string v24);

  (* Test 25 - Recursive composed functions *)
  let t25 = 
    LetFun ("factorial", "n", TRecFunctional (TInt, TInt),
           IfThenElse (Less (Var "n", TNum 2), TNum 1, 
                     Times (Var "n", FunApp (Var "factorial", Minus (Var "n", TNum 1)))),
           Let ("double", Fun ("x", TInt, Times (Var "x", TNum 2)),
                LetFun ("compose_rec", "f", TRecFunctional (TFunctional (TInt, TInt), 
                                      TFunctional (TRecFunctional (TInt, TInt),
                                        TFunctional (TInt, TInt))),
                      Fun ("g", TRecFunctional (TInt, TInt),
                        Fun ("n", TInt,
                          IfThenElse (Less (Var "n", TNum 1),
                            FunApp (Var "f", FunApp (Var "g", TNum 1)),
                            Plus (FunApp (Var "f", FunApp (Var "g", Var "n")),
                                 FunApp (FunApp (FunApp (Var "compose_rec", Var "f"), Var "g"), Minus (Var "n", TNum 1))
                                )
                          ))),
                      FunApp (FunApp (FunApp (Var "compose_rec", Var "double"), Var "factorial"), TNum 3)))) in
  let v25 = TypeChecker.check_type emptycxt t25 in
  Printf.printf "Test 25 - Recursive composed functions: Expected: TInt, Result: %s\n" (type_to_string v25);

  (* Test 26 - simple function *)
  let t26 = 
    Fun ("n", TInt, Plus (Var "n", TNum 1))
  in 
  let v26 = TypeChecker.check_type emptycxt t26 in
  Printf.printf "Test 26 - Returns a function: Expected: TInt -> TInt, Result: %s\n" (type_to_string v26);

  Printf.printf "\n********************* Running tests for eval function ************************\n";

  let r1 = Interpreter.eval emptyenv emptycxt t1 in
  Printf.printf "Test 1 - Result: %s\n" (value_to_string r1);

  let r2 = Interpreter.eval emptyenv emptycxt t2 in
  Printf.printf "Test 2 - Result: %s\n" (value_to_string r2);

  (try
    let _ = Interpreter.eval emptyenv emptycxt t3 in
    Printf.printf "Test 3 - Variable not found: Expected: Exception, Result: No exception\n"
   with Failure msg ->
    Printf.printf "Test 3 - Variable not found: Expected: Exception, Result: %s\n" msg);

  let r4 = Interpreter.eval emptyenv emptycxt t4 in
  Printf.printf "Test 4 - Result: %s\n" (value_to_string r4);

  let r5 = Interpreter.eval emptyenv emptycxt t5 in
  Printf.printf "Test 5 - Result: %s\n" (value_to_string r5);

  (try
    let _ = Interpreter.eval emptyenv emptycxt t6 in
    Printf.printf "Test 6 - IfThenElse with mismatched branches: Expected: Exception, Result: No exception\n"
   with Failure msg ->
    Printf.printf "Test 6 - IfThenElse with mismatched branches: Expected: Exception, Result: %s\n" msg);

  let r7 = Interpreter.eval emptyenv emptycxt t7 in
  Printf.printf "Test 7 - Result: %s\n" (value_to_string r7);

  let r8 = Interpreter.eval emptyenv emptycxt t8 in
  Printf.printf "Test 8 - Result: %s\n" (value_to_string r8);

  (try
    let _ = Interpreter.eval emptyenv emptycxt t9 in
    Printf.printf "Test 9 - Arithmetic operation with type error: Expected: Exception, Result: No exception\n"
   with Failure msg ->
    Printf.printf "Test 9 - Arithmetic operation with type error: Expected: Exception, Result: %s\n" msg);

  let r10 = Interpreter.eval emptyenv emptycxt t10 in
  Printf.printf "Test 10 - Result: %s\n" (value_to_string r10);

  let r11 = Interpreter.eval emptyenv emptycxt t11 in
  Printf.printf "Test 11 - Result: %s\n" (value_to_string r11);

  let r12 = Interpreter.eval emptyenv emptycxt t12 in
  Printf.printf "Test 12 - Result: %s\n" (value_to_string r12);

  let r14 = Interpreter.eval emptyenv emptycxt t14 in
  Printf.printf "Test 14 - Result: %s\n" (value_to_string r14);

  let r15 = Interpreter.eval emptyenv emptycxt t15 in
  Printf.printf "Test 15 - Result: %s\n" (value_to_string r15);

  let r16 = Interpreter.eval emptyenv emptycxt t16 in
  Printf.printf "Test 16 - Result: %s\n" (value_to_string r16);

  (try
    let r17 = Interpreter.eval emptyenv emptycxt t17 in
    Printf.printf "Test 17 - Self-applying function: Expected: Type Error or TInt, Result: %s\n" (value_to_string r17)
   with Failure msg ->
    Printf.printf "Test 17 - Self-applying function: Expected: Type Error, Result: %s\n" msg);

  let r18 = Interpreter.eval emptyenv emptycxt t18 in
  Printf.printf "Test 18 - Result: %s\n" (value_to_string r18);

  (try
    let r19 = Interpreter.eval emptyenv emptycxt t19 in
    Printf.printf "Test 19 - Type error in nested expression: Expected: Type Error, Result: %s\n" (value_to_string r19)
   with Failure msg ->
    Printf.printf "Test 19 - Type error in nested expression: Expected: Type Error, Result: %s\n" msg);

  let r20 = Interpreter.eval emptyenv emptycxt t20 in
  Printf.printf "Test 20 - Result: %s\n" (value_to_string r20);

  let r21 = Interpreter.eval emptyenv emptycxt t21 in
  Printf.printf "Test 21 - Result: %s\n" (value_to_string r21);

  let r22 = Interpreter.eval emptyenv emptycxt t22 in
  Printf.printf "Test 22 - Result: %s\n" (value_to_string r22);

  let r23 = Interpreter.eval emptyenv emptycxt t23 in
  Printf.printf "Test 23 - Result: %s\n" (value_to_string r23);

  let r24 = Interpreter.eval emptyenv emptycxt t24 in
  Printf.printf "Test 24 - Result: %s\n" (value_to_string r24);

  let r25 = Interpreter.eval emptyenv emptycxt t25 in
  Printf.printf "Test 25 - Result: %s\n" (value_to_string r25);

  let r26 = Interpreter.eval emptyenv emptycxt t26 in
  Printf.printf "Test 26 - Result: %s\n" (value_to_string r26);

;;



