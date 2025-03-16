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
;;

(* Ambiente/Memoria *)
type env = var -> int option;;

let emptyenv = function _x-> None;;

(*Binding fra una stringa x e un Num *)
let bind (s: env) (x: var) (v: int option) =
  function (i: var) -> if (i = x) then v else (s i);;


(* Alg expressions eval function *)
let rec eval_aexp (e: env) (exp: aexp) : int =
  match exp with
  | Num n -> n
  | Var x -> (match e x with
    | Some(v) -> v
    | None -> failwith ("Variable " ^ x ^ " not found"))
  | Plus (a1, a2) -> (eval_aexp e a1) + (eval_aexp e a2)
  | Minus (a1, a2) -> (eval_aexp e a1) - (eval_aexp e a2)
  | Times (a1, a2) -> (eval_aexp e a1) * (eval_aexp e a2)
;;

(* Bool expressions eval function *)
let rec eval_bexp (e: env) (exp: bexp) : bool =
  match exp with
  | Bool b -> b
  | And (b1, b2) -> (eval_bexp e b1) && (eval_bexp e b2)
  | Not b -> not (eval_bexp e b)
  | Less (a1, a2) -> (eval_aexp e a1) < (eval_aexp e a2)
;;

(* Command expressions eval function *)
let rec eval_command (e: env) (c: c) : env =
  match c with
  | Skip -> e
  | Let (var_name, value) -> bind e var_name (Some(eval_aexp e value))
  | Seq (c1, c2) -> let e1 = eval_command e c1 in eval_command e1 c2
  | If (b, c1, c2) -> if (eval_bexp e b) then eval_command e c1 else eval_command e c2
  | While (b, c1) -> if (eval_bexp e b) then eval_command e (Seq(c1, While(b,c1))) else e
;;


(* Root expression eval *)
let eval_prg (p: prog) (input: int) : int option =
  match p with
  | Main (x, y, c) -> 
    let e1 = bind emptyenv x (Some(input)) in
    let e2 = bind e1 y None in
    let result_env = eval_command e2 c in
    result_env y
  ;;


(* test 
let _ = 
  let p = Main("in", "out", Seq(Let("out", Num(1)), Let("out", Num(2)))) in
  let e1 = eval_prg p 1 in
  match e1 "out" with
  | Some value -> Printf.printf "Output: %d\n" value
  | None -> failwith "Output variable not found in the environment"
;;

(* test with a while loop *)
let _ = 
  let p = Main("in", "out", While(Less(Var("in"), Num(5)), Let("in", Plus(Var("in"), Num(1)))) ) in
  let e1 = eval_prg p 1 in
  match e1 "in" with
  | Some value -> Printf.printf "Output: %d\n" value
  | None -> failwith "Output variable not found in the environment"

(* test with some new variables *)
let _ = 
  let p = Main("in", "out", Seq(Let("x", Num(1)), Seq(Let("y", Num(2)), Let("out", Plus(Var("x"), Var("y")))))) in
  let e1 = eval_prg p 1 in
  match e1 "out" with
  | Some value -> Printf.printf "Output: %d\n" value
  | None -> failwith "Output variable not found in the environment"

(* test with a if then else *)
let _ = 
  let p = Main("in", "out", If(Less(Var("in"), Num(5)), Let("out", Num(1)), Let("out", Num(2)))) in
  let e1 = eval_prg p 1 in
  match e1 "out" with
  | Some value -> Printf.printf "Output: %d\n" value
  | None -> failwith "Output variable not found in the environment"

(* Test 1: Simple Letment *)
let _ = 
  let p = Main("in", "out", Let("out", Num(42))) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 2: Sequence of Letments *)
let _ = 
  let p = Main("in", "out", Seq(Let("out", Num(1)), Let("out", Num(2)))) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 3: Conditional statement (true case) *)
let _ = 
  let p = Main("in", "out", If(Bool(true), Let("out", Num(10)), Let("out", Num(20)))) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 4: Conditional statement (false case) *)
let _ = 
  let p = Main("in", "out", If(Bool(false), Let("out", Num(10)), Let("out", Num(20)))) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 5: While loop (sum of first 5 natural numbers) *)
let _ = 
  let p = Main("in", "out", 
    Seq(
      Let("out", Num(0)),
      Seq(
        Let("x", Num(1)),
        While(Less(Var "x", Num(6)), 
          Seq(
            Let("out", Plus(Var "out", Var "x")),
            Let("x", Plus(Var "x", Num(1)))
          )
        )
      )
    )
  ) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 6: Arithmetic expression evaluation *)
let _ = 
  let p = Main("in", "out", Let("out", Times(Plus(Num(2), Num(3)), Minus(Num(5), Num(2))))) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 7: Boolean expression evaluation (And) *)
let _ = 
  let p = Main("in", "out", If(And(Bool(true), Bool(false)), Let("out", Num(1)), Let("out", Num(0)))) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 8: Boolean expression evaluation (Not) *)
let _ = 
  let p = Main("in", "out", If(Not(Bool(false)), Let("out", Num(1)), Let("out", Num(0)))) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 9: Boolean expression evaluation (Less) *)
let _ = 
  let p = Main("in", "out", If(Less(Num(3), Num(5)), Let("out", Num(1)), Let("out", Num(0)))) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 10: Nested If statements *)
let _ = 
  let p = Main("in", "out", 
    If(Less(Num(3), Num(5)),
      If(Less(Num(2), Num(4)),
        Let("out", Num(1)),
        Let("out", Num(0))
      ),
      Let("out", Num(2))
    )
  ) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 11: Complex arithmetic and boolean expressions *)
let _ = 
  let p = Main("in", "out", 
    If(And(Less(Num(3), Num(5)), Not(Bool(false))),
      Let("out", Times(Plus(Num(2), Num(3)), Minus(Num(5), Num(2)))),
      Let("out", Num(0))
    )
  ) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 12: Multiple variables and Letments *)
let _ = 
  let p = Main("in", "out", 
    Seq(
      Let("x", Num(10)),
      Seq(
        Let("y", Num(20)),
        Let("out", Plus(Var "x", Var "y"))))
    )
   in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 13: While loop with multiple variables *)
let _ = 
  let p = Main("in", "out", 
    Seq(
      Let("out", Num(0)),
      Seq(
        Let("x", Num(1)),
        While(Less(Var "x", Num(6)), 
          Seq(
            Let("out", Plus(Var "out", Var "x")),
            Let("x", Plus(Var "x", Num(1)))
          )
        )
      )
    )
  ) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 14: Nested While loops *)
let _ = 
  let p = Main("in", "out", 
    Seq(
      Let("out", Num(0)),
      Seq(
        Let("x", Num(1)),
        While(Less(Var "x", Num(4)), 
          Seq(
            Let("y", Num(1)),
            Seq(
              While(Less(Var "y", Num(4)), 
                Seq(
                  Let("out", Plus(Var "out", Times(Var "x", Var "y"))),
                  Let("y", Plus(Var "y", Num(1)))
                )
              ),
              Let("x", Plus(Var "x", Num(1)))
            )
          )
        )
      )
    )
  ) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 15: Complex prog with multiple constructs *)
let _ = 
  let p = Main("in", "out", 
    Seq(
      Let("x", Num(10)),
      Seq(
        Let("y", Num(20)),
        If(Less(Var "x", Var "y"),
          Seq(
            Let("out", Num(1)),
            While(Less(Var "x", Var "y"),
              Seq(
                Let("x", Plus(Var "x", Num(1))),
                Let("out", Plus(Var "out", Num(1)))
              )
            )
          ),
          Let("out", Num(0))
        )
      )
    )
  ) in
  let e1 = eval_prg p 1 in
  print_var e1 "out"
;;

(* Test 16: Test with input variable *)
let _ = 
  let p = Main("in", "out", Let("out", Plus(Var "in", Num(1)))) in
  let e1 = eval_prg p 5 in
  print_var e1 "out"
;;

(* Test 17: Test with input variable and conditional *)
let _ = 
  let p = Main("in", "out", 
    If(Less(Var "in", Num(10)),
      Let("out", Num(1)),
      Let("out", Num(0))
    )
  ) in
  let e1 = eval_prg p 7 in
  print_var e1 "out"
;;

(* Test 18: Test with input variable and loop *)
let _ = 
  let p = Main("in", "out", 
    Seq(
      Let("out", Num(0)),
      While(Less(Var "out", Var "in"),
        Let("out", Plus(Var "out", Num(1)))
      )
    )
  ) in
  let e1 = eval_prg p 5 in
  print_var e1 "out"
;;

(* Test 19: Test with multiple variables and complex expressions *)
let _ = 
  let p = Main("in", "out", 
    Seq(
      Let("x", Num(2)),
      Seq(
        Let("y", Num(3)),
        Seq(
          Let("z", Times(Var "x", Var "y")),
          Let("out", Plus(Var "z", Var "in"))
        )
      )
    )
  ) in
  let e1 = eval_prg p 4 in
  print_var e1 "out"
;;

(* Test 20: Test with nested conditionals and loops *)
let _ = 
  let p = Main("in", "out", 
    Seq(
      Let("x", Num(1)),
      Seq(
        Let("out", Num(0)),
        While(Less(Var "x", Var "in"),
          Seq(
            If(Less(Var "x", Num(5)),
              Let("out", Plus(Var "out", Var "x")),
              Let("out", Plus(Var "out", Num(1)))
            ),
            Let("x", Plus(Var "x", Num(1)))
          )
        )
      )
    )
  ) in
  let e1 = eval_prg p 10 in
  print_var e1 "out"
;;*)