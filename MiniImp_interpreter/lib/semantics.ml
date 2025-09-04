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
  | Assign of var * aexp
  | Seq of c * c
  | If of bexp * c * c
  | While of bexp * c
;;

(* Ambiente/Memoria *)
let hash_env = Hashtbl.create 16;;

(*
If var is not in the environment, it will add to it with value v.
If var is already in the enviroment, it will update the value with v.
*)
let bind (x: var) (v: int) : unit =
  Hashtbl.replace hash_env x v
;;

let lookup (x: var) : int option =
  Hashtbl.find_opt hash_env x
;;

(* Alg expressions eval function *)
let rec eval_aexp (exp: aexp) : int =
  match exp with
  | Num n -> n
  | Var x -> (match lookup x with
    | Some(v) -> v
    | None -> failwith ("Variable " ^ x ^ " not found"))
  | Plus (a1, a2) -> (eval_aexp a1) + (eval_aexp a2)
  | Minus (a1, a2) -> (eval_aexp a1) - (eval_aexp a2)
  | Times (a1, a2) -> (eval_aexp a1) * (eval_aexp a2)
;;

(* Bool expressions eval function *)
let rec eval_bexp (exp: bexp) : bool =
  match exp with
  | Bool b -> b
  | And (b1, b2) -> (eval_bexp b1) && (eval_bexp b2)
  | Not b -> not (eval_bexp b)
  | Less (a1, a2) -> (eval_aexp a1) < (eval_aexp a2)
;;

(* Command expressions eval function *)
let rec eval_command (c: c) : unit =
  match c with
  | Skip -> ()
  | Assign (var_name, value) -> bind var_name (eval_aexp value)
  | Seq (c1, c2) -> (
    eval_command c1; 
    eval_command c2
  )
  | If (b, c1, c2) -> if (eval_bexp b) then eval_command c1 else eval_command c2
  | While (b, c1) -> if (eval_bexp b) then eval_command (Seq(c1, While(b,c1))) else ()
;;


(* Root expression eval *)
let eval_prg (p: prog) (input: int) : int option =
  match p with
  | Main (x, y, c) -> 
    bind x input;
    eval_command c;
    lookup y
  ;;