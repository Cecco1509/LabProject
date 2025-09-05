
type program = Main of label * label list

and label = Label of string * instruction list

and brop = Add | Sub | Mult | And | Less

and biop = AddI | SubI | MultI | AndI

and urop = Not | Copy

and instruction = 
  | Nop
  | Brop of brop * int * int * int
  | Biop of biop * int * int * int
  | Urop of urop * int * int
  | Load of int * int
  | LoadI of int * int
  | Store of int * int
  | Jump of string
  | CJump of int * string * string
;;

let string_of_brop (op : brop) : string =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mult -> "mult"
  | And -> "and"
  | Less -> "less"
;;

let string_of_biop (op : biop) : string =
  match op with
  | AddI -> "addi"
  | SubI -> "subi"
  | MultI -> "muli"
  | AndI -> "andi"
;;

let string_of_urop (op : urop) : string =
  match op with
  | Not -> "not"
  | Copy -> "copy"
;;

let string_of_label (label : label) : string =
  match label with
  | Label (name, _) -> name
;;

let string_of_instruction (instr : instruction) : string =
  match instr with
  | Nop -> "    nop"
  | Brop (op, r1, r2, rd) -> Printf.sprintf "    %s r%d r%d => r%d" (string_of_brop op) r1 r2 rd
  | Biop (op, r1, n, rd) -> Printf.sprintf "    %s r%d %d => r%d" (string_of_biop op) r1 n rd
  | Urop (op, r1, rd) -> Printf.sprintf "    %s r%d => r%d" (string_of_urop op) r1 rd
  | Load (addr, reg) -> Printf.sprintf "    load r%d => r%d" addr reg
  | LoadI (value, reg) -> Printf.sprintf "    loadi %d => r%d" value reg
  | Store (reg, addr) -> Printf.sprintf "    store r%d => r%d" reg addr
  | Jump label -> Printf.sprintf "    jump %s" label
  | CJump (reg, labelT, labelF) -> Printf.sprintf "    cjump r%d %s %s" reg labelT labelF
;;