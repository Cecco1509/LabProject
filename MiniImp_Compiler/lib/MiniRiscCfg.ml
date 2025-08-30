module MiniRiscCfg = struct

  open ControlFlowGraph
  open Ast
  open MiniRiscAst

  (* Type for variable mapping to registers *)
  type variable_map = (string, int) Hashtbl.t;;

  type result = {
    translation : (string, instruction list) Hashtbl.t;
    var_map : variable_map;
    max_reg : int;
  }

  (* These variables will be modified once the translation occurs *)
  let in_var = ref "in" ;;
  let out_var = ref "out" ;;

  (* Initialize table for variables to registers mapping *)
  let init_var_set () : variable_map =
    let var_map = Hashtbl.create 10 in
    Hashtbl.add var_map !in_var 0;
    Hashtbl.add var_map !out_var 1;
    var_map
  ;;

  (*
    0 : input
    1 : output
    2 : temporary register A
    3 : temporary register B
    4, 5, ... : additional registers

  *)
  let register_count = ref 4;;

  (* Function to create a new register *)
  let new_register () : int =
    let reg = !register_count in
    register_count := !register_count + 1;
    reg
  ;;

  let compute_aexp_branch_hight (exp : aexp) : int =
    let rec eval_hight (exp: aexp) (level): int =
      match exp with
      | Num _ | Var _ -> level  (* No deeper expr *)
      | Plus (a1, a2) -> 
        let h1 = eval_hight a1 level+1 in
        let h2 = eval_hight a2 level+1 in
        if h1 > h2 then h1 else h2
      | Minus (a1, a2) -> 
        let h1 = eval_hight a1 level+1 in
        let h2 = eval_hight a2 level+1 in
        if h1 > h2 then h1 else h2
      | Times (a1, a2) -> 
        let h1 = eval_hight a1 level+1 in
        let h2 = eval_hight a2 level+1 in
        if h1 > h2 then h1 else h2
      
    in
    
    eval_hight exp 0
  ;;

  let compute_bexp_branch_hight (exp : bexp) : int =
    let rec eval_hight (exp: bexp) (level): int =
      match exp with
      | Bool _ -> level  (* No deeper expr *)
      | Not (b1) -> eval_hight b1 level+1
      | And (b1,b2) -> 
        let h1 = eval_hight b1 level+1 in
        let h2 = eval_hight b2 level+1 in
        if h1 > h2 then h1 else h2
      | Less (a1, a2) -> 
        let h1 = compute_aexp_branch_hight a1 in
        let h2 = compute_aexp_branch_hight a2 in
        if h1 > h2 then (h1 + level + 1) else (h2 + level + 1)
    in 
      eval_hight exp 0
  ;;

  let rec translate_aexp (exp : aexp) (var_map : variable_map) (dest_reg : int) : instruction list =
    match exp with
    | Num n -> [LoadI (n, dest_reg)]  (* Load immediate value into register *)
    (* Plus *)
    | Var v -> [Load (Hashtbl.find var_map v, dest_reg)]  (* Variable should be in var_map *)
    | Plus (Num n, Var v) | Plus (Var v, Num n) -> [Biop (AddI, Hashtbl.find var_map v, n, dest_reg)]  (* Add immediate value to register *)
    | Plus (Var v, Var v2) -> [Brop (Add, Hashtbl.find var_map v, Hashtbl.find var_map v2, dest_reg)]  (* Add two variables *)
    | Plus (Num n, aexp)| Plus (aexp, Num n) -> 
      (translate_aexp aexp var_map dest_reg) @ [Biop (AddI, dest_reg, n, dest_reg)]  (* Add immediate value to register *)
    | Plus (aexp, Var v) | Plus (Var v, aexp) ->
      (translate_aexp aexp var_map dest_reg) @ [Brop (Add, dest_reg, Hashtbl.find var_map v, dest_reg)]  (* Add register to variable *)
    | Plus (a1, a2) ->
      (*
        Creates two new temporary registers for the expression
      *)
      let temp_reg = new_register () in
      translate_aexp a1 var_map dest_reg @
      translate_aexp a2 var_map temp_reg @
      [Brop (Add, dest_reg, temp_reg, dest_reg)]  (* Sub two registers and store result in dest_reg *)
    (* Minus *)
    | Minus (Num n, Var v) -> 
      [LoadI (n, dest_reg)] @
      [Brop (Sub, dest_reg, Hashtbl.find var_map v, dest_reg)]  (* Subtract immediate value from variable *)
    | Minus (Var v, Num n) -> [Biop (SubI, Hashtbl.find var_map v, n, dest_reg)]  (* Subtract immediate value from variable *)
    | Minus (Var v, Var v2) -> [Brop (Sub, Hashtbl.find var_map v, Hashtbl.find var_map v2, dest_reg)]  (* Subtract two variables *)
    | Minus (aexp, Num n) ->
      (translate_aexp aexp var_map dest_reg) @ [Biop (SubI, dest_reg, n, dest_reg)]  (* Subtract immediate value from register *)
    | Minus (Var v, aexp) ->
      (translate_aexp aexp var_map dest_reg) @ [Brop (Sub, dest_reg, Hashtbl.find var_map v, dest_reg)]  (* Add register to variable *)
    | Minus (a1, a2) ->

      let temp_reg = new_register () in
      translate_aexp a1 var_map dest_reg @
      translate_aexp a2 var_map temp_reg @
      [Brop (Sub, dest_reg, temp_reg, dest_reg)]  (* Sub two registers and store result in dest_reg *)

    | Times (Num n, Var v) | Times (Var v, Num n) -> [Biop (MultI, Hashtbl.find var_map v, n, dest_reg)]  (* Multtiply immediate value with variable *)
    | Times (Var v, Var v2) -> [Brop (Mult, Hashtbl.find var_map v, Hashtbl.find var_map v2, dest_reg)]  (* Multtiply two variables *)
    | Times (Num n, aexp) | Times (aexp, Num n) ->
      (translate_aexp aexp var_map dest_reg) @ [Biop (MultI, dest_reg, n, dest_reg)]  (* Multiply immediate value with register *)
    | Times (aexp, Var v) | Times (Var v, aexp) ->
      (translate_aexp aexp var_map dest_reg) @ [Brop (Mult, dest_reg, Hashtbl.find var_map v, dest_reg)]  (* Multiply register to variable *)
    | Times (a1, a2) ->

      let temp_reg = new_register () in
      translate_aexp a1 var_map dest_reg @
      translate_aexp a2 var_map temp_reg @
      [Brop (Mult, dest_reg, temp_reg, dest_reg)]  (* Multiply two registers and store result in dest_reg *)
  ;;

  let rec translate_bexp (exp : bexp) (var_map : variable_map) (dest_reg : int) : instruction list =
    match exp with
    | Not (Bool b) -> if b then [LoadI (0, dest_reg)] else [LoadI (1, dest_reg)]
    | Not (bexp) -> 
      translate_bexp bexp var_map 2 @
      [Urop (Not, 2, dest_reg)]
    | Bool (b) -> if b then [LoadI (1, dest_reg)] else [LoadI (0, dest_reg)]
    | And (Bool b1, Bool b2) -> if b1 && b2 then [LoadI (1, dest_reg)] else [LoadI (0, dest_reg)]
    | And (Bool b1, Not ( Bool b2)) -> if b1 && not b2 then [LoadI (1, dest_reg)] else [LoadI (0, dest_reg)]
    | And (Not (Bool b1), Bool b2) -> if not b1 && b2 then [LoadI (1, dest_reg)] else [LoadI (0, dest_reg)]
    | And (Not (Bool b1), Not (Bool b2)) -> if not b1 && not b2 then [LoadI (1, dest_reg)] else [LoadI (0, dest_reg)]
    | And (Bool b, bexp) ->
      let bval = if b then 1 else 0 in
      translate_bexp bexp var_map 2 @
      [Biop (AndI, 2, bval, dest_reg)]
    | And (Not (Bool b), bexp) ->
      let bval = if b then 0 else 1 in
      translate_bexp bexp var_map 2 @
      [Biop (AndI, 2, bval, dest_reg)]
    | And (bexp, Bool b) ->
      let bval = if b then 1 else 0 in
      translate_bexp bexp var_map 2 @
      [Biop (AndI, 2, bval, dest_reg)]
    | And (bexp, Not (Bool b)) ->
      let bval = if b then 0 else 1 in
      translate_bexp bexp var_map 2 @
      [Biop (AndI, 2, bval, dest_reg)]
    | And (b1, b2) ->
      let temp_reg = new_register () in
      translate_bexp b1 var_map dest_reg @
      translate_bexp b2 var_map temp_reg @
      [Brop (And, dest_reg, temp_reg, dest_reg)]
    | Less (Var v1, Var v2) -> [Brop (Less, Hashtbl.find var_map v1, Hashtbl.find var_map v2, dest_reg)]
    | Less (aexp, Var v) ->
      translate_aexp aexp var_map 2 @ [Brop (Less, 2, Hashtbl.find var_map v, dest_reg)]
    | Less (Var v, aexp) ->
      translate_aexp aexp var_map 2 @ [Brop (Less, Hashtbl.find var_map v, 2, dest_reg)]
    | Less (a1, a2) ->
      let temp_reg = new_register () in
      translate_aexp a1 var_map dest_reg @
      translate_aexp a2 var_map temp_reg @
      [Brop (Less, dest_reg, temp_reg, dest_reg)]  (* Compare two registers and store result in dest_reg *)
  ;;

  let translate_code (var_map : variable_map) (command : ControlFlowGraph.command) : instruction list =
    match command with
    | Skip -> [Nop]
    | Assign (var, Var v) ->
      let reg = match (Hashtbl.find_opt var_map var) with
        | Some r -> r
        | None ->
          let new_reg = new_register () in
          Hashtbl.add var_map var new_reg;
          new_reg
      in
      [Urop (Copy, Hashtbl.find var_map v, reg)]  (* Load variable value into register *)
    | Assign (var, aexp) ->
      (* Assign new register if variable is not in var_map  *)
        let reg = match (Hashtbl.find_opt var_map var) with
          | Some r -> r
          | None ->
            let new_reg = new_register () in
            Hashtbl.add var_map var new_reg;
            new_reg
        in

      (* Translate the arithmetic expression to MiniRisc instructions *)
      translate_aexp aexp var_map reg

    | If (bexp) | While (bexp) -> translate_bexp bexp var_map 2

  let translate_node (var_map : variable_map) (node : ControlFlowGraph.node) : label =

    let label_id = node.id in
    let translated_code = List.map (translate_code var_map) node.command in

    (* Flatten the list of instructions *)
    let instruction_list = List.flatten translated_code in

    (* Create a label for the node *)
    Label (label_id, instruction_list)
  ;;

  (* Build MiniRisc CFG from MiniImp CFG *)
  let translate (cfg : ControlFlowGraph.cfg) (input : string) (output : string) : result =
    in_var := input;
    out_var := output;

    let var_map = init_var_set () in

    let translated_nodes : label list = List.map (translate_node var_map) cfg.nodes in

    let translation_map = Hashtbl.create (List.length translated_nodes) in
    List.iter (fun label ->
      match label with
      | Label (id, instructions_list) -> Hashtbl.add translation_map id instructions_list
    ) translated_nodes;

    {
      translation = translation_map;
      var_map = var_map;
      max_reg = !register_count - 1;  (* max_reg is the highest register number used *)
    }
  ;;


  (* Creates a string representation of the MiniRisc CFG 

     Initial node must be renamed as "main"
      
  *)
  let sprint_mini_risc_cfg (translation_list : (string * instruction list) list) : string =
    let buf = Buffer.create 1024 in
    List.iter (fun (id, instructions) -> (
      Buffer.add_string buf (Printf.sprintf "%s:\n%s\n" id (String.concat "\n" (List.map string_of_instruction instructions))))
    ) translation_list;
    Buffer.contents buf
  ;;

  let print_mini_risc_cfg (cfg : ControlFlowGraph.cfg) (translation_map : (string, instruction list) Hashtbl.t) : unit =
    Printf.printf "MiniRisc CFG:\n";
    Printf.printf "Entry: %s\n" cfg.i.id;
    Printf.printf "Final: %s\n" cfg.f.id;
    Printf.printf "Labels:\n";
    Hashtbl.iter (fun id instructions -> (
      Printf.printf "  %s:\n%s\n" id (String.concat "\n" (List.map string_of_instruction instructions)))
    ) translation_map;
    Printf.printf "Edges:\n";
    List.iter (fun ((src, dst) : (string * string)) -> Printf.printf "  %s -> %s\n" src dst) cfg.edges;
  ;;

  let compute_spill_cost (tr_result : result) (max_reg : int) : int array =
    let spill_cost = Array.make (max_reg + 1) 0 in

    Hashtbl.iter ( fun _label instructions ->
      List.iter (fun instruction ->
        match instruction with
        | LoadI (_, reg) -> 
          spill_cost.(reg) <- spill_cost.(reg) + 1
        | Biop (_, reg1, _, reg2) -> 
          spill_cost.(reg1) <- spill_cost.(reg1) + 1;
          spill_cost.(reg2) <- spill_cost.(reg2) + 1
        | Brop (_, reg1, reg2, reg3) -> 
          spill_cost.(reg1) <- spill_cost.(reg1) + 1;
          spill_cost.(reg2) <- spill_cost.(reg2) + 1;
          spill_cost.(reg3) <- spill_cost.(reg3) + 1
        | Urop (_, reg1, reg2) -> 
          spill_cost.(reg1) <- spill_cost.(reg1) + 1;
          spill_cost.(reg2) <- spill_cost.(reg2) + 1
        | _ -> ()
      ) instructions

    ) tr_result.translation;

    spill_cost
  ;;

  let handle_register_spill (reg : int) (mini_risc_translation : result) : unit =

    (*  For each lablel 
          -> if used in, insert spill code
             -> if used as an operand -> add (loadI reg r2/r3)(load r2/r3 r2/r3)
             -> if used as a destination -> change(dest as r3) add (loadI reg r2) (store r2 r3)
    *)

    (* Iter over all labels *)

    Hashtbl.iter (fun label instructions ->

      let updated_instrs =
        List.fold_right ( fun instr acc ->

          match instr with
          | LoadI (n, r) when r = reg ->
              (LoadI (n, 2) :: LoadI (r, 3) :: Store(2, 3) :: acc)

          | Brop (op, r1, r2, r3) when r1 = reg ->
              let temp_reg = if r2 = 3 then 2 else 3 in
              (LoadI (reg, temp_reg) :: Load (temp_reg, temp_reg) :: Brop (op, temp_reg, r2, r3) :: acc)
          | Brop (op, r1, r2, r3) when r2 = reg ->
              let temp_reg = if r1 = 3 then 2 else 3 in
              (LoadI (reg, temp_reg) :: Load (temp_reg, temp_reg) :: Brop (op, r1, temp_reg, r3) :: acc)
          | Brop (op, r1, r2, r3) when r3 = reg ->
              (Brop (op, r1, r2, 2) :: LoadI (r3, 3) :: Store(2, 3) :: acc)


          | Biop (op, r1, n, r3) when r1 = reg ->
              (LoadI (reg, 2) :: Load (2, 2) :: Biop (op, 2, n, r3) :: acc)
          | Biop (op, r1, n, r3) when r3 = reg ->
              (Biop (op, r1, n, 2) :: LoadI (r3, 3) :: Store(2, 3) :: acc)


          | Urop (op, r1, r2) when r1 = reg ->
              (LoadI (reg, 2) :: Load (2, 2) :: Urop (op, 2, r2) :: acc)
          | Urop (op, r1, r2) when r2 = reg ->
              (Urop (op, r1, 2) :: LoadI (r2, 3) :: Store(2, 3) :: acc)

          | _ -> instr :: acc

        ) instructions []
        
      in 
        
      Hashtbl.replace mini_risc_translation.translation label updated_instrs;

    ) mini_risc_translation.translation

  ;;

end