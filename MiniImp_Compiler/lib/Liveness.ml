module Liveness = struct

  open ControlFlowGraph
  open MiniRiscAst
  open MiniRiscCfg

  (* Type for variable mapping to registers *)
  module RegisterSet = Set.Make(Int)

  (* In and Out registers *)
  let in_var = 0 ;;
  let out_var = 1 ;;
                     (*    in              used         defined     out *)
  type block_state = RegisterSet.t * RegisterSet.t * RegisterSet.t * RegisterSet.t;;

  type liveness_result = {
    block_analysis : (string, block_state) Hashtbl.t;
    instr_level_analysis : (string * int, block_state) Hashtbl.t;
  }

  (* Compute the used & defined variables in an MiniRisc instruction *)
  let compute_instr_used_vars (instr : MiniRiscAst.instruction) (def_vars : RegisterSet.t) (used_vars : RegisterSet.t) : (RegisterSet.t * RegisterSet.t) =
    match instr with
      | Load (src1, src2) -> let used = if RegisterSet.mem src1 def_vars then used_vars else RegisterSet.add src1 used_vars in
                             let def = RegisterSet.add src2 def_vars in
                             (used, def)

      | LoadI ( _, src2) -> (used_vars, RegisterSet.add src2 def_vars)

      | Biop (_, src1, _, src3) -> let used = if RegisterSet.mem src1 def_vars then used_vars else RegisterSet.add src1 used_vars in
                                   let def = RegisterSet.add src3 def_vars in
                                   (used, def)

      | Brop (_, src1, src2, src3) -> let set1 = if RegisterSet.mem src1 def_vars then used_vars else RegisterSet.add src1 used_vars in
                                      let used = if RegisterSet.mem src2 def_vars then set1 else RegisterSet.add src2 set1 in
                                      let def = RegisterSet.add src3 def_vars in
                                      (used, def)

      | Urop (_, src1, src2) -> let used = if RegisterSet.mem src1 def_vars then used_vars else RegisterSet.add src1 used_vars in
                                let def = RegisterSet.add src2 def_vars in
                                (used, def)
      | _ -> (RegisterSet.empty, RegisterSet.empty)

  (* Compute the used & defined variables in a node *)
  let compute_node_used_vars (label : MiniRiscAst.instruction list) : (RegisterSet.t * RegisterSet.t) =

    List.fold_left (
      fun acc (instr : MiniRiscAst.instruction) ->
        let (used_vars, def_vars) = acc in
        let (used', def') = compute_instr_used_vars instr def_vars used_vars in
        (RegisterSet.union used_vars used', RegisterSet.union def_vars def')
    ) (RegisterSet.empty, RegisterSet.empty) label
  ;;

  let _is_temp_registers (reg : int) : bool = reg == 2 || reg == 3 

  (************************ Initialize the state for a node in the CFG ************************)
  (* Returns a block_state containing:
   * - used variables
   * - defined variables
   * - liveness_in  (initially empty)
   * - liveness_out (initially empty)
   *)
  let start_state (is_initial : bool) (label : MiniRiscAst.instruction list) : block_state =
    let (used_vars, def_vars) = compute_node_used_vars label in
    let final_def_vars = if is_initial then RegisterSet.add 0 def_vars else def_vars in
    (RegisterSet.empty, used_vars, final_def_vars, RegisterSet.empty)
  ;;

  (************************ Initialize the state for each node in the CFG **********************)
  let init_state (cfg : ControlFlowGraph.cfg) (mini_risc_tr : MiniRiscCfg.result) : (string, block_state) Hashtbl.t =
    let state = Hashtbl.create (List.length cfg.nodes) in
    List.iter (
      fun (node: ControlFlowGraph.node) ->
        let node_start_state = start_state (node.id == cfg.i.id) (Hashtbl.find mini_risc_tr.translation node.id) in
        Hashtbl.add state node.id node_start_state
    ) cfg.nodes;
    state
  ;;


  (* {r used in L}∪(lvout (L)\{r defined in L}) *)
  let compute_incoming_vars (node : ControlFlowGraph.node) ( state : (string, block_state) Hashtbl.t ) (_cfg : ControlFlowGraph.cfg) : RegisterSet.t =

    let (_, used_vars, defined_vars, out_vars) = Hashtbl.find state node.id in

    RegisterSet.union used_vars (RegisterSet.diff out_vars defined_vars)
  ;;


  (* ⋃(L,L′)∈CFG edges dvin(L′) *)
  let compute_outgoing_vars (node : ControlFlowGraph.node) ( state : (string, block_state) Hashtbl.t ) (cfg : ControlFlowGraph.cfg) : RegisterSet.t =
    if node.id = cfg.f.id then
      RegisterSet.singleton out_var
    
    else
      let outgoing_nodes = ControlFlowGraph.get_out_nodes node cfg in
      List.fold_left (fun acc (n: string) ->
        let (in_vars, _, _, _) = Hashtbl.find state n in
        RegisterSet.union acc in_vars
      ) RegisterSet.empty outgoing_nodes
  ;;

  (* Print the current state for debugging purposes *)
  let print_current_state (global_state : (string, block_state) Hashtbl.t) : unit =
    Hashtbl.iter (fun block_id (in_vars, used_vars, def_vars, out_vars) ->
      Printf.printf "Block %s:\n" block_id;
      Printf.printf "  In: %s\n" ("{" ^ (String.concat ", " (List.map string_of_int (RegisterSet.elements in_vars))) ^ "}");
      Printf.printf "  Used: %s\n" ("{" ^ (String.concat ", " (List.map string_of_int (RegisterSet.elements used_vars))) ^ "}");
      Printf.printf "  Def: %s\n" ("{" ^ (String.concat ", " (List.map string_of_int (RegisterSet.elements def_vars))) ^ "}");
      Printf.printf "  Out: %s\n" ("{" ^ (String.concat ", " (List.map string_of_int (RegisterSet.elements out_vars))) ^ "}");
    ) global_state;
  ;;


  (* Find the fixpoint of the liveness analysis *)
  let rec find_fixpoint (g_state : (string, block_state) Hashtbl.t) (cfg : ControlFlowGraph.cfg) : unit =
    let update = ref false
    in
    (* Iterate over each node in the CFG *)
    List.iter (fun (node: ControlFlowGraph.node) ->
      let res = Hashtbl.find_opt g_state node.id in
      match res with
      | None -> failwith ("Node " ^ node.id ^ " not found in global state")
      | Some (in_vars, used_vars, def_vars, out_vars) -> (
        let new_out_vars : RegisterSet.t = compute_outgoing_vars node g_state cfg in
        let new_in_vars : RegisterSet.t = compute_incoming_vars node g_state cfg in

        if not (RegisterSet.equal in_vars new_in_vars) || not (RegisterSet.equal out_vars new_out_vars) then (
          Hashtbl.replace g_state node.id (new_in_vars, used_vars, def_vars, new_out_vars);
          update := true
        )
      )
    ) (List.rev cfg.nodes);
    (* Done backward for performance reasons *)

    if !update then find_fixpoint g_state cfg
    else ()
  ;;

  (* Analyze the defined variables in the CFG *)
  let compute_vars_liveness (cfg : ControlFlowGraph.cfg) (mini_risc_tr : MiniRiscCfg.result)  : liveness_result =

    (* Init global state *)
    let global_state = init_state cfg mini_risc_tr in

    (* Find the minimal fixpoint *)
    find_fixpoint global_state cfg;

    (* Compute liveness at instruction level for maximal blocks *)
    let instruction_level_liveness = Hashtbl.create (List.length cfg.nodes) in
    List.iter (fun (node: ControlFlowGraph.node) ->
      let (_, _, _, out_vars) = Hashtbl.find global_state (node.id) in

      (* If block contains more than one instruction compute the liveness at instruction level *)
      if List.length (Hashtbl.find mini_risc_tr.translation node.id) > 1 then begin

        let idx = ref (List.length (Hashtbl.find mini_risc_tr.translation node.id) - 1) in

        (* Here begins the Liveness Analysis at instruction level
          WHY is it needed? -> Needed for a better contruction of the Interference Graph

          WHY it is done here? -> Because this computation doesn't affect the bigger block state
                                  so it is useless to do for each iteration of the fixpoint

          HOW it is done? -> Starting from the last block instruction, the live_in is computed by
                             using the live_out of the previous instruction (or the block one for the last instruction)
                             and the used/defined registers in the current instruction

          Further implementation notes:
            - This computation is done twice, ones at the init phase and once here after the fixpoint
            - At init phase we care only about the maximal block liveness
        *)
        ignore (
          List.fold_left (fun acc instr ->
            (* Compute the registers used and the defined one in the current instruction *)
            let (used, def) = compute_instr_used_vars instr RegisterSet.empty RegisterSet.empty in

            let live_in = RegisterSet.union used (RegisterSet.diff acc def) in

            Hashtbl.add instruction_level_liveness (node.id, !idx) (live_in, used, def, acc);

            decr idx;
            live_in
          ) out_vars (List.rev (Hashtbl.find mini_risc_tr.translation node.id));
        )
      end
    ) cfg.nodes;

    {
      block_analysis = global_state; 
      instr_level_analysis = instruction_level_liveness
    }
  ;;
end