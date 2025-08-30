module GenerateCode = struct

  open ControlFlowGraph
  open MiniRiscAst
  open MiniRiscCfg


  (* Input node_n -> output ln

     takes the last character of the node ID and appends it to "l"
  *)
  let compute_risc_label (node_id : string) (initial_id: string) : string =
    if node_id = initial_id then "main"
    else 
      let id = String.sub node_id 5 (String.length node_id - 5) in
      "l" ^ id
  ;;

  (* Adds jumps over labels and writes the resulting code to the specified path *)
  let generate_code (cfg : ControlFlowGraph.cfg) (mini_risc_result : MiniRiscCfg.result) (path : string) =
  
    let translation = mini_risc_result.translation in

    let final_translation : (string * instruction list) list = 
      List.fold_right (fun node acc ->

      let edges : string list = ControlFlowGraph.get_out_nodes node cfg in
      let instrs = Hashtbl.find translation node.id in
      let risc_label = compute_risc_label node.id cfg.i.id in

      match edges with
      | [] -> 
        (* Last label *)
        (risc_label, instrs) :: acc

      | t_branch :: [] -> 

        let instr_jump = instrs @ [(Jump (compute_risc_label t_branch cfg.i.id))] in
        (risc_label, instr_jump) :: acc

      | t_branch :: e_branch :: [] ->

        let instr_jump = instrs @ [(CJump (2, compute_risc_label t_branch cfg.i.id, compute_risc_label e_branch cfg.i.id))] in
        (risc_label, instr_jump) :: acc

      | _ -> 
        failwith ("Error: Unexpected control flow in CFG for label " ^ node.id)

    ) cfg.nodes []

    in

    (* Load the MiniRISC code into a string *)
    let minirisc_code : string = MiniRiscCfg.sprint_mini_risc_cfg final_translation in

    (* Write the MiniRisc code into the given file *)
    let oc = open_out path in
    Printf.fprintf oc "%s" minirisc_code;
    close_out oc;

  ;;

end