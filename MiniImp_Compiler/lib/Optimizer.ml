module Optimizer = struct

  open ControlFlowGraph
  open MiniRiscCfg
  open Liveness

  (* Graph as adjacency list, where for each register we keep track of its interfering registers *)
  type interference_graph = (int, Liveness.RegisterSet.t) Hashtbl.t

  (** Adds an undirected edge r1--r2 (avoids self-loop) *)
  let add_edge (g : interference_graph) (r1 : int) (r2 : int) : unit =
    if r1 <> r2 then begin
      let adj1 = Hashtbl.find_opt g r1 |> Option.value ~default:Liveness.RegisterSet.empty in
      let adj2 = Hashtbl.find_opt g r2 |> Option.value ~default:Liveness.RegisterSet.empty in
      Hashtbl.replace g r1 (Liveness.RegisterSet.add r2 adj1);
      Hashtbl.replace g r2 (Liveness.RegisterSet.add r1 adj2)
    end
  ;;

  (** Builds the interference graph from the liveness state. **********************************
      block_state = (in, used, def, out)
      Rule:
        - Every defined register (def) interferes with all live-out (out)
  *)
  let build_interference_graph (live_state : Liveness.liveness_result) (cfg : ControlFlowGraph.cfg) (mini_risc_result : MiniRiscCfg.result) : interference_graph =
    let g = Hashtbl.create 16 in

    let mini_risc_translation = mini_risc_result.translation in

    List.iter (fun (node: ControlFlowGraph.node) ->

      let mini_risc_code = Hashtbl.find mini_risc_translation (node.id) in

      (* In case of a block with multiple instructions *)
      if List.length mini_risc_code > 1 then (
        List.iteri (fun i _ ->
          (* For each instruction, add edges between defined and live-out registers *)
          match Hashtbl.find_opt live_state.instr_level_analysis (node.id, i) with
          | None -> ()
          | Some (_, _, def_s, out_s) ->
            Liveness.RegisterSet.iter (fun d ->
              Liveness.RegisterSet.iter (fun o -> 
                if d <> o then add_edge g d o
              ) out_s
            ) def_s
        ) mini_risc_code;

      ) else (
        match Hashtbl.find_opt live_state.block_analysis node.id with
        | None -> ()
        | Some (_, _, def_s, out_s) ->
          Liveness.RegisterSet.iter (fun d ->
            Liveness.RegisterSet.iter (fun o ->
              if d <> o then add_edge g d o
            ) out_s
          ) def_s;
      )
    ) cfg.nodes;
    g
  ;;

  (*********************** Print the interference graph ***********************)
  let print_interference_graph (g : interference_graph) : unit =
    print_endline "Interference graph:";
    Hashtbl.iter (fun r adj ->
      let neighbors =
        Liveness.RegisterSet.elements adj
        |> List.map (fun x -> Printf.sprintf "r%d" x)
        |> String.concat ", " in
      Printf.printf "r%d: { %s }\n" r neighbors
    ) g
  ;;

end