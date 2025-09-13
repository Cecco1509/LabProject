module RegisterAllocation = struct

open ControlFlowGraph
open MiniRiscAst
open MiniRiscCfg
open Liveness
open Optimizer

(*
 0) initialize S <- empty set, R <- empty stack.
    - S represents the set of spill registers
    - R will be used in the allocation algorithm
 1) Construct interference graph
    S' <- S
 2) Simplify by graph coloring (deconstruct graph)
 3) Assign colors to registers
 4) repeat from 1 if S' != S

*)
                  (*  reg         edges              color/spill   *)
type stack_register = int * Liveness.RegisterSet.t * bool

(* Maximum physical registers -> updated by caller*)
let k = ref 4;;

(*************** Helper function that creates a degree map from the interference graph ***************)
(*
  example:
    reg0 -> 4
    reg1 -> 2
    reg2 -> 3
    reg3 -> 6
*)
let create_degree_map (igraph : Optimizer.interference_graph) : (int, int) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  Hashtbl.iter (fun reg regs_set ->
    Hashtbl.add tbl reg (Liveness.RegisterSet.cardinal regs_set)
  ) igraph;
  tbl
;;

(***** Check if a register is a default register ******)
(* I've putted this here in case of future updates *)
let is_reg_default (reg : int) : bool =
  reg < 4


(******************** Find a register with degree less than k ********************)
(*
  k -> max degree
  degree_map -> map of degrees for each register
  spill_cost -> array of spill costs for each register
                  spill_cost[i] is the spill cost for register i
*)
let find_register (degree_map : (int, int) Hashtbl.t) (spill_cost : int array) : int * bool =

  let degree_list = Hashtbl.fold (fun k v acc -> (k,v)::acc) degree_map [] in

  (* Sort registers by degree and spill cost, register with higher spill cost comes after *)
  (* This ensures that the registers with higher spill costs are considered earlier by the next phase *)
  let sorted = List.sort (fun (reg1, d1) (reg2, d2) -> compare (spill_cost.(reg1)/(d1+1)) (spill_cost.(reg2)/(d2+1))) degree_list in

  (* Find the first register with degree less than k *)
  let r = List.find_opt (fun (_reg, degree) -> degree < !k) sorted in

  
  match r with
  | Some (reg, _) -> (reg, true)              (* if a register is found that return it *)
  | None -> (fst (List.hd sorted), false)     (* in case there is no register with degree < k, return the one with lower spill cost *)

;;

(************************** Function that simplifies the interference graph **************************)
let simplify_graph (igraph : Optimizer.interference_graph) (degree_map : (int, int) Hashtbl.t) (mini_risc_result : MiniRiscCfg.result) : (stack_register) Stack.t =

  let stack = Stack.create () in

  let special_registers = List.fold_left (fun acc reg ->
    let neighbors = Hashtbl.find_opt igraph reg |> Option.value ~default:Liveness.RegisterSet.empty in
    Hashtbl.replace igraph reg Liveness.RegisterSet.empty;
    Hashtbl.remove degree_map reg;
    List.iter (fun neighbor  ->
        (* adjust degree_map *)
        Hashtbl.replace degree_map neighbor (Hashtbl.find degree_map neighbor - 1);
        (* remove edge from igraph *)
        Hashtbl.replace igraph neighbor (Liveness.RegisterSet.remove reg (Hashtbl.find igraph neighbor));
        (* adjust edges in igraph*)
      ) (Liveness.RegisterSet.elements neighbors);

    acc @ [neighbors]
  ) [] [3; 2; 1; 0] in

  (* Find max reg inside igraph *)
  let max_reg = List.fold_left (fun acc reg ->
    if reg > acc then reg else acc
  ) 0 (List.of_seq (Hashtbl.to_seq_keys igraph)) in

  (* Retrieve spill cost *)
  (* This was the problematic line, causing an "index out of bounds" exception
  let spill_cost = MiniRiscCfg.compute_spill_cost mini_risc_result (Hashtbl.length igraph) in *)
  let spill_cost = MiniRiscCfg.compute_spill_cost mini_risc_result max_reg in

  (* Process the degree map *)
  while Hashtbl.length degree_map > 0 do

    match find_register degree_map spill_cost with
    | (reg, true) -> (* Color case *)(
      let neighbors : Liveness.RegisterSet.t = Hashtbl.find igraph reg in
      if not (is_reg_default reg) then begin
        Stack.push (reg, neighbors, true) stack;
        Hashtbl.replace igraph reg Liveness.RegisterSet.empty
      end;
      
      Hashtbl.remove degree_map reg;
      (* Update neighbors *)
      List.iter (fun neighbor  ->
        (* adjust degree_map *)
        Hashtbl.replace degree_map neighbor (Hashtbl.find degree_map neighbor - 1);
        (* remove edge from igraph *)
        Hashtbl.replace igraph neighbor (Liveness.RegisterSet.remove reg (Hashtbl.find igraph neighbor));
        (* adjust edges in igraph*)
      ) (Liveness.RegisterSet.elements neighbors);
    )
    | (reg, false) -> (
      let neighbors : Liveness.RegisterSet.t = Hashtbl.find igraph reg in
      if not (is_reg_default reg) then begin
        Stack.push (reg, neighbors, false) stack;
        Hashtbl.replace igraph reg Liveness.RegisterSet.empty
      end;
      Hashtbl.remove degree_map reg;
      (* Update neighbors *)
      List.iter (fun neighbor  ->
        (* adjust degree_map *)
        Hashtbl.replace degree_map neighbor (Hashtbl.find degree_map neighbor - 1);
        (* remove edge from igraph *)
        Hashtbl.replace igraph neighbor (Liveness.RegisterSet.remove reg (Hashtbl.find igraph neighbor));
        (* adjust edges in igraph*)
      ) (Liveness.RegisterSet.elements neighbors);
    )
  done;

  (* 
    Push special registers into the stack 

    This way they will be colored first,
    which is a must to ensure that special registers are assigned the correct physical registers.

  *)
  List.iteri (fun i neighbors ->
    Stack.push (3-i, neighbors, true) stack
  ) special_registers;

  stack
;;

(*  Search if neighbor is colored and assign the minimal color among the unused colors *)
let get_color (neighbors : Liveness.RegisterSet.t) (color_map : (int, int) Hashtbl.t) : int option =

  (* Create a list of colors used by neighbors *)
  let colors_used_by_neighbours : int list = 
    List.filter_map (fun n -> Hashtbl.find_opt color_map n) (Liveness.RegisterSet.elements neighbors) in

  (* Sort the colors used by neighbors *)
  let colors_used_sorted = List.sort compare colors_used_by_neighbours in

  (* No colors available, return None *)
  if List.length colors_used_by_neighbours = !k 
    then None
  else 
    (* At least one color is available, search for a color between 0 and max *)
    let color = List.fold_left (fun acc c -> if acc = c then c + 1 else acc) 0 colors_used_sorted in
    if color >= !k 
      then None            (* Spill case *)
    else 
      Some color           (* Register found *)
;;

(******************** Function that colors the registers in the stack ********************)
let color_nodes (stack : (stack_register) Stack.t) (igraph : Optimizer.interference_graph) : ((int, int) Hashtbl.t * Liveness.RegisterSet.t) =
  let color_map = Hashtbl.create 16 in
  let spilled = ref Liveness.RegisterSet.empty in
  
  while not (Stack.is_empty stack) do
    let (reg, neighbors, color) = Stack.pop stack in

    (* re-add the neighbors to the graph edges *)
    List.iter (fun neighbor ->
      Hashtbl.replace igraph neighbor (Liveness.RegisterSet.add reg (Hashtbl.find igraph neighbor));
    ) (Liveness.RegisterSet.elements neighbors);

    Hashtbl.replace igraph reg (Liveness.RegisterSet.union neighbors (Hashtbl.find igraph reg));

    if color then begin
      (* Assign a color to the register *)
      let s_color = ( if reg < 4 then
                      Some(reg)
                    else
                      get_color (Hashtbl.find igraph reg) color_map
      ) in
      (match s_color with
      | Some v -> Hashtbl.add color_map reg v
      | None -> ( 
          Hashtbl.add color_map reg (-1);
          spilled := Liveness.RegisterSet.add reg !spilled
      )
      );
    end 
    else begin
      (* Spill case, just push back to the stack  NO *)
      let s_color = ( if reg < 4 then
                      Some(reg)
                    else
                      get_color (Hashtbl.find igraph reg) color_map
      ) in
      (match s_color with
      | Some v -> Hashtbl.add color_map reg v
      | None -> (
        Hashtbl.add color_map reg (-1);
        spilled := Liveness.RegisterSet.add reg !spilled
      )
      )
    end
  done;
  (color_map, !spilled)
;;

(*************** Substitute virtual registers with physical registers in the final MiniRISC representation ***************)
let assign_registers (mini_risc_translation : MiniRiscCfg.result) (color_map : (int, int) Hashtbl.t) : unit = 

  Hashtbl.iter (fun label instrs ->

    let updated_instrs = 
      List.fold_right ( fun instr acc ->
        match instr with
        | Brop (op, r1, r2, rd) -> (
          (* If rd is not found in the color map, means it was not in the interference graph 
             -> Not inferencing with anything means it's never used in the program*)
          let dest = Hashtbl.find_opt color_map rd in
          match dest with
          | Some d -> Brop (op, Hashtbl.find color_map r1, Hashtbl.find color_map r2, d) :: acc
          | None -> acc
        )
        | Biop (op, r1, n, rd) -> (
          let dest = Hashtbl.find_opt color_map rd in
          match dest with
          | Some d -> Biop (op, Hashtbl.find color_map r1, n, d) :: acc
          | None -> acc
        )
        (* If the copy instr is performed in the same register, skip the instruction *)
        | Urop (_, r1, rd) when r1 = rd -> acc

        | Urop (op, r1, rd) -> (
          let dest = Hashtbl.find_opt color_map rd in
          match dest with
          | Some d -> Urop (op, Hashtbl.find color_map r1, d) :: acc
          | None -> acc
        )
        | LoadI (value, reg) -> (
          let dest = Hashtbl.find_opt color_map reg in
          match dest with
          | Some d -> LoadI (value, d) :: acc
          | None -> acc
        )
        | _ -> instr :: acc

      ) instrs []

    in

    Hashtbl.replace mini_risc_translation.translation label updated_instrs

  ) mini_risc_translation.translation;

;;

(************** Create a list of integers from a to b **************)
let rec range a b =
  if a > b then []
  else a :: range (a+1) b
;;

(*** Handle spilling for registers that are not colored ***)
let handle_spilling_register (max_regs : int) (mini_risc_result : MiniRiscCfg.result) : unit =
  (* Handle spilling for the given register *)
  List.iter (fun reg ->
    MiniRiscCfg.handle_register_spill reg mini_risc_result
  ) (range (max_regs) mini_risc_result.max_reg);
;;

(********************************* Allocator function ***************************************)
(* 
  Starting point for virtual register to physical register mapping.
  Takes the MiniImpCFG, the maximum number of registers, and the MiniRiscCFG.

  It performs register allocation following a simplified Chaitin-Briggs allocator.
    - The SSA form is not used here, this can lead to more spills in some cases.
*)
let allocate_registers (cfg : ControlFlowGraph.cfg) (max_regs : int) (mini_risc_result : MiniRiscCfg.result) : unit =

  (* Spill registers *)
  let s = ref Liveness.RegisterSet.empty in
  let s' = ref (Liveness.RegisterSet.singleton 0) in
  let final_color_map : (int, int) Hashtbl.t ref = ref (Obj.magic ()) in

  k := max_regs;

  (* While the spilled register of previous iteration is not equal to the current one restart the allocation *)
  while !s <> !s' do

    s' := !s;

    (* Step 1: Build Interference Graph *)
    let liveness_analysis = Liveness.compute_vars_liveness cfg mini_risc_result in

    (* Perform liveness analysis *)
    let interference_graph = Optimizer.build_interference_graph liveness_analysis cfg mini_risc_result in

    (* Assign degrees to each register *)
    let degree_map = create_degree_map interference_graph  in
    
    (* Simplify graph *)
    let r = simplify_graph interference_graph degree_map mini_risc_result in
    
    (* Try finding a feasible graph coloring for the registers *)
    let color_map, spilled = color_nodes r interference_graph in
    
    (* add new spilled registers to the set of spilled registers *)
    s := Liveness.RegisterSet.union spilled !s';
    
    (* For each spilled reg, handle with temp registers *)
    Liveness.RegisterSet.iter (fun reg ->
      MiniRiscCfg.handle_register_spill reg mini_risc_result
    ) (Liveness.RegisterSet.diff spilled !s');

    final_color_map := color_map;

  done;

  (* substitute virtual registers with physical registers in the final MiniRISC representation *)
  assign_registers mini_risc_result !final_color_map;
;;    
end