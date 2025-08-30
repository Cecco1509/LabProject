module DefinedVars = struct

  open ControlFlowGraph
  open Ast

  (* Sets for holding variable names *)
  module VariableSet = Set.Make(String)

  (* HashMap for mapping defined variables (name, line of definition inside the block) *)
  type def_vars = (string * int) list

  (* variable for storing input variable name *)
  let in_var = ref "in" ;;

  (* Block state                   *)
  type block_state = VariableSet.t * def_vars * VariableSet.t;;

  (* Compute the defined variables in a node *)
  let compute_node_def_vars (node : ControlFlowGraph.node) : def_vars =
    let i = ref 0 in
    let vars = List.fold_left (fun acc (command : ControlFlowGraph.command) ->
      let curr = !i in
      match command with
      | Assign (var, _) -> incr i; (var, curr) :: acc
      | _ -> acc
    ) [] node.command in
    vars
  ;;

  let start_state (node : ControlFlowGraph.node) (t : VariableSet.t) : block_state = 
    (t , compute_node_def_vars node, t)
  ;;

  let compute_T (nodes : ControlFlowGraph.node list) : VariableSet.t =
    List.fold_left (
      fun acc node ->
        let block_def = compute_node_def_vars node in
        let block_def_to_set = VariableSet.of_list (List.map fst block_def) in
        VariableSet.union acc block_def_to_set
      ) (VariableSet.singleton !in_var) nodes
  ;;

  (* Initialize the state for each node in the CFG *)
  let init_state (nodes : ControlFlowGraph.node list) : (string, block_state) Hashtbl.t =
    let state = Hashtbl.create (List.length nodes) in
    let t = compute_T nodes in
    List.iter (fun (node : ControlFlowGraph.node) -> Hashtbl.add state node.id (start_state node t)) nodes;
    state
  ;;

  let check_in_block_def (var : string) (def_vars : def_vars) (max_line : int) : bool =
    List.exists (fun (x, i) -> x = var && i <= max_line) def_vars
  ;;

  let rec check_aexp_defined_vars (exp : aexp) (in_vars : VariableSet.t) (def_vars : def_vars) (i : int) : unit =
    match exp with
    | Num _ -> ()
    | Var v -> if not (VariableSet.mem v in_vars) && not (check_in_block_def v def_vars i)
                then failwith ("Variable " ^ v ^ " not defined")
                else ()
    | Plus (a1, a2) -> check_aexp_defined_vars a1 in_vars def_vars i; check_aexp_defined_vars a2 in_vars def_vars i
    | Minus (a1, a2) -> check_aexp_defined_vars a1 in_vars def_vars i; check_aexp_defined_vars a2 in_vars def_vars i
    | Times (a1, a2) -> check_aexp_defined_vars a1 in_vars def_vars i; check_aexp_defined_vars a2 in_vars def_vars i
  ;;

  let rec check_bexp_defined_vars (exp : bexp) (in_vars : VariableSet.t) (def_vars : def_vars) (i : int) : unit =
    match exp with
    | Bool _ -> ()
    | And (b1, b2) -> check_bexp_defined_vars b1 in_vars def_vars i; check_bexp_defined_vars b2 in_vars def_vars i
    | Not b -> check_bexp_defined_vars b in_vars def_vars i
    | Less (a1, a2) -> check_aexp_defined_vars a1 in_vars def_vars i; check_aexp_defined_vars a2 in_vars def_vars i
  ;;

  let check_undef_block_vars (node : ControlFlowGraph.node) (in_vars : VariableSet.t) (def_vars : def_vars) : unit =
    let i = ref 0 in
    List.iter (fun (command : ControlFlowGraph.command) ->
      match command with
      | Assign (_, aexp) -> check_aexp_defined_vars aexp in_vars def_vars !i; incr i;
      | If (bexp) -> check_bexp_defined_vars bexp in_vars def_vars !i; incr i;
      | While (bexp) -> check_bexp_defined_vars bexp in_vars def_vars !i; incr i;
      | _ -> ()
    ) node.command
    ;;

  (* dv_in(L) ∪ {variables defined in L } *)
  let compute_incoming_vars (node : ControlFlowGraph.node) ( state : (string, block_state) Hashtbl.t ) (cfg : ControlFlowGraph.cfg) : VariableSet.t =

    if node.id = cfg.i.id then

      VariableSet.add !in_var VariableSet.empty

    else
      List.fold_left (fun acc (n: string) ->

        let (_, _, out_vars) = Hashtbl.find state n in

        if VariableSet.is_empty acc then
          VariableSet.union acc out_vars
        else
          (* Intersect in_dv with out_vars *)
          VariableSet.inter acc out_vars

      ) VariableSet.empty (ControlFlowGraph.get_in_nodes node cfg)
  ;;


  (* dv_in(L) ∪ {variables defined in L } *)
  let compute_outgoing_vars (node : ControlFlowGraph.node) ( state : (string, block_state) Hashtbl.t ) (_cfg : ControlFlowGraph.cfg) : VariableSet.t =
    let (in_vars, def_vars, _) = Hashtbl.find state node.id in

    (* Merge the two sets *)
    VariableSet.union in_vars (VariableSet.of_list (List.map fst def_vars))
  ;;

  (* Print the current state for debugging purposes *)
  let print_current_state (global_state : (string, block_state) Hashtbl.t) : unit =
    Hashtbl.iter (fun block_id (in_vars, def_vars, out_vars) ->
      Printf.printf "Block %s:\n" block_id;
      Printf.printf "  In: %s\n" ("{" ^ (String.concat ", " (VariableSet.elements in_vars)) ^ "}");
      Printf.printf "  Def: %s\n" ("{" ^ (String.concat ", " (List.map fst def_vars)) ^ "}");
      Printf.printf "  Out: %s\n" ("{" ^ (String.concat ", " (VariableSet.elements out_vars)) ^ "}");
    ) global_state;
  ;;


  let rec find_fixpoint (g_state : (string, block_state) Hashtbl.t) (cfg : ControlFlowGraph.cfg) : unit =
    let update = ref false
    in 
    (* Iterate over each node in the CFG *)
    List.iter (fun (node: ControlFlowGraph.node) ->

      let res = Hashtbl.find_opt g_state node.id in
      match res with
      | None -> failwith ("Node " ^ node.id ^ " not found in global state")
      | Some (in_vars, def_vars, out_vars) -> (
        let new_in_vars : VariableSet.t = compute_incoming_vars node g_state cfg in
        let new_out_vars : VariableSet.t = compute_outgoing_vars node g_state cfg in

        if not (VariableSet.equal in_vars new_in_vars) || not (VariableSet.equal out_vars new_out_vars) then (
          update := true
        );

        Hashtbl.replace g_state node.id (new_in_vars, def_vars, new_out_vars)

      )
    ) cfg.nodes;

    if !update then find_fixpoint g_state cfg
    else ()
  ;;

  (* Check for undefined variables in the global state *)
  let check_undef_vars (global_state : (string, block_state) Hashtbl.t) (cfg : ControlFlowGraph.cfg) : unit =
    List.iter (fun (node: ControlFlowGraph.node) ->
      let (in_vars, def_vars, _) = Hashtbl.find global_state node.id in

      check_undef_block_vars node in_vars def_vars
      
    ) cfg.nodes
  ;;

  (* Analyze the defined variables in the CFG *)
  let analyze_defined_vars (cfg : ControlFlowGraph.cfg) (in_name : string) : unit =

    in_var := in_name;

    (* Init global state *)
    let global_state = init_state cfg.nodes in

    (* Find the maximal fixpoint *)
    find_fixpoint global_state cfg;

    (* Check undefined variables *)
    check_undef_vars global_state cfg
  ;;

end