module ControlFlowGraph = struct

  open Ast

  (* Node types for the control flow graph *)
  type node_type = If | While | Assign | Skip

  (* Reduced AST type, used for marking each instruction without carrying full AST. *)
  type command =
    | Skip
    | Assign of var * aexp
    | If of bexp
    | While of bexp

  (* Control flow graph node type *)
  type node = {
    id: string;                     (* unique identifier for the node *)
    command: command list;          (* the commands contained by this node *)
    node_type: node_type;           (* type of the node *)
  }

  (* Control flow graph type *)
  type cfg = {
    nodes: node list;               (* list of nodes, initial and final nodes are excluded from this list *)
    edges: (string * string) list;  (* adjacency list representation of edges *)
    i: node;                        (* entry node *)
    f: node;                        (* final node *)
  }

  (* Counter for unique node IDs *)
  let counter = ref 0

  (* function that returns a new id as string*)
  let get_new_id () =
    let id = Printf.sprintf "node_%d" !counter in
    incr counter;
    id

  (* helper function to convert node types to strings *)
  let node_type_to_string (nt: node_type) : string =
    match nt with
    | If -> "If"
    | While -> "While"
    | Assign -> "Assign"
    | Skip -> "Skip"

  (* Helper function to convert AST commands to reduced commands *)
  let to_command (c: Ast.c) : command =
    match c with
    | Skip -> Skip
    | Assign(v, a) -> Assign(v, a)
    | If(b, _, _) -> If(b)
    | While(b, _) -> While(b)
    | _ -> failwith "Unsupported command type"

  let command_to_string (c: command) : string =
    match c with
    | Skip -> "Skip"
    | Assign(v, a) -> Printf.sprintf "%s := %s" v (Ast.string_of_aexp a)
    | If(b) -> Printf.sprintf "If (%s)" (Ast.string_of_bexp b)
    | While(b) -> Printf.sprintf "While (%s)" (Ast.string_of_bexp b) 

  (**** Creates an empty control flow graph, with a skip node inside ****)
  let empty_cfg ?(final=None) (): cfg =
    let skip_node : node = { id = get_new_id(); command = [Skip]; node_type = Skip } in
    match final with 
    | None -> 
      {
        nodes = [];
        edges = [];
        i = skip_node;
        f = skip_node;
      }
    | Some final_node ->
      {
        nodes = [];
        edges = [(skip_node.id, final_node.id)];
        i = skip_node;
        f = final_node;
      }
  ;;

  (**************************** Helper function to merge edges of if-else ****************************)
  let merge_if_edges (conditional_node: node) (then_branch: cfg option) (else_branch: cfg option) (final_node: node): (string * string) list =

    match (then_branch, else_branch) with
    | (None, None) -> [
        (conditional_node.id, final_node.id);
        (conditional_node.id, final_node.id)
      ]
    | (Some tb, None) ->
        (conditional_node.id, tb.i.id)          (* cond -> then initial *)
        :: (conditional_node.id, final_node.id) (* cond -> final node     *)
        :: tb.edges                             (* then block edges     *)
    | (None, Some eb) ->
        (conditional_node.id, eb.i.id)          (* cond -> else initial *)
        :: (conditional_node.id, final_node.id) (* cond -> final node     *)
        :: eb.edges                             (* else block edges     *)
    | (Some tb, Some eb) -> 
        (conditional_node.id, tb.i.id)          (* cond -> then initial *)
        :: (conditional_node.id, eb.i.id)       (* cond -> else initial *)
        :: tb.edges                             (* then block edges     *)
        @ eb.edges                              (* else block edges     *)
    ;;

  (**************************** Helper function to merge nodes of if-else ****************************)
  let merge_if_nodes (then_branch: cfg option) (else_branch: cfg option) : node list =

    match (then_branch, else_branch) with
    | (None, None) -> []
    | (Some tb, None) -> tb.i :: tb.nodes
    | (None, Some eb) -> eb.i :: eb.nodes
    | (Some tb, Some eb) -> tb.i :: tb.nodes @ eb.i :: eb.nodes

  (**************************** Helper function to merge edges of while loop ****************************)
  let merge_while_edges (conditional_node: node) (body_branch: cfg) (final_node: node): (string * string) list =

    (conditional_node.id, body_branch.i.id)          (* cond -> body initial *)
    :: (conditional_node.id, final_node.id)          (* cond -> final node     *)
    :: body_branch.edges                             (* body block edges     *)

  (**************************** Helper function to get edges to final node and remove them ****************************)
  let get_edges_to_final (cfg: cfg) : (string * string) list * (string * string) list =
    (* Partition the edges into those leading to the final node and others *)
    let (other_edges, final_edges) = List.partition (fun (_, dst) -> dst <> cfg.f.id) cfg.edges in
    (other_edges, final_edges)
  ;;

  (**************************** Helper function that merges two consecutive cfg ***********************************)
  (* Merges the final and initial node of the CFGs *)
  let merge_cfg (first : cfg) (second : cfg) : cfg =
    
    match second.i.node_type with
    | While -> 
      (* In case the second CFG starts with a While, we cannot merge the two nodes *)
      { 
        nodes = first.nodes @ (second.i :: second.nodes); 
        edges = first.edges @ [(first.f.id, second.i.id)] @ second.edges; 
        i = first.i; 
        f = second.f 
      }
    | _ ->
      (* Retrieve all the edges to final node *)
      let (edges, removed) = get_edges_to_final first in

      (* Creates new updated edges to block *)
      let new_edges : (string * string) list = 
        if List.length removed > 0 then
          List.map (fun (src, _first_final) -> (src, second.i.id)) removed
        else
          []
      in

      (* Remove unnecessary Skip commands *)
      let new_commands =
        match first.f.command, second.i.command with
        | [], [] -> [Skip]
        | Skip::[], cmds -> cmds                         (* Case [Skip], [command1, command2] *)
        | _, Skip::cmds -> first.f.command @ cmds        (* Case [commands...], [Skip, command1, command2] || Case [commands...], [Skip] *)
        | _, _ -> first.f.command @ second.i.command     (* Case [commands...], [commands...] *)
      in

      (* Create new combined node, which is the updated version of the initial node of the second CFG *)
      (* So the direction for merging is always from the final node of 1° CFG to the initial node of 2° CFG *)
      let combined_node = { second.i with command = new_commands; }
      in

      match first.nodes with
      | [] -> { (* Case: first CFG contains no nodes *)
        second with
        edges = edges @ new_edges @ second.edges;
        i = combined_node;
      }
      | _ when second.i.id = second.f.id -> { (* Case: second CFG contains no nodes *)
        nodes = first.nodes;
        edges = edges @ new_edges @ second.edges;
        i = first.i;
        f = combined_node
      }
      | _ -> { (* General case: both CFGs contain nodes *)
        nodes = first.nodes @ [combined_node] @ second.nodes;
        edges = edges @ new_edges @ second.edges;
        i = first.i;
        f = second.f
      }
  ;;

  (**************************** Function to merge two uncontained CFGs ****************************)
  let merge_seq_cfgs_uncontained (cfg1: cfg option) (cfg2: cfg option) : cfg =
    match cfg1, cfg2 with
    | None, None -> empty_cfg ()
    | Some first, None -> first
    | None, Some second -> second
    | Some first, Some second -> merge_cfg first second
  ;;

  (* merges two sequential CFGs that are contained in a block *)
  let merge_seq_cfgs_contained (cfg1: cfg option) (cfg2: cfg option) (block : node) : cfg =

    match (cfg1, cfg2) with
    | (None, None) -> empty_cfg ~final:(Some block) ()
    | (Some first, None) -> 
      (* This case covers the situation -> NonSkip; Skip*)
      let (edges, removed) = get_edges_to_final first in

      (* Creates new updated edges to block *)
      let new_edges : (string * string) list = 
        if List.length removed > 0 then
          List.map (fun (src, _first_final) -> (src, block.id)) removed
        else
          [(first.f.id, block.id)]
      in

      {
        first with
        edges = edges @ new_edges; (* Inserts back the removed edges with changed destination *)
        f = block
      }
    | (None, Some second) -> second
    | (Some first, Some second) -> merge_cfg first second
  ;;


  (**************************** Function to create a control flow graph from a command ****************************)
  (*
    com => the Ast command
    contained => whether the command 'com' is the last command in a WhileBody/IfThen/IfElse block
    block => != None only if contained is true. 
      This param represent the block to which this command should return.
  *)
  let rec create_cfg (com: Ast.c) (contained : bool) (block : node option): cfg option =

    match com with
    | Skip when not contained -> Some (empty_cfg ())
    | Skip -> None
    | Assign(_, _) -> 
      let assign_node = {id = get_new_id(); command = [to_command com]; node_type = Assign } in
      let final_node = if contained then Option.get block else assign_node in

      (* if the assignment is contained in a If/While block it's required to add an edge to the final block *)
      (* this only happens if this command is the last one of the block/sequence *)
      let edges = if contained then [(assign_node.id, final_node.id)] else [] in

      Some { 
        nodes = []; 
        edges = edges;
        i = assign_node;
        f = final_node
      }
    | If(_, c1, c2) -> 
      let conditional_node = {id = get_new_id(); command = [to_command com]; node_type = If } in
      let final_node = if contained then Option.get block else {id = get_new_id(); command = [Skip]; node_type = Skip} in
      let then_branch = create_cfg c1 true (Some final_node) in
      let else_branch = create_cfg c2 true (Some final_node) in

      let edges = merge_if_edges conditional_node then_branch else_branch final_node in
      let nodes = merge_if_nodes then_branch else_branch in

      Some { 
        nodes = nodes; 
        edges = edges;
        i = conditional_node;
        f = final_node
      }
    | While(_, c1) -> 
      let guard_node = {id = get_new_id(); command = [to_command com]; node_type = While } in
      let (initial_node, initial_edges, final_node) = 
        if contained then
          (guard_node, [], Option.get block)
        else
          let initial_id = get_new_id() in
          let final_id = get_new_id() in
          (
            {id = initial_id; command = [Skip]; node_type = Skip}, 
            [(initial_id, guard_node.id)],
            {id = final_id; command = [Skip]; node_type = Skip}
          ) 
      in

      let body : cfg = Option.value (create_cfg c1 true (Some guard_node)) ~default:(empty_cfg ~final:(Some guard_node) ()) in

      let edges = merge_while_edges guard_node body final_node in
      let nodes = 
        if contained then body.i :: body.nodes
        else guard_node :: body.i :: body.nodes
      in

      Some { 
        nodes = nodes; 
        edges = initial_edges @ edges; 
        i = initial_node;
        f = final_node;
      }
    | Seq(c1, c2) -> 
      let temp_cfg1 = create_cfg c1 false None in
      let cfg2 = create_cfg c2 contained block in

      (* Remove first Skip node if the first command is a While and this Sequence is contained *)
      let cfg1 = 
        if contained then 
          match c1, temp_cfg1 with
          | (While(_, _), Some value) ->
            let guard_node = List.hd value.nodes in
            let nodes      = List.tl value.nodes in
            let edges      = List.tl value.edges in
            Some { 
              i = guard_node;
              f = value.f;
              nodes = nodes;
              edges = edges;
            }
          | _ -> temp_cfg1
        else temp_cfg1

      in

      (* Then merge the two CFGs *)
      if contained then
        Some (merge_seq_cfgs_contained cfg1 cfg2 (Option.get block))
      else
        Some (merge_seq_cfgs_uncontained cfg1 cfg2)
  ;;

  (*************** Function to build the CFG from an AST command ***************)
  let build_cfg (com: Ast.c) : cfg =
    match create_cfg com false None with
    | Some cfg when cfg.i.id = cfg.f.id -> {cfg with nodes = [cfg.i]}  (* Case where the CFG contains only initial and final node *)
    | Some cfg -> 
      {
        cfg with
        nodes = cfg.i :: cfg.nodes @ [cfg.f]  (* Include initial and final nodes in the nodes list *)
      }
    | None -> failwith "Failed to create CFG"
  ;;

  (***************************** Print CFG *****************************)
  let print_cfg (cfg: cfg) =
    let print_node (node : node) =
      Printf.printf "%s: %s\n" node.id (String.concat ", " (List.map command_to_string node.command))
    in
    let print_edge (src : string) (dst : string) =
      Printf.printf "Edge from %s to %s\n" src dst
    in
    Printf.printf "CFG:\n";
    print_node cfg.i;
    List.iter print_node cfg.nodes;
    print_node cfg.f;
    List.iter (fun (src, dst) -> print_edge src dst) cfg.edges;
  ;;

  (*********************** Function to get out nodes given a specific node ***********************)
  let get_out_nodes (n : node) (cfg : cfg) : string list =
    List.map (fun (_, t) -> t) (List.filter (fun (f, _) -> f = n.id) cfg.edges)
  ;;

  (*********************** Function to get in nodes given a specific node ************************)
  let get_in_nodes (n : node) (cfg : cfg) : string list =
    List.map (fun (f, _) -> f) (List.filter (fun (_, t) -> t = n.id) cfg.edges)
  ;;

end