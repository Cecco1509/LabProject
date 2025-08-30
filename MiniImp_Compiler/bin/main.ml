open MiniImp.ControlFlowGraph
open MiniImp.DefinedVars
open MiniImp.MiniRiscCfg
open MiniImp.RegisterAllocation
open MiniImp.GenerateCode

(* Main function to run the MiniImp compiler *)

let () =
  (* Check if the correct number of arguments is passed *)
  if Array.length Sys.argv <> 6 then
    begin
      Printf.eprintf "Usage: %s <enable/disable defined var check> <enable/disable optimization> <num of registers> <program.miniimp> <output.minirisc>\n" Sys.argv.(0);
      exit 1
    end;

  (* Assign command line arguments to variables, also check for flags *)
  let num_registers : int     = int_of_string Sys.argv.(3) in
  let program_file  : string  = Sys.argv.(4) in
  let output_file   : string  = Sys.argv.(5) in
  let def_vars_flag : bool    = bool_of_string Sys.argv.(1) in
  let opt_flag      : bool    = bool_of_string Sys.argv.(2) in

  (* Read the MiniImp program from the file *)
  let lexbuf = Lexing.from_channel (open_in program_file) in

  (* Parse the program *)
  let program = try
      MiniImp.Parser.program MiniImp.Lexer.read lexbuf
    with
    | MiniImp.Parser.Error ->
      Printf.eprintf "Syntax error while parsing %s\n" program_file;
      exit 1
  in

  Printf.printf "Program parsed successfully\n";

  let ( in_var, out_var, body) = match program with
    | Main (in_var, out_var, body) -> (in_var, out_var, body)
  in

  let cfg = ControlFlowGraph.build_cfg body in

  Printf.printf "Control Flow Graph built\n";
  (* -D flag Check defined variables *)
  if def_vars_flag then (
    DefinedVars.analyze_defined_vars cfg in_var;
    Printf.printf "No undefined variables detected\n";
  );

  let translation_result = MiniRiscCfg.translate cfg in_var out_var in
  Printf.printf "Translation to MiniRISC IR done\n";

  (* -O optimization flag*)
  if opt_flag then
    (* Register allocation -> Change this -> move part of the optimization to the Optimization module *)
    RegisterAllocation.allocate_registers cfg num_registers translation_result
  else
    (* If optimization is disabled, we still have to handle spilled registers*)
    RegisterAllocation.handle_spilling_register num_registers translation_result;

  (* generate the final minirisc code
    * Error -> doesn't care about spills
  *)
  GenerateCode.generate_code cfg translation_result output_file;

  (* Exit successfully *)
  exit 0 

;;