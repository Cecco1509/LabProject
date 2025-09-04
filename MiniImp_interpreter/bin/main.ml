
let () =
  (* Check if the correct number of arguments is passed *)
  if Array.length Sys.argv <> 2 then
    begin
      Printf.eprintf "Usage: %s <program.miniimp>\n" Sys.argv.(0);
      exit 1
    end;

  let program_file = Sys.argv.(1) in

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

  (* Now we read the integer input from the user via standard input *)
  let input_value = int_of_string (read_line ()) in

  (* Initialize an empty environment *)

  (* Evaluate the program with the provided input value *)
  match MiniImp.Semantics.eval_prg program input_value with
    | Some value -> Printf.printf "%d\n" value
    | None -> failwith "Program did not return any output value"
;;