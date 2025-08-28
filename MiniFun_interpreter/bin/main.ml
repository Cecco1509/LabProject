let () =
  (* Check if the correct number of arguments is passed *)
  if Array.length Sys.argv <> 2 then
    begin
      Printf.eprintf "Usage: %s <program.minifun>\n" Sys.argv.(0);
      exit 1
    end;

  let program_file = Sys.argv.(1) in

  (* Read the MiniImp program from the file *)
  let ic = open_in program_file in
  let lexbuf = Lexing.from_channel ic in
  
  (* Set the filename in lexbuf for better error reporting *)
  Lexing.set_filename lexbuf program_file;

  (* Parse the program *)
  let term = try
      MiniFun.Parser.term MiniFun.Lexer.read lexbuf
    with
    | MiniFun.Parser.Error -> 
      let pos = lexbuf.Lexing.lex_curr_p in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      let token = Lexing.lexeme lexbuf in
      Printf.eprintf "\nSyntax error at line %d, column %d (near '%s') in file %s\n"
        line
        column
        token
        program_file;
      
      (* Try to print the problematic line *)
      begin
        try
          close_in ic;
          let ic = open_in program_file in
          let rec skip_lines n =
            if n <= 0 then ()
            else (ignore (input_line ic); skip_lines (n-1))
          in
          skip_lines (line - 1);
          let error_line = input_line ic in
          Printf.eprintf "Line %d: %s\n" line error_line;
          Printf.eprintf "%s^\n" (String.make (column - 1) ' ');
          close_in ic
        with _ ->
          (* If reading the file fails, just continue *)
          ()
      end;
      
      exit 1
  in
  
  close_in ic;

  (* Now we read the integer input from the user via standard input *)
  Printf.printf "\nEnter an integer: ";
  let input_value = int_of_string (read_line ()) in

  (* Evaluate the program with the provided input value *)
  Printf.printf "Output: %s\n" (MiniFun.Semantics.eval term input_value)
;;