{
  open Parser
  exception LexingError of string
}

let integer    = '0'|['1'-'9']['0'-'9']*                                (* 0 | 123 | 200 *)
let varname    = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*    (* x | x1 | x_1  *)
let white      = [' ' '\t' '\n' '\r']

rule read = parse
  | white         { read lexbuf }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "if"          { IF }
  | "then"        { THEN }
  | "else"        { ELSE }
  | "not"         { NOT }
  | "and"         { AND }
  | "let"         { LET }
  | "letfun"      { LETFUN }
  | "fun"         { FUN }
  | "in"          { IN }
  | "="           { ASSIGN }
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | "<"           { LESS }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { TIMES }
  | "=>"          { ARROW }
  | varname       { VAR (Lexing.lexeme lexbuf) }
  | integer       { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | _ { raise (LexingError (Printf.sprintf "Unexpected character: %s" (Lexing.lexeme lexbuf))) }