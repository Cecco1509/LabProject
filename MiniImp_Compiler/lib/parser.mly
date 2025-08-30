%{
  open Ast
%}

(* tokens *)
%token <string> VAR
%token <int> NUM
%token DEF MAIN WITH INPUT OUTPUT AS
%token IF THEN ELSE WHILE DO
%token NOT AND LESS TRUE FALSE
%token ASSIGN PLUS MINUS TIMES
%token SKIP SEMICOLON LPAREN RPAREN EOF

%type <Ast.c> cmd
%type <Ast.aexp> aexp
%type <Ast.bexp> bexp

(* start nonterminal *)
%start program
%type <Ast.prog> program
%type <Ast.c> scmd
%type <Ast.c> parcmd

(* precedences *)
%left PLUS MINUS
%left TIMES

%right NOT
%left AND

%% (* grammar *)

program:
  | DEF; MAIN; WITH; INPUT; input = VAR; OUTPUT; output = VAR; AS; command = cmd; EOF  { Main(input, output, command) }

cmd:
  | c = scmd;                                                                        { c }
  | c1 = scmd; SEMICOLON; c2 = cmd                                                   { Seq(c1, c2) }

scmd: (* simple command *)
  | varname = VAR; ASSIGN; a = aexp                                                  { Assign(varname, a) }
  | IF; b = bexp; THEN; c1 = scmd; ELSE; c2 = scmd                                   { If(b, c1, c2) }
  | WHILE; b = bexp; DO; c = scmd                                                    { While(b, c) }
  | c = parcmd                                                                       { c }
  | SKIP                                                                             { Skip }

parcmd:
  | LPAREN; c = cmd; RPAREN                                                          { c }

bexp:
  | TRUE                                                                             { Bool true }
  | FALSE                                                                            { Bool false }
  | NOT; b = bexp                                                                    { Not(b) }
  | b1 = bexp; AND; b2 = bexp                                                        { And(b1, b2) }
  | a1 = aexp; LESS; a2 = aexp                                                       { Less(a1, a2) }
  | LPAREN; b = bexp; RPAREN                                                         { b }

aexp:
  | varname = VAR                                                                    { Var(varname) }
  | int = NUM                                                                        { Num(int) }
  | MINUS; int = NUM                                                                 { Num(- int) }
  | a1 = aexp; PLUS; a2 = aexp                                                       { Plus(a1, a2) }
  | a1 = aexp; MINUS; a2 = aexp                                                      { Minus(a1, a2) }
  | a1 = aexp; TIMES; a2 = aexp                                                      { Times(a1, a2) }
  | LPAREN; a = aexp; RPAREN                                                         { a }