%{
  open Semantics
%}

(* tokens *)
%token <string> VAR
%token <int> NUM
%token DEF MAIN WITH INPUT OUTPUT AS
%token IF THEN ELSE WHILE DO
%token NOT AND LESS TRUE FALSE
%token ASSIGN PLUS MINUS TIMES
%token SKIP SEMICOLON LPAREN RPAREN EOF

%type <Semantics.c> c
%type <Semantics.aexp> aexp
%type <Semantics.bexp> bexp

(* start nonterminal *)
%start program
%type <Semantics.prog> program

(* precedences *)
%nonassoc ELSE

%left PLUS MINUS
%left TIMES

%left AND
%nonassoc NOT

%left DO

%right SEMICOLON


%% (* grammar *)

program:
  | DEF; MAIN; WITH; INPUT; input = VAR; OUTPUT; output = VAR; AS; command = c; EOF  { Main(input, output, command) }

c:
  | varname = VAR; ASSIGN; a = aexp                                                  { Let(varname, a) }    (* x := a *)
  | c1 = c; SEMICOLON; c2 = c                                                        { Seq(c1, c2) }        (* c1; c2 *)
  | IF; b = bexp; THEN; c1 = c; ELSE; c2 = c                                         { If(b, c1, c2) }      (* if b then c1 else c2 *)
  | WHILE; b = bexp; DO; c = c                                                       { While(b, c) }        (* while b do c *)
  | LPAREN; c = c; RPAREN                                                            { c }                  (* (c) *)
  | SKIP                                                                             { Skip }

bexp:
  | TRUE                                                                             { Bool true }
  | FALSE                                                                            { Bool false }
  | NOT; b = bexp                                                                    { Not(b) }
  | b1 = bexp; AND; b2 = bexp                                                        { And(b1, b2) }
  | a1 = aexp; LESS; a2 = aexp                                                       { Less(a1, a2) }

aexp:
  | varname = VAR                                                                    { Var(varname) }
  | int = NUM                                                                        { Num(int) }
  | MINUS; int = NUM                                                                 { Num(-int) }
  | a1 = aexp; PLUS; a2 = aexp                                                       { Plus(a1, a2) }
  | a1 = aexp; MINUS; a2 = aexp                                                      { Minus(a1, a2) }
  | a1 = aexp; TIMES; a2 = aexp                                                      { Times(a1, a2) }
  | LPAREN; a = aexp; RPAREN                                                         { a }