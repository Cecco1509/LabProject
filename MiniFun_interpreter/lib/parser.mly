%{
  open Semantics
%}

(* tokens *)
%token <string> VAR
%token <int> NUM
%token IF THEN ELSE
%token LESS NOT AND TRUE FALSE
%token FUN LET LETFUN ASSIGN IN ARROW
%token PLUS MINUS TIMES
%token LPAREN RPAREN EOF

%type <Semantics.term> trm
%type <Semantics.term> parterm
%type <Semantics.term> sterm
%type <Semantics.term> appterm
(* start nontrminal *)
%start term
%type <Semantics.term> term

(* precedences *)

%left PLUS MINUS
%left TIMES
%left AND

%nonassoc NOT
%nonassoc LESS



%% (* grammar *)
term:
  | t = trm; EOF                                                                { t }

(* (t) terms surrounded by () *)
parterm:
  | LPAREN; t = trm; RPAREN                                                     { t }

trm:
  | FUN; x = VAR; ARROW; t = sterm                                              { Fun(x, t) }
  | LET; x = VAR; ASSIGN; t1 = sterm; IN; t2 = sterm;                           { Let(x, t1, t2) }
  | LETFUN; f = VAR; x = VAR; ASSIGN; t1 = sterm; IN; t2 = sterm                { LetFun(f, x, t1, t2) }
  | IF; b = sterm; THEN; t1 = sterm; ELSE; t2 = sterm                           { IfThenElse(b, t1, t2) }
  | t = appterm                                                                 { t }

sterm: (* simple term *)
  | n = NUM                                                                     { TNum n }
  | x = VAR                                                                     { Var x }  (* not *)
  | LPAREN; MINUS; n = NUM; RPAREN                                              { TNum (-n) }
  | NOT; t = sterm                                                              { Not t }
  | TRUE                                                                        { TBool true }
  | FALSE                                                                       { TBool false }
  | t = parterm                                                                 { t }
  | t1 = sterm; PLUS; t2 = sterm                                                { Plus(t1, t2) }
  | t1 = sterm; MINUS; t2 = sterm                                               { Minus(t1, t2) }
  | t1 = sterm; TIMES; t2 = sterm                                               { Times(t1, t2) }
  | t1 = sterm; AND; t2 = sterm                                                 { And(t1, t2) }
  | t1 = sterm; LESS; t2 = sterm                                                { Less(t1, t2) }

appterm:
  | t1 = appterm; t2 = sterm                                                    { FunApp(t1, t2) }
  | t = sterm;                                                                  { t }
