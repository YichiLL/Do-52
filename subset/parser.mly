%{ 
(* parser.mly takes a sequence of tokens produced by the scanner and assembles
 * them into an abstract syntax tree. Each pattern-action rule takes some
 * pattern in the thus-far-assembled input and createse a "higher" type out
 * of it. *)

open Ast 
%}

%token EOF
%token NEWLINE
%token <string> STRING_LITERAL ID
%token <int> NUMBER_LITERAL
%token ADD MINUS TIMES DIVIDE LT LTOE GT GTOE EQUAL NOTEQUAL
%token OPENPAREN CLOSEPAREN 
%token DO WITH

/* Lowest Precedence */
%left EQUAL NOTEQUAL
%left LT LTOE GT GTOE
%left ADD MINUS
%left TIMES DIVIDE
/* Highest Precedence */

%start program
%type <Ast.program> program

%%

program: 
      /*nothing*/                   { [] }
    | program stmt                  { $2 :: $1 }

stmt:
      expr NEWLINE                  { Expr($1) }
    | expr EOF                      { Expr($1) }

expr:
      NUMBER_LITERAL                { Number($1) }
    | expr ADD expr                 { Binop($1, Add, $3) }
    | expr MINUS expr               { Binop($1, Minus, $3) }
    | expr TIMES expr               { Binop($1, Multiply, $3) }
    | expr DIVIDE expr              { Binop($1, Divide, $3) }
    | expr LT expr                  { Binop($1, Lt, $3) }
    | expr LTOE expr                { Binop($1, Ltoe, $3) }
    | expr GT expr                  { Binop($1, Gt, $3) }
    | expr GTOE expr                { Binop($1, Gtoe, $3) }
    | expr EQUAL expr               { Binop($1, Equal, $3) }
    | expr NOTEQUAL expr            { Binop($1, NotEqual, $3) }
    | OPENPAREN expr CLOSEPAREN     { $2 }

%%

