%{ 
(* parser.mly takes a sequence of tokens produced by the scanner and assembles
 * them into an abstract syntax tree. Each pattern-action rule takes some
 * pattern in the thus-far-assembled input and createse a "higher" type out
 * of it. *)

open Ast 

%}

%token EOF
%token NEWLINE
%token DO WITH AND
%token OPENPAREN CLOSEPAREN 
%token <bool> BOOL_LITERAL
%token <string> STRING_LITERAL ID
%token <int> NUMBER_LITERAL
%token ADD MINUS TIMES DIVIDE LT LTOE GT GTOE EQUAL NOTEQUAL
%token NOT DISJ CONJ

/* Lowest Precedence */
%left DISJ CONJ
%left EQUAL NOTEQUAL
%left LT LTOE GT GTOE
%left ADD MINUS
%left TIMES DIVIDE
%right NOT
/* Highest Precedence */

%start program
%type <Ast.program> program

%%

program:
    | program_rev                   { List.rev $1 }

program_rev: 
    | /* nothing */                 { [] }
    | program_rev stmt              { $2 :: $1 }

stmt:
    | stmt NEWLINE                  { $1 }
    | expr                          { Expr($1) }
    | DO ID                         { Call({ fname = $2; args = [] }) }
    | DO ID WITH args_list          { Call({ fname = $2; 
                                             args = List.rev $4 }) }

expr:
    | NUMBER_LITERAL                { Number($1) }
    | BOOL_LITERAL                  { Boolean($1) }
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
    | expr DISJ expr                { Binop($1, Disj, $3) }
    | expr CONJ expr                { Binop($1, Conj, $3) }
    | NOT expr                      { Unop(Not, $2) }
    | OPENPAREN expr CLOSEPAREN     { $2 }

args_list:
    expr                            { [$1] }
    | args_list AND expr            { $3 :: $1 }

%%

