%{ 
(* parser.mly takes a sequence of tokens produced by the scanner and assembles
 * them into an abstract syntax tree. Each pattern-action rule takes some
 * pattern in the thus-far-assembled input and createse a "higher" type out
 * of it. *)

open Ast 

%}

%token EOF
%token NEWLINE INDENT DEDENT
%token DO WITH AND
%token NEW COLON CONFIGURE
%token IF ELSE WHILE FOR SEMI BREAK CONTINUE
%token OPENPAREN CLOSEPAREN 
%token <bool> BOOL_LITERAL
%token <string> STRING_LITERAL ID
%token <int> NUMBER_LITERAL
%token ADD MINUS TIMES DIVIDE LT LTOE GT GTOE EQUAL NOTEQUAL
%token NOT DISJ CONJ

/* Lowest Precedence */
%left DISJ CONJ
%left EQUAL NOTEQUAL
%nonassoc LT LTOE GT GTOE
%left ADD MINUS
%left TIMES DIVIDE
%right NOT
/* Highest Precedence */

%start program
%type <Ast.program> program

%%

program:
	| func_list                            	{ List.rev $1 }

func_list:
    | /* nothing */                         { [] }
    | func_list func                        { $2 :: $1 }
    | func_list NEWLINE                     { $1 }

block:
    | INDENT stmt_list DEDENT               { List.rev $2 }

stmt_list:
    | /* nothing */                         { [] }
    | stmt_list stmt                        { $2 :: $1 }

stmt:
    | stmt NEWLINE                          { $1 }
    | update                                { Update($1) }
    | DO ID                                 { Call({ fname = $2; args = [] }) }
    | DO ID WITH arg_list                   { Call({ fname = $2; 
                                                     args = List.rev $4 }) }
    | IF expr COLON block                   { If($2, $4, []) }
    | IF expr COLON block ELSE COLON block  { If($2, $4, $7) }
    | WHILE expr COLON block                { While($2, $4) }
    | FOR update SEMI expr SEMI update 
        COLON block                         { For($2, $4, $6, $8) }
    | BREAK                                 { Break }
    | CONTINUE                              { Continue }

update:
    | NEW ID ID COLON expr                  { VarDecl({ id = $3;
                                                        _type = $2;
                                                        value = $5 }) }
    | ID COLON expr                         { Assign($1, $3) }

arg_list:
    | expr                                  { [$1] }
    | arg_list AND expr                     { $3 :: $1 }

func:
    | ID COLON block                        { { fname = $1;
                                                formals = [];
                                                body = $3; } }
    | ID WITH formal_list COLON block       { { fname = $1;
                                                formals = List.rev $3;
                                                body = $5; } }
formal_list:
    | formal                                { [$1] }
    | formal_list AND formal                { $3 :: $1 }

formal:
    | ID ID                                 { { id = $2; _type = $1 } }

expr:
    | NUMBER_LITERAL                        { Number($1) }
    | BOOL_LITERAL                          { Boolean($1) }
    | STRING_LITERAL                        { String($1) }
    | ID                                    { Id($1) } 
    | expr ADD expr                         { Binop($1, Add, $3) }
    | expr MINUS expr                       { Binop($1, Minus, $3) }
    | expr TIMES expr                       { Binop($1, Multiply, $3) }
    | expr DIVIDE expr                      { Binop($1, Divide, $3) }
    | expr LT expr                          { Binop($1, Lt, $3) }
    | expr LTOE expr                        { Binop($1, Ltoe, $3) }
    | expr GT expr                          { Binop($1, Gt, $3) }
    | expr GTOE expr                        { Binop($1, Gtoe, $3) }
    | expr EQUAL expr                       { Binop($1, Equal, $3) }
    | expr NOTEQUAL expr                    { Binop($1, NotEqual, $3) }
    | expr DISJ expr                        { Binop($1, Disj, $3) }
    | expr CONJ expr                        { Binop($1, Conj, $3) }
    | NOT expr                              { Unop(Not, $2) }
    | OPENPAREN expr CLOSEPAREN             { $2 }

%%

