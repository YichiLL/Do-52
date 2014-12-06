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
%token OPENPAREN CLOSEPAREN OPENBRACE CLOSEBRACE
%token <bool> BOOL_LITERAL
%token <string> STRING_LITERAL ID
%token <int> NUMBER_LITERAL
%token ADD MINUS TIMES DIVIDE LT LTOE GT GTOE EQUAL NOTEQUAL DOT
%token NOT DISJ CONJ
%token PREPEND_TOP PREPEND_BOTTOM APPEND_TOP APPEND_BOTTOM

/* Lowest Precedence */
%left DISJ CONJ
%left EQUAL NOTEQUAL
%nonassoc LT LTOE GT GTOE
%left ADD MINUS
%left TIMES DIVIDE
%right NOT
%left DOT
/* Highest Precedence */

%start program
%type <Ast.program> program

%%

program:
    | config_list vdecl_list func_list      { { configs = List.rev $1;
                                                vars = List.rev $2;
                                                funcs = List.rev $3; } }
    | config_list func_list                 { { configs = List.rev $1;
                                                vars = [];
                                                funcs = List.rev $2; } }

config_list:
    | /* nothing */                         { [] }
    | config_list config                    { $2 :: $1 }
    | config_list NEWLINE                   { $1 }

config:
    | CONFIGURE ID COLON expr               { { id = $2; value = $4 } }

vdecl_list:
    | vdecl                                 { [$1] }
    | vdecl_list vdecl                      { $2 :: $1 } 
    | vdecl_list NEWLINE                    { $1 }

vdecl:
    | NEW ID ID COLON expr                  { VarDecl({ id = $3;
                                                        _type = $2;
                                                        value = $5 }) }

func_list:
    | func                                  { [$1] }
    | func_list func                        { $2 :: $1 }
    | func_list NEWLINE                     { $1 }

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
    | OPENBRACE stmt CLOSEBRACE TIMES expr  { TimesLoop($2, $5) }
    | expr PREPEND_TOP expr                 { Prepend($1, $3, Top) }
    | expr PREPEND_BOTTOM expr              { Prepend($1, $3, Bottom) }
    | expr APPEND_TOP expr                  { Append($1, $3, Top) }
    | expr APPEND_BOTTOM expr               { Append($1, $3, Bottom) }

arg_list:
    | expr                                  { [$1] }
    | arg_list AND expr                     { $3 :: $1 }

update:
    | vdecl                                 { $1 }
    | ID COLON expr                         { Assign($1, $3) }

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
    | expr DOT expr                         { Binop($1, Dot, $3) }
    | NOT expr                              { Unop(Not, $2) }
    | OPENPAREN expr CLOSEPAREN             { $2 }

%%

