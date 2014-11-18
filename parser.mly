%{ open Ast %}

%token COMMENT NEWLINE TAB 
%token OR AND NOT DOTOP PREPEND APREND
%token SEMI OPENPAREN CLOSEPAREN OPENBLOCK CLOSEBLOCK COMMA PREPEN APPEND
%token ADD MINUS MULTIPLY DIVIDE ASSIGNMENT
%token EQUAL NOTEQUAL LT LTOE GT GTOE
%token IF ELSE FOR WHILE INT STRING BOOLEAN CARD SET PLAYER UNTIL BREAK CONTINUE
%token HAS CALLED DO WITH NEW CONFIG 
%token <string> LITERAL
%token <int> NUMBER
%token <string> ID
%token EOF

%left COMMENT NEWLINE
%nonassoc IF ELSEIF ELSE NOELSE
%right ASSIGNMENT
%left PREPEN APPEND
%left EQUAL NOTEQUAL LT GT GTOE LTOE
%left NOT AND OR
%left ADD MINUS
%left MULTIPLY DIVIDE
%left OPENPAREN CLOSEPAREN 
%left OPENBLOCK CLOSEBLOCK

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   ID WITH arg_list ASSIGNMENT vdecl_list stmt_list 
   { { 
      fname = $1;
      formals = $3;
      locals = List.rev $5;
      body = List.rev $6
    } } 
| ID ASSIGNMENT vdecl_list stmt_list 
  { {
     fname = $1;
     formals = [];
     locals = List.rev $3;
     body = List.rev $4
  } }

arg_opt:
  /* nothing */  { [] }
  | arg_list { List.rev $1}

arg_list: 
  arg { [$1] }
  | arg_list AND arg { $3 :: $1} 

arg: 
 INT ID { $2  }
 | STRING ID { $2 }
 | BOOLEAN ID { $2 } 

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
  INT ID { $2 }

condecl:
  CONFIG ID ASSIGNMENT expr {Config($2,$4)}

stmt:
    expr { Expr($1) }
  | OPENBLOCK stmt_list CLOSEBLOCK { Block(List.rev $2) }
  | IF OPENPAREN expr CLOSEPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF OPENPAREN expr CLOSEPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | loop_block {$1}
  | do_block {Call($1)}


stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

do_block:
  DO ID WITH expr_list{{ 
    fname : $2;
    formals : $4;
}}
  | DO ID {{ 
    fname : $2;
    formals : [];
}}

loop_block:
  FOR OPENPAREN expr_opt SEMI expr_opt SEMI expr_opt CLOSEPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE OPENPAREN expr CLOSEPAREN stmt { While($3, $5) }
  | OPENBLOCK stmt CLOSEBLOCK MULTIPLY NUMBER { SimpLoop( $2, $5) }
  | OPENBLOCK stmt CLOSEBLOCK UNTIL expr { Until( $2, $5 ) }


expr_list: 
  expr {$1}
  | expr_list AND expr { $3 :: $1 }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr ADD   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr MULTIPLY  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQUAL     expr { Binop($1, Equal, $3) }
  | expr NOTEQUAL  expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LTOE    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GTOE    expr { Binop($1, Geq,   $3) }
  | ID ASSIGNMENT expr   { Assignment($1, $3) }
  | ID OPENPAREN actuals_opt CLOSEPAREN { Call($1, $3) }
  | OPENPAREN expr CLOSEPAREN { $2 } 
  | BREAK { Noexpr }
  | CONTINUE { Noexpr }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
