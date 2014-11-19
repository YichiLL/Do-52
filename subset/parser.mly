%{ open Ast %}

%token EOF
%token <string> STRING_LITERAL
%token <int> NUMBER_LITERAL
%token ADD MINUS

%left ADD MINUS

%start program
%type <Ast.program> program

%%

program: 
    expr { $1 }

expr:
      NUMBER_LITERAL { Number($1) }
    | expr ADD expr { Binop($1, Add, $3) }
    | expr MINUS expr { Binop($1, Minus, $3) }

%%

