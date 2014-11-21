%{ open Ast %}

%token EOF
%token <string> STRING_LITERAL
%token <int> NUMBER_LITERAL
%token ADD MINUS TIMES DIVIDE
%token LT LTOE GT GTOE EQUAL NOTEQUAL
%token OPENPAREN CLOSEPAREN 


%left EQUAL NOTEQUAL
%left LT LTOE GT GTOE
%left ADD MINUS
%left TIMES DIVIDE



%start program
%type <Ast.program> program

%%

program: 
    expr { $1 }

expr:
      NUMBER_LITERAL { Number($1) }
    | expr ADD expr { Binop($1, Add, $3) }
    | expr MINUS expr { Binop($1, Minus, $3) }
    | expr TIMES expr { Binop($1, Multiply, $3) }
    | expr DIVIDE expr { Binop($1, Divide, $3) }
    | expr LT expr { Binop($1, Lt, $3) }
    | expr LTOE expr { Binop($1, Ltoe, $3) }
    | expr GT expr { Binop($1, Gt, $3) }
    | expr GTOE expr { Binop($1, Gtoe, $3) }
    | expr EQUAL expr { Binop($1, Equal, $3) }
    | expr NOTEQUAL expr { Binop($1, Notequal, $3) }
    | OPENPAREN expr CLOSEPAREN { $2 }

%%

