(*Scanner.mll
Scanner generates tokens for the parser*)

{ open Parser }

rule token = parse
| ' ' { token lexbuf }
| '\n' { token lexbuf }
| '+' { ADD }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '<' { LT }
| '>' { GT }
| "<=" { LTOE }
| ">=" { GTOE }
| '=' { EQUAL }
| "!=" { NOTEQUAL}
| '(' { OPENPAREN }
| ')' { CLOSEPAREN }
| ['0'-'9']+ as number { NUMBER_LITERAL(int_of_string(number)) } 
| '\"'[^'\"']* '\"' as lit { STRING_LITERAL(lit) } 
| eof { EOF }
