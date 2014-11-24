(* scanner.mll groups characters read from input into tokens that are then
 * passed to the parser *)

{ open Parser }

rule token = parse
(* White Space *)
| ' '                           { token lexbuf }
| '\n'                          { NEWLINE }
| eof                           { EOF }

(* Operators *)
| '+'                           { ADD }
| '-'                           { MINUS }
| '*'                           { TIMES }
| '/'                           { DIVIDE }
| '<'                           { LT }
| '>'                           { GT }
| "<="                          { LTOE }
| ">="                          { GTOE }
| '='                           { EQUAL }
| "!="                          { NOTEQUAL}
| "|"                           { DISJ }   (* i.e. disjunct *)
| "&"                           { CONJ }   (* i.e. conjunct *)
| "!"                           { NOT }

(* Functions *)
| "do"                          { DO }
| "with"                        { WITH }
| "and"                         { AND }

(* Grouping and Blocks *)
| '('                           { OPENPAREN }
| ')'                           { CLOSEPAREN }

(* Literals *)
| ['0'-'9']+ as num             { NUMBER_LITERAL(int_of_string(num)) } 
| '\"'[^'\"']* '\"' as str      { STRING_LITERAL(str) } 
| "true"                        { BOOL_LITERAL(true) }
| "false"                       { BOOL_LITERAL(false) }

(* Miscellaneous *)
(* IDs can be any letter followed by a combination of numbers and letters,
 * but no underscores *)
| ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9']* as id       { ID(id) }
(* Comments *)
| "//"[^'\n']*'\n'              { token lexbuf }
