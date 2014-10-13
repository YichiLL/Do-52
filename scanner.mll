(*Scanner.mll
Scanner generates tokens for the parser*)

{ open Parser }

rule token = parse
| ' ' {SPACE}
| '\t' {TAB}
| '\n' {NEWLINE}
| '+' {ADD}
| '-' {MINUS}
| '*' {MULTIPLY}
| '/' {DIVIDE}
| '=' {EQUAL}
| "!=" {NOTEQUAL}
| '<' {LT}
| '>' {GT}
| "<=" {LTOE}
| ">=" {GTOE}
| '|' {OR}
| '&' {AND}
| '!' {NOT}
| ':' {ASSIGNMENT}
| '_' {DOTOP}
| ">>" {PREPEND}
| "<<" {APPEND}
| '"' {QUOTE}          (* might be problematic *)
| '\\' {BACKSLASH}    (* might be problematic *)
| "Number" {INT}
| "String" {STRING}
| "boolean" {BOOLEAN}
| "Card" {CARD}
| "Set" {SET}
| "Player" {PLAYER}
| "if" {IF}
| "else" {ELSE}
| "else if" {ELSEIF}
| '{' {OPENBLOCK}
| '}' {CLOSEBLOCK}
| "while" {WHILE}
| "for" {FOR}
| "until" {UNTIL}
| "break" {BREAK}
| "continue" {CONTINUE}
| '(' {OPENPARENT}
| ')' {CLOSEPARENT}
| "has" {HAS}
| "called" {CALLED}
| "do" {DO}
| "with" {WITH}
| "new" {NEW}
| "configure" {CONFIG}
| ['A'-'Z''a'-'z'] ['a'-'z''A'-'Z''0'-'9''_']* as lxm {ID(lxm)}
| ['0'-'9']+ as digit { DIGIT(digit) } 
| '\"'[^'\"']* '\"' as lit {LITERAL (lit)} 
| eof { EOF }
| "//" {comment lexbuf}

and comment = parse
| '\n' {token lexbuf}
| _ {comment lexbuf}  