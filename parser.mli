type token =
  | COMMENT
  | NEWLINE
  | SEMI
  | OPENPAREN
  | CLOSEPAREN
  | OPENBLOCK
  | CLOSEBLOCK
  | COMMA
  | PREPEN
  | APPEND
  | ADD
  | MINUS
  | MULTIPLY
  | DIVIDE
  | ASSIGNMENT
  | EQUAL
  | NOTEQUAL
  | LT
  | LTOE
  | GT
  | GTOE
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | STRING
  | BOOLEAN
  | CARD
  | SET
  | PLAYER
  | UNTIL
  | BREAK
  | CONTINUE
  | HAS
  | CALLED
  | DO
  | WITH
  | NEW
  | CONFIG
  | LITERAL of (string)
  | NUMBER of (int)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
