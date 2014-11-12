type token =
  | COMMENT
  | NEWLINE
  | TAB
  | OR
  | AND
  | NOT
  | DOTOP
  | PREPEND
  | APREND
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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 59 "parser.ml"
let yytransl_const = [|
  257 (* COMMENT *);
  258 (* NEWLINE *);
  259 (* TAB *);
  260 (* OR *);
  261 (* AND *);
  262 (* NOT *);
  263 (* DOTOP *);
  264 (* PREPEND *);
  265 (* APREND *);
  266 (* SEMI *);
  267 (* OPENPAREN *);
  268 (* CLOSEPAREN *);
  269 (* OPENBLOCK *);
  270 (* CLOSEBLOCK *);
  271 (* COMMA *);
  272 (* PREPEN *);
  273 (* APPEND *);
  274 (* ADD *);
  275 (* MINUS *);
  276 (* MULTIPLY *);
  277 (* DIVIDE *);
  278 (* ASSIGNMENT *);
  279 (* EQUAL *);
  280 (* NOTEQUAL *);
  281 (* LT *);
  282 (* LTOE *);
  283 (* GT *);
  284 (* GTOE *);
  285 (* IF *);
  286 (* ELSE *);
  287 (* FOR *);
  288 (* WHILE *);
  289 (* INT *);
  290 (* STRING *);
  291 (* BOOLEAN *);
  292 (* CARD *);
  293 (* SET *);
  294 (* PLAYER *);
  295 (* UNTIL *);
  296 (* BREAK *);
  297 (* CONTINUE *);
  298 (* HAS *);
  299 (* CALLED *);
  300 (* DO *);
  301 (* WITH *);
  302 (* NEW *);
  303 (* CONFIG *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  304 (* LITERAL *);
  305 (* NUMBER *);
  306 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\007\000\007\000\004\000\
\004\000\008\000\008\000\008\000\009\000\009\000\010\000\010\000\
\005\000\005\000\002\000\011\000\013\000\013\000\013\000\013\000\
\013\000\013\000\006\000\006\000\015\000\015\000\014\000\014\000\
\014\000\014\000\016\000\016\000\017\000\017\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\018\000\
\018\000\019\000\019\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\006\000\004\000\000\000\001\000\001\000\
\003\000\002\000\002\000\002\000\000\000\001\000\001\000\003\000\
\000\000\002\000\002\000\004\000\001\000\003\000\005\000\007\000\
\001\000\001\000\000\000\002\000\004\000\002\000\009\000\005\000\
\005\000\005\000\001\000\003\000\000\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\003\000\001\000\001\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\002\000\003\000\019\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\018\000\000\000\010\000\011\000\012\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\054\000\055\000\000\000\039\000\
\000\000\000\000\028\000\025\000\026\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\053\000\022\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\043\000\044\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\000\000\033\000\000\000\023\000\000\000\032\000\000\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\031\000"

let yydgoto = "\002\000\
\003\000\017\000\007\000\015\000\011\000\018\000\000\000\016\000\
\000\000\000\000\000\000\034\000\035\000\036\000\037\000\088\000\
\065\000\069\000\070\000"

let yysindex = "\010\000\
\000\000\000\000\229\254\221\254\251\254\000\000\000\000\000\000\
\000\000\012\255\016\255\004\255\006\255\007\255\005\255\000\000\
\000\000\061\255\000\000\000\000\000\000\012\255\000\000\003\255\
\061\255\047\255\049\255\051\255\000\000\000\000\013\255\000\000\
\252\254\143\255\000\000\000\000\000\000\000\000\016\255\095\255\
\056\255\054\255\003\255\003\255\003\255\020\255\003\255\003\255\
\003\255\003\255\003\255\003\255\003\255\003\255\003\255\003\255\
\003\255\003\255\061\255\000\000\000\000\238\254\113\255\143\255\
\065\255\130\255\003\255\143\255\064\255\066\255\143\255\255\254\
\255\254\000\000\000\000\154\255\154\255\165\255\108\255\165\255\
\108\255\030\255\003\255\061\255\003\255\061\255\143\255\077\255\
\000\000\003\255\000\000\143\255\000\000\074\255\000\000\003\255\
\143\255\061\255\003\255\143\255\000\000\079\255\061\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\086\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\002\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\075\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\186\001\000\000\000\000\000\000\000\000\033\002\000\000\
\000\000\000\000\000\000\084\255\000\000\210\001\083\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\040\255\
\000\000\000\000\000\000\253\254\000\000\086\255\117\001\042\000\
\083\000\000\000\000\000\035\001\076\001\206\000\124\000\247\000\
\165\000\000\000\000\000\000\000\084\255\000\000\140\001\234\001\
\000\000\000\000\000\000\010\002\000\000\000\000\000\000\000\000\
\001\255\000\000\087\255\163\001\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\100\000\000\000\000\000\085\000\239\255\000\000\088\000\
\000\000\000\000\000\000\237\255\231\255\000\000\000\000\000\000\
\212\255\000\000\000\000"

let yytablesize = 851
let yytable = "\042\000\
\040\000\082\000\005\000\004\000\040\000\004\000\047\000\041\000\
\058\000\022\000\001\000\058\000\059\000\024\000\008\000\059\000\
\009\000\048\000\051\000\052\000\083\000\059\000\005\000\063\000\
\064\000\066\000\023\000\068\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\079\000\080\000\081\000\010\000\
\094\000\041\000\029\000\030\000\012\000\013\000\014\000\087\000\
\004\000\038\000\032\000\038\000\033\000\019\000\102\000\020\000\
\021\000\043\000\093\000\044\000\095\000\045\000\046\000\092\000\
\067\000\064\000\024\000\062\000\025\000\061\000\097\000\024\000\
\101\000\025\000\085\000\089\000\100\000\104\000\091\000\064\000\
\090\000\096\000\042\000\099\000\026\000\060\000\027\000\028\000\
\027\000\026\000\103\000\027\000\028\000\037\000\056\000\029\000\
\030\000\057\000\037\000\031\000\029\000\030\000\006\000\032\000\
\031\000\033\000\060\000\039\000\032\000\038\000\033\000\000\000\
\049\000\050\000\051\000\052\000\000\000\053\000\054\000\055\000\
\056\000\057\000\058\000\048\000\084\000\049\000\050\000\051\000\
\052\000\000\000\049\000\050\000\051\000\052\000\000\000\053\000\
\054\000\055\000\056\000\057\000\058\000\086\000\000\000\000\000\
\000\000\000\000\000\000\049\000\050\000\051\000\052\000\000\000\
\053\000\054\000\055\000\056\000\057\000\058\000\000\000\000\000\
\049\000\050\000\051\000\052\000\050\000\053\000\054\000\055\000\
\056\000\057\000\058\000\049\000\050\000\051\000\052\000\000\000\
\000\000\000\000\055\000\056\000\057\000\058\000\049\000\050\000\
\051\000\052\000\000\000\000\000\000\000\000\000\056\000\000\000\
\058\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\
\000\000\000\000\040\000\000\000\040\000\040\000\040\000\040\000\
\000\000\000\000\040\000\040\000\040\000\040\000\000\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\045\000\005\000\004\000\000\000\000\000\000\000\
\040\000\040\000\000\000\000\000\040\000\000\000\041\000\000\000\
\040\000\000\000\040\000\041\000\041\000\041\000\041\000\041\000\
\041\000\000\000\000\000\041\000\041\000\000\000\000\000\000\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\046\000\000\000\000\000\000\000\000\000\
\000\000\041\000\041\000\000\000\000\000\041\000\000\000\042\000\
\000\000\041\000\000\000\041\000\042\000\042\000\042\000\042\000\
\042\000\042\000\000\000\000\000\042\000\042\000\000\000\000\000\
\000\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\051\000\000\000\000\000\000\000\
\000\000\000\000\042\000\042\000\000\000\000\000\042\000\000\000\
\048\000\000\000\042\000\000\000\042\000\048\000\048\000\048\000\
\048\000\048\000\048\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\000\000\000\000\000\000\
\000\000\000\000\036\000\048\000\048\000\000\000\000\000\048\000\
\000\000\050\000\000\000\048\000\000\000\048\000\050\000\050\000\
\050\000\050\000\050\000\050\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\000\000\000\000\
\000\000\000\000\000\000\000\000\050\000\050\000\000\000\000\000\
\050\000\030\000\047\000\000\000\050\000\000\000\050\000\047\000\
\047\000\047\000\047\000\047\000\047\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\047\000\047\000\000\000\
\047\000\029\000\047\000\047\000\047\000\047\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\047\000\000\000\
\000\000\047\000\000\000\049\000\000\000\047\000\000\000\047\000\
\049\000\049\000\049\000\049\000\049\000\049\000\000\000\000\000\
\000\000\034\000\000\000\000\000\000\000\049\000\049\000\049\000\
\000\000\049\000\000\000\049\000\049\000\049\000\049\000\049\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\049\000\
\027\000\000\000\049\000\000\000\000\000\000\000\049\000\045\000\
\049\000\000\000\000\000\000\000\045\000\045\000\045\000\045\000\
\045\000\045\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\045\000\000\000\000\000\000\000\000\000\045\000\
\045\000\045\000\045\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\045\000\045\000\000\000\000\000\045\000\000\000\
\046\000\000\000\045\000\000\000\045\000\046\000\046\000\046\000\
\046\000\046\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000\046\000\000\000\000\000\000\000\000\000\
\046\000\046\000\046\000\046\000\046\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\000\046\000\000\000\000\000\046\000\
\000\000\051\000\000\000\046\000\000\000\046\000\051\000\051\000\
\051\000\051\000\051\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\000\051\000\051\000\051\000\051\000\051\000\035\000\000\000\
\035\000\035\000\000\000\000\000\051\000\051\000\000\000\000\000\
\051\000\000\000\000\000\000\000\051\000\000\000\051\000\036\000\
\035\000\035\000\035\000\035\000\035\000\036\000\000\000\036\000\
\036\000\000\000\000\000\035\000\035\000\000\000\000\000\035\000\
\000\000\000\000\000\000\035\000\000\000\035\000\000\000\036\000\
\036\000\036\000\036\000\036\000\021\000\000\000\021\000\021\000\
\000\000\000\000\036\000\036\000\000\000\000\000\036\000\000\000\
\000\000\000\000\036\000\000\000\036\000\000\000\021\000\021\000\
\021\000\021\000\021\000\000\000\030\000\000\000\030\000\030\000\
\000\000\021\000\021\000\000\000\000\000\021\000\000\000\000\000\
\000\000\021\000\000\000\021\000\000\000\000\000\030\000\030\000\
\030\000\030\000\030\000\000\000\029\000\000\000\029\000\029\000\
\000\000\030\000\030\000\000\000\000\000\030\000\000\000\000\000\
\000\000\030\000\000\000\030\000\000\000\000\000\029\000\029\000\
\029\000\029\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\029\000\000\000\034\000\029\000\034\000\034\000\
\000\000\029\000\000\000\029\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\034\000\034\000\
\034\000\034\000\034\000\027\000\000\000\027\000\000\000\000\000\
\000\000\034\000\034\000\000\000\000\000\034\000\000\000\000\000\
\000\000\034\000\000\000\034\000\000\000\027\000\000\000\027\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\027\000\000\000\000\000\027\000\000\000\000\000\000\000\
\027\000\000\000\027\000"

let yycheck = "\025\000\
\000\000\020\001\000\000\000\000\024\000\033\001\011\001\025\000\
\012\001\005\001\001\000\015\001\012\001\011\001\050\001\015\001\
\022\001\022\001\020\001\021\001\039\001\039\000\050\001\043\000\
\044\000\045\000\022\001\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\045\001\
\085\000\000\000\040\001\041\001\033\001\034\001\035\001\067\000\
\033\001\010\001\048\001\012\001\050\001\050\001\099\000\050\001\
\050\001\011\001\084\000\011\001\086\000\011\001\050\001\083\000\
\045\001\085\000\011\001\014\001\013\001\014\001\090\000\011\001\
\098\000\013\001\010\001\012\001\096\000\103\000\049\001\099\000\
\015\001\005\001\000\000\010\001\029\001\000\000\031\001\032\001\
\014\001\029\001\012\001\031\001\032\001\010\001\012\001\040\001\
\041\001\012\001\012\001\044\001\040\001\041\001\003\000\048\001\
\044\001\050\001\012\001\023\000\048\001\022\000\050\001\255\255\
\018\001\019\001\020\001\021\001\255\255\023\001\024\001\025\001\
\026\001\027\001\028\001\000\000\012\001\018\001\019\001\020\001\
\021\001\255\255\018\001\019\001\020\001\021\001\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\012\001\255\255\255\255\
\255\255\255\255\255\255\018\001\019\001\020\001\021\001\255\255\
\023\001\024\001\025\001\026\001\027\001\028\001\255\255\255\255\
\018\001\019\001\020\001\021\001\000\000\023\001\024\001\025\001\
\026\001\027\001\028\001\018\001\019\001\020\001\021\001\255\255\
\255\255\255\255\025\001\026\001\027\001\028\001\018\001\019\001\
\020\001\021\001\255\255\255\255\255\255\255\255\026\001\255\255\
\028\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\255\255\255\255\
\255\255\255\255\010\001\255\255\012\001\013\001\014\001\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\000\000\033\001\033\001\255\255\255\255\255\255\
\040\001\041\001\255\255\255\255\044\001\255\255\005\001\255\255\
\048\001\255\255\050\001\010\001\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\018\001\019\001\255\255\255\255\255\255\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\000\000\255\255\255\255\255\255\255\255\
\255\255\040\001\041\001\255\255\255\255\044\001\255\255\005\001\
\255\255\048\001\255\255\050\001\010\001\011\001\012\001\013\001\
\014\001\015\001\255\255\255\255\018\001\019\001\255\255\255\255\
\255\255\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\000\000\255\255\255\255\255\255\
\255\255\255\255\040\001\041\001\255\255\255\255\044\001\255\255\
\005\001\255\255\048\001\255\255\050\001\010\001\011\001\012\001\
\013\001\014\001\015\001\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\255\255\000\000\040\001\041\001\255\255\255\255\044\001\
\255\255\005\001\255\255\048\001\255\255\050\001\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\255\255\255\255\
\255\255\255\255\255\255\255\255\040\001\041\001\255\255\255\255\
\044\001\000\000\005\001\255\255\048\001\255\255\050\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\023\001\024\001\025\001\255\255\
\027\001\000\000\029\001\030\001\031\001\032\001\033\001\255\255\
\255\255\255\255\255\255\255\255\255\255\040\001\041\001\255\255\
\255\255\044\001\255\255\005\001\255\255\048\001\255\255\050\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\023\001\024\001\025\001\
\255\255\027\001\255\255\029\001\030\001\031\001\032\001\033\001\
\255\255\255\255\255\255\255\255\255\255\255\255\040\001\041\001\
\000\000\255\255\044\001\255\255\255\255\255\255\048\001\005\001\
\050\001\255\255\255\255\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\023\001\024\001\255\255\255\255\255\255\255\255\029\001\
\030\001\031\001\032\001\033\001\255\255\255\255\255\255\255\255\
\255\255\255\255\040\001\041\001\255\255\255\255\044\001\255\255\
\005\001\255\255\048\001\255\255\050\001\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\023\001\024\001\255\255\255\255\255\255\255\255\
\029\001\030\001\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\255\255\255\255\040\001\041\001\255\255\255\255\044\001\
\255\255\005\001\255\255\048\001\255\255\050\001\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\005\001\029\001\030\001\031\001\032\001\033\001\011\001\255\255\
\013\001\014\001\255\255\255\255\040\001\041\001\255\255\255\255\
\044\001\255\255\255\255\255\255\048\001\255\255\050\001\005\001\
\029\001\030\001\031\001\032\001\033\001\011\001\255\255\013\001\
\014\001\255\255\255\255\040\001\041\001\255\255\255\255\044\001\
\255\255\255\255\255\255\048\001\255\255\050\001\255\255\029\001\
\030\001\031\001\032\001\033\001\011\001\255\255\013\001\014\001\
\255\255\255\255\040\001\041\001\255\255\255\255\044\001\255\255\
\255\255\255\255\048\001\255\255\050\001\255\255\029\001\030\001\
\031\001\032\001\033\001\255\255\011\001\255\255\013\001\014\001\
\255\255\040\001\041\001\255\255\255\255\044\001\255\255\255\255\
\255\255\048\001\255\255\050\001\255\255\255\255\029\001\030\001\
\031\001\032\001\033\001\255\255\011\001\255\255\013\001\014\001\
\255\255\040\001\041\001\255\255\255\255\044\001\255\255\255\255\
\255\255\048\001\255\255\050\001\255\255\255\255\029\001\030\001\
\031\001\032\001\033\001\255\255\255\255\255\255\255\255\255\255\
\255\255\040\001\041\001\255\255\011\001\044\001\013\001\014\001\
\255\255\048\001\255\255\050\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\029\001\030\001\
\031\001\032\001\033\001\011\001\255\255\013\001\255\255\255\255\
\255\255\040\001\041\001\255\255\255\255\044\001\255\255\255\255\
\255\255\048\001\255\255\050\001\255\255\029\001\255\255\031\001\
\032\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\040\001\041\001\255\255\255\255\044\001\255\255\255\255\255\255\
\048\001\255\255\050\001"

let yynames_const = "\
  COMMENT\000\
  NEWLINE\000\
  TAB\000\
  OR\000\
  AND\000\
  NOT\000\
  DOTOP\000\
  PREPEND\000\
  APREND\000\
  SEMI\000\
  OPENPAREN\000\
  CLOSEPAREN\000\
  OPENBLOCK\000\
  CLOSEBLOCK\000\
  COMMA\000\
  PREPEN\000\
  APPEND\000\
  ADD\000\
  MINUS\000\
  MULTIPLY\000\
  DIVIDE\000\
  ASSIGNMENT\000\
  EQUAL\000\
  NOTEQUAL\000\
  LT\000\
  LTOE\000\
  GT\000\
  GTOE\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  STRING\000\
  BOOLEAN\000\
  CARD\000\
  SET\000\
  PLAYER\000\
  UNTIL\000\
  BREAK\000\
  CONTINUE\000\
  HAS\000\
  CALLED\000\
  DO\000\
  WITH\000\
  NEW\000\
  CONFIG\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  NUMBER\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
                 ( [], [] )
# 474 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 35 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 482 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 36 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 490 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'arg_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 40 "parser.mly"
   ( { 
      fname = _1;
      formals = _3;
      locals = List.rev _5;
      body = List.rev _6
    } )
# 505 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 47 "parser.mly"
  ( {
     fname = _1;
     formals = [];
     locals = List.rev _3;
     body = List.rev _4
  } )
# 519 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                 ( [] )
# 525 "parser.ml"
               : 'arg_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_list) in
    Obj.repr(
# 56 "parser.mly"
             ( List.rev _1)
# 532 "parser.ml"
               : 'arg_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 59 "parser.mly"
      ( [_1] )
# 539 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 60 "parser.mly"
                     ( _3 :: _1)
# 547 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
        ( [ _2 ] )
# 554 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
             ( [ _2 ] )
# 561 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
              ( [ _2 ] )
# 568 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                  ( [] )
# 574 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 69 "parser.mly"
                  ( List.rev _1 )
# 581 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                         ( [_1] )
# 588 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
                         ( _3 :: _1 )
# 596 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                     ( [] )
# 602 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 77 "parser.mly"
                     ( _2 :: _1 )
# 610 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
         ( _2 )
# 617 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                            (Config(_2,_4))
# 625 "parser.ml"
               : 'condecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
         ( Expr(_1) )
# 632 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 87 "parser.mly"
                                   ( Block(List.rev _2) )
# 639 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 88 "parser.mly"
                                                   ( If(_3, _5, Block([])) )
# 647 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 89 "parser.mly"
                                                   ( If(_3, _5, _7) )
# 656 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'loop_block) in
    Obj.repr(
# 90 "parser.mly"
               (_1)
# 663 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'do_block) in
    Obj.repr(
# 91 "parser.mly"
             (_1)
# 670 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                   ( [] )
# 676 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 96 "parser.mly"
                   ( _2 :: _1 )
# 684 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 99 "parser.mly"
                      ({ 
                    fname = _2;
                    formals = List.rev _4
                  })
# 695 "parser.ml"
               : 'do_block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "parser.mly"
          ({
            fname = _2;
            formal = [] 
          })
# 705 "parser.ml"
               : 'do_block))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 110 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 715 "parser.ml"
               : 'loop_block))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 111 "parser.mly"
                                         ( While(_3, _5) )
# 723 "parser.ml"
               : 'loop_block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'stmt) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
                                              ( SimpLoop( _2, _5) )
# 731 "parser.ml"
               : 'loop_block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'stmt) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                                         ( Until( _2, _5 ) )
# 739 "parser.ml"
               : 'loop_block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
       ( [_1] )
# 746 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                       ( _3 :: _1 )
# 754 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
                  ( Noexpr )
# 760 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                  ( _1 )
# 767 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 125 "parser.mly"
                     ( Literal(_1) )
# 774 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "parser.mly"
                     ( Id(_1) )
# 781 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                    ( Binop(_1, Add,   _3) )
# 789 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 797 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                        ( Binop(_1, Mult,  _3) )
# 805 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 813 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                        ( Binop(_1, Equal, _3) )
# 821 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                        ( Binop(_1, Neq,   _3) )
# 829 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 837 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                      ( Binop(_1, Leq,   _3) )
# 845 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 853 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                      ( Binop(_1, Geq,   _3) )
# 861 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                         ( Assignment(_1, _3) )
# 869 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 138 "parser.mly"
                                        ( Call(_1, _3) )
# 877 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                              ( _2 )
# 884 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
          ( Noexpr )
# 890 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
             ( Noexpr )
# 896 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
                  ( [] )
# 902 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 145 "parser.mly"
                  ( List.rev _1 )
# 909 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                            ( [_1] )
# 916 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                            ( _3 :: _1 )
# 924 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program) 