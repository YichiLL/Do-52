(* scanner.mll groups characters read from input into tokens that are then
 * passed to the parser *)

{ 
    open Parser
    open Indent
    open Printf

    (* Persistent reference cell counter for the current indent depth. *)
    let cur_depth = ref 0

    (* This function returns INDENT or DEDENT tokens whenever we change depth.
     * DEDENT means we've reached the end of a block. INDENT means we've
     * entered one. In OCaml ";" has lower precedence than "if", hence all
     * the begins and ends. *)
    let eval_indent str =
        let depth = 
            Indent.depth_count (Indent.explode str)
        in
            if depth < !cur_depth then begin
                cur_depth := depth;
                DEDENT 
            end
            else if depth == !cur_depth then
                NEWLINE 
            else begin
                cur_depth := depth; 
                INDENT 
            end
}

rule token = parse
(* White Space *)
| ' '                           { token lexbuf }
| '\n'[' ''\t']* as str         { eval_indent str }
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
| "."                           { DOT }
| "!"                           { NOT }
| "t>"                          { PREPEND_TOP }
| "b>"                          { PREPEND_BOTTOM }
| "<t"                          { APPEND_TOP }
| "<b"                          { APPEND_BOTTOM }

(* Variables *)
| "new"                         { NEW }
| "configure"                   { CONFIGURE }

(* Functions *)
| "do"                          { DO }
| "with"                        { WITH }
| "and"                         { AND }

(* Control Flow *)
| '('                           { OPENPAREN }
| ')'                           { CLOSEPAREN }
| '{'                           { OPENBRACE }
| '}'                           { CLOSEBRACE }
| "if"                          { IF }
| "else"                        { ELSE } 
| "while"                       { WHILE }
| "for"                         { FOR }
| "break"                       { BREAK }
| "continue"                    { CONTINUE }

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
| ":"                           { COLON }
| ";"                           { SEMI }

