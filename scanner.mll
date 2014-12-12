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
     * the begins and ends. 
     *
     * If we dedent more than 1 level, we need to produce a token that holds
     * the number of levels we have dedented. This gets turned into multiple
     * dedent tokens later. *)
    let eval_indent str =
        let depth = 
            Indent.depth_count 0 (Indent.explode str)
        in
            if depth < !cur_depth then begin
                let diff = !cur_depth - depth in
                cur_depth := depth;

                if diff > 1 then
                    DEDENT_MULT(diff)
                else
                    DEDENT
            end
            else if depth == !cur_depth then
                NEWLINE 
            else begin
                cur_depth := depth; 
                INDENT 
            end
}

(* Complicated Regexes *)
let rgx_indent = ('\n'[' ''\t']*("//"[^'\n']*)*)+
let rgx_id = ['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*

rule token = parse
(* White Space and Comments *)
| [' ''\t']                     { token lexbuf }
| rgx_indent as str             { eval_indent str }
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
| "t>"                          { PREPEND_TOP }
| "b>"                          { PREPEND_BOTTOM }
| "<t"                          { APPEND_TOP }
| "<b"                          { APPEND_BOTTOM }

(* Variables *)
| "new"                         { NEW }
| "configure"                   { CONFIGURE }
| "has"                         { HAS }
| "called"                      { CALLED }

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

(* -------------- Miscellaneous ------------ *)
(* IDs can be any lowercase letter followed by a combination of numbers,
 * letters, or underscores. *)
| rgx_id as id                  { ID(id) }
| ((rgx_id)'.')+(rgx_id) as id      { DOT_ID(id) }

(* Type IDs can be an uppecase letter followed by a combination of letters. *)
| ['A'-'Z']['A'-'Z''a'-'z']* as _type              { TYPE(_type) }

(* This triggered if comment starts a program. Otherwise comments taken care of
 * in rgx_indent. *)
| "//"[^'\n']*                  { token lexbuf }

(* Punctuation *)
| ":"                           { COLON }
| ";"                           { SEMI }
