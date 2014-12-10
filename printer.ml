(* printer.ml prints the AST for a program using the pretty print functions
 * defined in ast.ml 
 *
 * USAGE: ./printer < test_file.do *)

open Ast

let print_tree lexbuf = 
    let program = 
        Parser.program Scanner.token lexbuf
    in
        print_string (string_of_program program)

let _ =
    print_tree (Lexing.from_channel stdin)
