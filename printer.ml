(* printer.ml prints the AST for a program using the pretty print functions
 * defined in ast.ml 
 *
 * USAGE: ./printer test_file.do *)

open Ast

let print_tree lexbuf = 
    let program = 
        Parser.program Cache.process lexbuf
    in
        print_string (string_of_program program)

let _ =
    let file = 
        open_in Sys.argv.(1) 
    in
        print_tree (Lexing.from_channel file)
