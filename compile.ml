(* compile.ml produces an AST and prints it as a java Game class.
 *
 * Usage: ./compile program.do *)

open Ast

let compile lexbuf = 
    let program = (* This is our AST *)
        Parser.program Cache.process lexbuf
    in let output_file =
        open_out "Game.java"
    in
        output_string output_file (string_of_program program)

let _ =
    let input_file = 
        open_in Sys.argv.(1)
    in
        compile (Lexing.from_channel input_file)
