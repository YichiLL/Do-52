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


(* ========================================================================= *)
(*                             Java Printing                                 *)
(* ========================================================================= *)
(* This is basically a reimplementation of the pretty print functions in
*  ast.ml, except now we're printing in Java's syntax rather than our own 
*  made-up "pretty" syntax. 
*
*  The resulting program will be ugly because we don't have information about
*  spacing or indentation in the AST. But that's okay, because Java ignores
*  whitespace. *)
let java_of_op = function
    | Add -> "+"
    | Minus -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Equal -> "=="
    | NotEqual -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Ltoe -> "<="
    | Gtoe -> ">="
    | Disj -> "||"
    | Conj -> "&&"
    | Dot -> "."
    | Not -> "!"

let rec java_of_expr = function
    | Number num -> string_of_int num
    | String str -> "\"" ^ str ^ "\""
    | Boolean boolean ->
        if boolean then
            "true"
        else
            "false"
    | Id id -> id
    | Unop(op, e) -> java_of_op op ^ java_of_expr e
    | Binop(e1, op, e2) -> "(" ^ java_of_expr e1 ^ " " ^ java_of_op op ^ " " ^
                           java_of_expr e2 ^ ")"

exception UnknownType of string

let java_of_type _type = 
    match _type with
    | "Number" -> "int"
    | "String" -> "string"
    | "Boolean" -> "boolean"
    | _ -> raise UnknownType("Type " ^ _type ^ " is not a valid type.")

(* ; not appended here, see java_of_stmt *)
let java_of_call call =
    let args_java =
        String.concat ", " (List.map (fun arg -> java_of_expr arg) call.args)
    in
        call.fname ^ "(" ^ args_java ^ ")"

(* ; not appended here, see java_of_stmt *)
let java_of_update = function
    | Assign(id, e) -> id ^ " = " ^ java_of_expr e
    | VarDecl(var) -> java_of_type var._type ^ " " ^ var.id ^ " = " ^
                      java_of_expr var.value

let rec java_of_stmt = function
    let value =
        match stmt with
        | Call call -> java_of_call call
        | Update(update) -> java_of_update update
        | If(e, tb, fb) -> "if (" ^ java_of_expr e ^ ")\n" ^ java_of_block tb
                            ^ "else\n" ^ java_of_block fb
        | While(e, b) -> "while (" ^ java_of_expr e ^ ")\n" ^ java_of_block b
        | Break -> "break"
        | Continue -> "continue"
        | For(a, e, u, b) -> "for (" ^ java_of_update a ^ "; " ^ java_of_expr e
                             ^ "; " ^ java_of_update u ^ ")\n" ^ 
                             java_of_block b
        | TimesLoop(stmt, expr) -> "for (int i = 0; i < " ^ java_of_expr expr ^
                                    "; i++)\n{\n" ^ java_of_stmt stmt ^ "\n}\n"
        | Prepend(e1, e2, draw_source) ->
                let source =
                    match draw_source with
                    | Top -> "Set.TOP"
                    | Bottom -> "Set.BOTTOM"
                in
                    "prepend(" ^ java_of_expr e1 ^ ", "  ^ source ^ ", " ^
                    java_of_expr e2 ^ ")"
        | Appened(e1, e2, draw_source) ->
                let source =
                    match draw_source with
                    | Top -> "Set.TOP"
                    | Bottom -> "Set.BOTTOM"
                in
                    "append(" ^ java_of_expr e1 ^ ", " ^ source ", " ^
                    java_of_expr e2 ^ ")"
    in
        value ^ ";"
and java_of_block block =
    let value =
        String.concat "\n" (List.map java_of_stmt block)
    in
        "{\n" ^ value ^ "}\n"
