(* compile.ml produces an AST and prints it as a java Game class.
 *
 * Usage: ./compile program.do *)

open Ast

(* ========================================================================= *)
(*                             Java Printing                                 *)
(* ========================================================================= *)
(* This is basically a reimplementation of the pretty print functions in
*  ast.ml, except now we're printing in Java's syntax rather than our own 
*  made-up "pretty" syntax. 
*
*  The resulting program will be ugly because we don't have information about
*  spacing or indentation in the AST. We could maybe keep track of depth and
*  space things that way, but it's not really worth it because nobody is meant
*  to see the java code and java ignores whitespace anyway. *)
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
    (*| Dot -> "."*)
    | Not -> "!"

let java_of_objects var =  
    if Str.string_match (Str.regexp "player[0-9]+")  var 0 then 
    "players.get(" ^ string_of_int ((int_of_string (String.sub var 6 ((String.length var) - 6 )))-1) ^ ")"
    else
    match var with
    | "size" -> "size()"
    | "top" -> "peek()"
    | _ -> var



let java_of_var var =
    match var with
    |SimpleId(str) -> str
    |DotId(str_list) -> String.concat "." (List.map (fun arg -> java_of_objects arg) str_list)

let rec java_of_expr = function
    | Number num -> string_of_int num
    | String str -> str
    | Boolean boolean ->
        if boolean then
            "true"
        else
            "false"
    | Var var -> java_of_var var
    | Unop(op, e) -> java_of_op op ^ java_of_expr e
    | Binop(e1, op, e2) -> "(" ^ java_of_expr e1 ^ " " ^ java_of_op op ^ " " ^
                           java_of_expr e2 ^ ")"

exception UnknownType of string

let java_of_type _type = 
    match _type with
    | "Number" -> "int"
    | "String" -> "String"
    | "Boolean" -> "boolean"
    | _ -> raise (UnknownType ("Type " ^ _type ^ " is not a valid type."))

(* ; not appended here, see java_of_stmt *)
(*let rec output_expr = function


    | Number num -> string_of_int num
    | String str -> "\"" ^ str ^ "\""
    | Id id -> id
    | Binop(e1, Add, e2) -> "(" ^ java_of_expr e1 ^ " " ^ java_of_op op ^ " " ^
                           java_of_expr e2 ^ ")"
    | _ -> raise (UnknownType ("Argument for output is not valid"))
*)


let rec java_of_output = function
    | Number num -> "\"" ^ string_of_int num ^"\""
    | String str -> str
    | Var var -> java_of_var var
    | Binop(e1, op, e2) -> if op !=  Add then raise (UnknownType ("Argument for output is not valid"))
    else " " ^ java_of_output e1 ^ " " ^ java_of_op op ^ " " 
    ^ java_of_output e2 ^ " "
    | _ -> raise (UnknownType ("Argument for output is not valid"))

let output_call call=
    match call.args with
    | [a]-> "System.out.println(" ^ (java_of_output a) ^ ");"
    | _ -> raise (UnknownType "Argument for input call not valid.")
    
let input_call call=
    match call.args with
    | [a]-> (java_of_expr a) ^ " = scanner.nextLine();"
    | _ -> raise (UnknownType "Argument for input call not valid.")

let normal_call call =
    let args_java =
        String.concat ", " (List.map (fun arg -> java_of_expr arg) call.args)
    in
        call.fname ^ "(" ^ args_java ^ ");"

let java_of_call call =
    match call.fname with
    | "output" -> output_call call
    | "input" -> input_call call
    | _ -> normal_call call
(* ; not appended here, see java_of_stmt *)
let java_of_update = function
    | Assign(id, e) -> java_of_var id ^ " = " ^ java_of_expr e ^ ";"
    | VarDecl(var) -> java_of_type var.var_decl_type ^ " " ^ 
                      var.var_decl_id ^ " = " ^
                      java_of_expr var.var_decl_value ^ ";"

let rec java_of_stmt stmt =
    match stmt with
    | Call call -> java_of_call call
    | Update(update) -> java_of_update update
    | If(e, tb, fb) -> "if (" ^ java_of_expr e ^ ")\n" ^ java_of_block tb
                        ^ "else\n" ^ java_of_block fb
    | While(e, b) -> "while (" ^ java_of_expr e ^ ")\n" ^ java_of_block b
    | Break -> "break;"
    | Continue -> "continue;"
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
                "Set.prepend(" ^ java_of_expr e1 ^ ", "  ^ source ^ ", " ^
                java_of_expr e2 ^ ");\n"
    | Append(e1, e2, draw_source) ->
            let source =
                match draw_source with
                | Top -> "Set.TOP"
                | Bottom -> "Set.BOTTOM"
            in
                "Set.append(" ^ java_of_expr e1 ^ ", " ^ source ^ ", " ^
                java_of_expr e2 ^ ");\n"
and java_of_block block =
    let value =
        String.concat "\n" (List.map java_of_stmt block)
    in
    "{\n" ^ value ^ "}\n"

let java_of_function func =
    let access =
        match func.decl_name with
        | "setup"
        | "round" -> "public"
        | _ -> "private"
    in let formals =
        String.concat ", " (List.map (fun formal -> 
                            " " ^ formal.formal_type ^ " " ^
                            formal.formal_id) func.formals) 
    in
        access ^ " void " ^ func.decl_name ^ "(" ^ formals ^ ")\n" ^
        java_of_block func.body 

let normal_config config = 
    match config.config_value with
    | Number(n) -> config.config_id ^ " = " ^ string_of_int n ^ ";"
    | Boolean(b) -> config.config_id ^ " = " ^ string_of_bool b ^ ";"
    | _ -> raise (UnknownType "Invalid type used for configure statement.")

let interpret_card_name name =
    match name with
    | "\"Ace\"" -> 0
    | "\"Jack\"" -> 10
    | "\"Queen\""-> 11
    | "\"King\""-> 12
    | _ -> raise (UnknownType "Invalid card name")

let interpret_card_number config =
    match config.config_value with
    |Number(n) -> config.config_id ^ " = " ^ string_of_int (n-1) ^ ";"
    |String(str) -> config.config_id ^ " = " ^ string_of_int (interpret_card_name str) ^" ;"
    |_ -> raise (UnknownType "Invalid type used for configure maxCard statement.")
(* Semantic check assures us that config_value is either a number or bool *)
let java_of_config config =
    match config.config_id with
    |"maxCard" -> interpret_card_number config
    | _ -> normal_config config


let java_of_field_decl field_decl =
    field_decl.field_type ^ " " ^ field_decl.field_id ^ ";\n"

let java_of_field_decl_assign field_decl = 
    match field_decl.field_type with
    | "Set" -> field_decl.field_id ^ " = new Set();"
    | _ -> ""

let java_of_player field_decls =
    let instance_vars = 
        String.concat "\n" (List.map java_of_field_decl field_decls)
    in let assigns =
        String.concat "\n" (List.map java_of_field_decl_assign field_decls)
    in  
        "public class MyPlayer extends Player {\n" ^
        instance_vars ^
        "public MyPlayer(String playerName) {\n"
        ^   "super(playerName);\n" ^
        assigns ^
        "}\n}"

(* Default config values yet to be implemented *)
let java_of_game program =
    let config_vars = 
        String.concat "\n" (List.map java_of_config program.configs)
    in let instance_vars =
        String.concat "\n" (List.map java_of_update program.vars)
    in let funcs =
        String.concat "\n" (List.map java_of_function program.funcs)
    in
        "import java.util.Scanner;\n" ^
        "import java.util.ArrayList;\n\n" ^
        "public class Game {\n" ^
        "ArrayList<MyPlayer> players;\n" ^
        "Scanner scanner;\n" ^
        "Set deck;\n" ^
        "int playerCount = 4;\n" ^
        "int maxCard = 12;\n" ^
        "boolean ascend = true;\n" ^
        instance_vars ^ "\n" ^
        "public Game() {\n" ^
        config_vars ^
        "scanner = new Scanner(System.in);\n" ^
        "deck = new Deck(maxCard, ascend);\n" ^
        "players = new ArrayList<MyPlayer>();\n" ^
        "for(int i = 0; i < playerCount; i++) {\n" ^
        "players.add(new MyPlayer(\"Player \" + (i+1)));\n" ^
        "}\n" ^
        "}\n\n" ^
        funcs ^
        "}"

let compile lexbuf = 
    let program = (* This is our AST *)
        Parser.program Cache.process lexbuf
    in let game_file =
        open_out "Game.java"
    in let player_file =
        open_out "MyPlayer.java"
    in
        output_string game_file (java_of_game program);
        output_string player_file (java_of_player program.field_decls)

let _ =
    let input_file = 
        open_in Sys.argv.(1)
    in
        compile (Lexing.from_channel input_file)
