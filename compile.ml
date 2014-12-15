(* compile.ml produces an AST and prints it as a java Game class.
 *
 * Usage: ./compile program.do *)

open Ast
open Sast

(* ========================================================================= *)
(*                             Helper Functions                              *)
(* ========================================================================= *)
(* Returns the java code for a stdlib var. *)
let check_std_lib_for_var var_id =
    let var_decl, java_code =
        try
            List.find (fun (vdecl, java) -> (vdecl.var_decl_id = var_id)) 
                        Stdlib.vars
        with Not_found ->
            var_id
    in
        java_code

(* ========================================================================= *)
(*                             Java Printing                                 *)
(* ========================================================================= *)
(* This is basically a reimplementation of the pretty print functions in
*  ast.ml, except now we're printing in Java's syntax rather than our own 
*  made-up "pretty" syntax. 
*
*  The resulting program will be ugly because we don't have information about
*  spacing or indentation in the SAST. We could maybe keep track of depth and
*  space things that way, but it's not really worth it because nobody is meant
*  to see the java code and java ignores whitespace anyway. *)
(* Converts a type to a string representation of a java type. *)
let java_of_type = function
    | BooleanType -> "boolean"
    | NumberType -> "int"
    | StringType -> "String"
    | CardType -> "Card"
    | SetType -> "Set"
    | PlayerType -> "MyPlayer"

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
    | Not -> "!"

(* Converts a simple expression in our SAST to java code, checking the stdlib
 * for special java code representations. *)
let rec java_of_simple_expr expr =
    let simple, _type = expr in 
    match simple with
    | Number num -> string_of_int num
    | String str -> str
    | Boolean boolean ->
        if boolean then
            "true"
        else
            "false"
    | Var(var) -> check_std_lib_for_var var
    | Unop(op, e) -> java_of_op op ^ java_of_simple_expr e
    | Binop(e1, op, e2) -> 
           (*assumes that logical operators on set will be regarding set size*)
        begin match op with
        |Disj | Conj ->  "(" ^ java_of_simple_expr e1 ^ " " ^
                        java_of_op op  ^ " " ^ java_of_simple_expr e2  ")"
        | _ -> begin match e1 with
        |( _ , StringType) -> begin match op with
            | Equal ->  "(Utility.compareString(" ^ java_of_simple_expr e1  ^ 
                    ", " ^ java_of_simple_expr e2  ")"
            | NotEqual -> "(!Utility.compareString(" ^ java_of_simple_expr e1  ^ 
                    ", " ^ java_of_simple_expr e2  ")"
            | Add ->  java_of_simple_expr e1 ^ " " ^
                        java_of_op op  ^ " " ^ java_of_simple_expr e2 
        |( _ , NumberType) -> "(" ^ java_of_simple_expr e1 ^ " " ^
                        java_of_op op  ^ " " ^ java_of_simple_expr e2 ^ ")"
        | ( _ , CardType) -> begin match op with
            | Equal ->   "(Utility.cardEqual(" ^ java_of_simple_expr e1  ^ 
                    ", " ^ java_of_simple_expr e2  ")"
            | NotEqual ->  "(Utility.cardNotEqual(" ^ java_of_simple_expr e1  ^ 
                    ", " ^ java_of_simple_expr e2  ")"
            | Lt ->   "(Utility.cardLessThan(" ^ java_of_simple_expr e1  ^ 
                    ", " ^ java_of_simple_expr e2  ")"
            | Gt ->  "(Utility.cardGreaterThan(" ^ java_of_simple_expr e1  ^ 
                    ", " ^ java_of_simple_expr e2  ")"
            | Ltoe -> "(Utility.cardLessOrEqualThan(" ^ java_of_simple_expr e1  ^ 
                    ", " ^ java_of_simple_expr e2  ")"
            | Gtoe -> "(Utility.cardGreaterOrEqualThan(" ^ java_of_simple_expr e1  ^ 
                    ", " ^ java_of_simple_expr e2  ")"
            | _ -> raise (WrongType ("You can't have operation" ^ java_of_op op ^ " with type Card"))
        | ( _ , SetType) -> begin match op with
            | Equal | NotEqual | Lt | Gt | Ltoe | Gtoe ->  
             "(" ^ java_of_simple_expr e1 ^ " " ^
                        java_of_op op  ^ " " ^ java_of_simple_expr e2 ^ ")"
            
            | _ -> raise (WrongType ("You can't have operation" ^ java_of_op op ^ " with type Card"))
    


        

(* Converts a config_decl to a java assignment. Only numbers, booleans, or
 * variables can be used for configure statements. *)
let java_of_config config = 
    match config.config_value with
    | Number(n) -> 
        let num =
            if (config.config_id = "highestCard") then
                n - 1
            else
                n
        in
            config.config_id ^ " = " ^ string_of_int (num) ^ ";"
    | Boolean(b) -> config.config_id ^ " = " ^ string_of_bool b ^ ";"
    | Var(var) -> config.config_id ^ " = " ^ check_stdlib_for_var var ^ ";" 
    | _ -> raise (UnknownType "Invalid type used for configure statement.")

(* Converts a field decl to a java instance var declaration. *)
let java_of_field_decl field_decl =
    (string_of_type field_decl.field_type) ^ " " ^ field_decl.field_id ^ ";\n"

(* Converts a field decl to a java var assignment, which will appear in the 
 * constructor of the MyPlayer class. *)
let java_of_field_decl_assign field_decl = 
    match field_decl.field_type with
    | SetType -> field_decl.field_id ^ " = new Set();"
    | StringType -> field_decl.field_id ^ " = \"\";"
    | BooleanType -> field_decl.field_id ^ " = false;"
    | NumberType -> field_decl.field_id ^ " = 0;"
    (* Should be caught by semantic analysis, here to prevent warning. *)
    | _ -> raise (WrongType("You can't have a field declaration with type " ^
                    "\"" ^ string_of_type field_decl.field_type ^ ".\""))

(* Takes a list of field_decls and converts them to a MyPlayer class. *)
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


let java_of_update = function
    | Assign(id, e) -> java_of_var id ^ " = " ^ java_of_expr e ^ ";"
    | VarDecl(var) -> java_of_type var.var_decl_type ^ " " ^ 
                      var.var_decl_id ^ " = " ^
                      java_of_expr var.var_decl_value ^ ";"







let java_of_player p =
    string_of_int ((int_of_string (String.sub p 6 ((String.length p) - 6 )))-1)

let java_of_objects var =  
    if Str.string_match (Str.regexp "player[0-9]+")  var 0 then 
    "players.get(" ^ java_of_player var ^ ")"
    else
    match var with
    | "size" -> "size()"
    | "top" -> "peek()"
    | _ -> var



let java_of_var var =
    match var with
    |SimpleId(str) -> if Str.string_match (Str.regexp "player[0-9]+")  str 0 then 
    "players.get(" ^ java_of_player str ^ ")"
    else str
    |DotId(str_list) -> String.concat "." (List.map (fun arg -> java_of_objects arg) str_list)


exception UnknownType of string


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
    | "quit" -> "System.exit(0);\n"
    | _ -> normal_call call
(* ; not appended here, see java_of_stmt *)

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
                            " " ^ (java_of_type formal.formal_type) ^ " " ^
                            formal.formal_id) func.formals) 
    in
        access ^ " void " ^ func.decl_name ^ "(" ^ formals ^ ")\n" ^
        java_of_block func.body 

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
