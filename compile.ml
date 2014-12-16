(* compile.ml produces an AST and prints it as a java Game class.
 *
 * Usage: ./compile program.do *)

open Sast

(* ========================================================================= *)
(*                             Helper Functions                              *)
(* ========================================================================= *)
exception CompilerError of string

(* Finds the java code for a given variable ID from stdlib. If the ID isn't 
 * in stdlib, just returns the id. *)
let find_java_for_var var_id = 
    let java_code =
        try
            let _, java =
                List.find (fun (vdecl, _) -> (vdecl.var_decl_id = var_id)) 
                            Stdlib.vars
            in
                java
        with Not_found ->
            var_id
    in
        java_code

(* Returns the java equivalent of a field access. *)
let find_java_for_field field_id =
    let java_code =
        try 
            let _, java =
                List.find (fun (field_decl, _) -> 
                    (field_decl.field_id = field_id)) Stdlib.fields
            in
                java
        with Not_found ->
            field_id
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
    | BooleanType -> "boolean[]"
    | NumberType -> "int[]"
    | StringType -> "String[]"
    | CardType -> "Card"
    | SetType -> "Set"
    | PlayerType -> "MyPlayer"

(* Converts an op to the Java equivalent. *)
let java_of_op = function
    | Ast.Add -> "+"
    | Ast.Minus -> "-"
    | Ast.Multiply -> "*"
    | Ast.Divide -> "/"
    | Ast.Equal -> "=="
    | Ast.NotEqual -> "!="
    | Ast.Lt -> "<"
    | Ast.Gt -> ">"
    | Ast.Ltoe -> "<="
    | Ast.Gtoe -> ">="
    | Ast.Disj -> "||"
    | Ast.Conj -> "&&"
    | Ast.Not -> "!"

(* Returns the proper java field names concatenated together. *)
let rec java_of_field_vars field_vars =
    match field_vars with
    | [] -> ""
    | _ -> "." ^ (String.concat "." (List.map find_java_for_field field_vars))

(* Returns the java code for a var. Vars in the stdlib have special java
 * representations, most vars just use the var_id. *)
let java_of_var var_id =
    let id_list =
        Str.split (Str.regexp("[.]")) var_id
    in 
        match id_list with
        | [] -> raise (CompilerError("No id given to java_of_var."))
        | hd :: tl -> find_java_for_var hd ^ java_of_field_vars tl

(* Converts a simple expression in our SAST to java code, checking the stdlib
 * for special java code representations. *)
let rec java_of_expr expr = 
let (simple, _type) = 
    expr  
in 
match simple with 
    | Number(num)-> string_of_int num
    | String(str)-> str
    | Boolean(boolean)->
        if boolean then
            "true"
        else
            "false"
    | Var(var)-> 
        begin match _type with
        | StringType
        | NumberType
        | BooleanType -> java_of_var var ^ "[0]"
        | _ -> java_of_var var
        end
    | Unop(op, e)-> java_of_op op ^ java_of_expr e
    | Binop(e1, op, e2)-> 
        let _, expr_types = 
            e1
        in 
            begin match expr_types with
            | NumberType | BooleanType -> 
                "(" ^ java_of_expr e1 ^ " " ^ java_of_op op ^ " " ^
                java_of_expr e2 ^ ")"
            | StringType ->
                begin match op with
                | Ast.Add ->
                    "(" ^ java_of_expr e1 ^ " " ^ java_of_op op ^ " " ^
                    java_of_expr e2 ^ ")"
                | Ast.Equal ->
                    "(Utility.compareString(" ^ java_of_expr e1 ^ ", " ^
                    java_of_expr e2 ^ ")"
                | Ast.NotEqual ->
                    "(!Utility.compareString(" ^ java_of_expr e1 ^ ", " ^
                    java_of_expr e2 ^ ")"
                | _ -> raise (CompilerError("Invalid Op."))
                end
            | CardType ->
                begin match op with
                | Ast.Equal -> "(Utility.cardEqual(" ^ java_of_expr e1  ^ 
                        ", " ^ java_of_expr e2 ^  ")"
                | Ast.NotEqual -> "(Utility.cardNotEqual(" ^ java_of_expr e1  ^ 
                        ", " ^ java_of_expr e2 ^ ")"
                | Ast.Lt -> "(Utility.cardLessThan(" ^ java_of_expr e1  ^ 
                        ", " ^ java_of_expr e2 ^ ")"
                | Ast.Gt -> "(Utility.cardGreaterThan(" ^ java_of_expr e1  ^ 
                        ", " ^ java_of_expr e2 ^ ")"
                | Ast.Ltoe -> "(Utility.cardLessOrEqualThan(" ^ java_of_expr e1  
                            ^ ", " ^ java_of_expr e2 ^ ")"
                | Ast.Gtoe -> "(Utility.cardGreaterOrEqualThan(" ^ 
                          java_of_expr e1  ^ ", " ^ 
                          java_of_expr e2 ^ ")"
                | _ -> raise (CompilerError("Invalid Op."))
                end
            | SetType | PlayerType ->
                begin match op with
                | Ast.Equal | Ast.NotEqual -> 
                    "(" ^ java_of_expr e1 ^ " " ^ java_of_op op ^ " " ^
                    java_of_expr e2 ^ ")"
                | _ -> raise (CompilerError("Invalid Op."))
                end
            end 


(* Converts a config_decl to a java assignment. Only numbers, booleans, or
 * variables can be used for configure statements. *)
let java_of_config config = 
    match config.config_value with
    | Number(n), _ -> 
        let num =
            if (config.config_id = "highestCard") then
                n - 1
            else
                n
        in
            config.config_id ^ " = " ^ string_of_int (num) ^ ";"
    | Boolean(b), _ -> config.config_id ^ " = " ^ string_of_bool b ^ ";"
    | Var(var), _-> config.config_id ^ " = " ^ java_of_var var ^ ";" 
    | _ -> raise (CompilerError("Invalid type used for configure statement."))

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
    | _ -> raise (CompilerError("You can't have a field declaration with type "
                  ^ "\"" ^ string_of_type field_decl.field_type ^ ".\""))

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

let java_of_function_expr expr = 
    match expr with
    | Var(var), _ -> java_of_var var
    | _ -> java_of_expr expr

let java_of_args args =
    String.concat ", " (List.map java_of_expr args)

let java_of_function_args args =
    String.concat ", " (List.map java_of_function_expr args)

let java_of_call call =
    match call.fname with
    | "output" -> "System.out.println(" ^ java_of_args call.args ^ ")"
    | "input" -> 
        let expr, _type =
            List.hd call.args
        in let var_id =
            begin match expr with
            | Var(id) -> id
            | _ -> raise (CompilerError("Bad type passed to input()."))
            end
        in
            begin match _type with
            | BooleanType -> var_id ^ " = Utility.inputBool()"
            | NumberType -> var_id ^ " = Utility.inputInt()"
            | StringType -> var_id ^ " = Utility.inputString()"
            | _ -> raise (CompilerError("Bad type passed to input()."))
            end
    | "quit" ->
        "System.exit(0)"
    | _ -> call.fname ^ "(" ^ java_of_function_args call.args ^ ")"
        
let java_of_update = function
    | Assign(id, e) -> 
        let expr, _type = 
            e 
        in 
            begin match _type with
            | BooleanType 
            | NumberType 
            | StringType -> java_of_var id ^ "[0] = " ^  java_of_expr e
            | _ -> java_of_var id ^ " = " ^ java_of_expr e
            end
    | VarDecl(var) -> 
        begin match var.var_decl_type with
        | BooleanType
        | NumberType
        | StringType -> java_of_type var.var_decl_type ^ " " ^ 
                      var.var_decl_id ^ " = new " ^ java_of_type var.var_decl_type 
                      ^ "{" ^ java_of_expr var.var_decl_value ^ "}"
        | _ -> java_of_type var.var_decl_type ^ " " ^ 
                      var.var_decl_id ^ " = " ^
                      java_of_expr var.var_decl_value
    end


let rec java_of_stmt stmt =
    match stmt with
    | Call(call) -> java_of_call call ^ ";"
    | Update(update) -> java_of_update update ^ ";"
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
                | Ast.Top -> "Set.TOP"
                | Ast.Bottom -> "Set.BOTTOM"
            in
                "Set.prepend(" ^ java_of_expr e1 ^ ", "  ^ source ^ ", " ^
                java_of_expr e2 ^ ");\n"
    | Append(e1, e2, draw_source) ->
            let source =
                match draw_source with
                | Ast.Top -> "Set.TOP"
                | Ast.Bottom -> "Set.BOTTOM"
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
        "Set deck;\n" ^
        "int numberOfPlayers = 4;\n" ^
        "int highestCard = 12;\n" ^
        "boolean ascendingOrder = true;\n" ^
        instance_vars ^ "\n" ^
        "public Game() {\n" ^
        config_vars ^
        "deck = new Deck(highestCard, ascendingOrder);\n" ^
        "players = new ArrayList<MyPlayer>();\n" ^
        "for(int i = 0; i < numberOfPlayers; i++) {\n" ^
        "players.add(new MyPlayer(\"Player \" + (i+1)));\n" ^
        "}\n" ^
        "}\n\n" ^
        funcs ^
        "}"

let compile lexbuf = 
    let program = (* This is our SAST. *)
        Semantic.check_prgm (Parser.program Cache.process lexbuf)
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
