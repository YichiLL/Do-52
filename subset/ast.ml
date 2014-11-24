(* ast.ml defines a set of disjoint unions or algebraic types that appear
 * in our parse tree. The parser is responsible for assembling a series
 * of tokens into our tree, and ultimately specifies the complete grammar
 * for our language. But the AST can be thought of as specifying the higher-
 * level structure of our grammar, once all tokens have been parsed into
 * a type. ast.ml defines all of those types, i.e. every type that will appear
 * in our tree. 
 *
 * NOTE: In this file a type cannot be used before it has been declared.
 * NOTE: If you change anything about a type, please update the matching pretty
 * print function below. *)

(* ========================================================================= *)
(*                          Abstract Syntax Tree                             *)
(* ========================================================================= *)
(* Standard operations of any arity. *)
type op = Add | Minus | Multiply | Divide | Equal | NotEqual | Lt | Gt | Ltoe
            | Gtoe | Disj | Conj | Not 

type expr =
    | Number of int
    | String of string
    | Boolean of bool
    | Id of string
    | Unop of op * expr
    | Binop of expr * op * expr

(* Record for a function call *)
type func_call = { 
    fname : string;
    args : expr list;
}

(* Record for variable declaration.
 * Here we're using "_type" because "type" is reserved in OCaml *)
type var_decl = {
    id : string;
    _type : string;
    value : expr
}

type stmt =
    | Expr of expr
    | Assign of string * expr
    | VarDecl of var_decl
    | Call of func_call
    | If of expr * stmt list * stmt list
    | While of expr * stmt list

type program = stmt list

(* ========================================================================= *)
(*                             Pretty Printing                               *)
(* ========================================================================= *)
(* The printed tree has a (<type>, val) tuple for each node in the AST. *)
let string_of_op = function
    | Add -> "+"
    | Minus -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Equal -> "="
    | NotEqual -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Ltoe -> "<="
    | Gtoe -> ">="
    | Disj -> "|"
    | Conj -> "&"
    | Not -> "!"

let rec string_of_expr expr = 
    let value = 
        match expr with
        | Number num -> "(<Number> " ^ string_of_int num ^ ")"
        | String str -> "(<String> " ^ str ^ ")"
        | Boolean boolean -> 
            let b =
                if boolean then
                    "true"
                else
                    "false"
            in
                "(<Boolean> " ^ b ^ ")"
        | Id id -> "(<Id> " ^ id ^ ")"
        | Unop(op, e) -> "(<Unop> " ^ string_of_op op ^ string_of_expr e ^ ")"
        | Binop(e1, op, e2) -> "(<Binop> " ^ string_of_expr e1 ^ " " ^
                                string_of_op op ^ " " ^ string_of_expr e2 ^ ")"
    in 
        "(<Expr> " ^ value ^ ")"

(* e.g. (<Call> name:foo args:[expr, expr]) *)
let string_of_call call =
    let args_s = 
        String.concat ", " (List.map (fun arg -> string_of_expr arg) call.args)
    in
        "(<Call> func:" ^ call.fname ^ " args:[" ^ args_s ^ "])"

let rec string_of_stmt stmt =
    let value =
        match stmt with
        | Expr e -> string_of_expr e
        | Assign(id, e) -> "(<Assign> var:" ^ id ^ " expr:" ^ string_of_expr e
                            ^ ")"
        | VarDecl(var) -> "(<VarDecl> var:" ^ var.id ^ " type:" ^ var._type ^
                          " value:" ^ string_of_expr var.value ^ ")"
        | Call call -> string_of_call call
        | If(e, tb, fb) -> 
            let string_of_block block =
                String.concat ",\n  " (List.map string_of_stmt block)  
            in 
                "(<If> p:" ^ string_of_expr e ^ " t-block:[\n  " ^ 
                string_of_block tb ^ "] f-block:[\n  " ^ 
                string_of_block fb ^ "])"
        | While(e, b) ->
                let string_of_block block =
                    String.concat ",\n  " (List.map string_of_stmt block)
                in
                    "(<While> p:" ^ string_of_expr e ^ " loop:[\n  " ^
                    string_of_block b ^ "])"
    in 
        "(<Stmt> " ^ value ^ ")"

let string_of_program program =
    let value = 
        String.concat "\n" (List.map string_of_stmt program)
    in
        "(<Prgm>\n" ^ value ^ "\n)\n"
