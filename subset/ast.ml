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
            | Gtoe | Or | And | Not 

type expr =
    | Number of int
    | String of string
    | Boolean of bool
    | Unop of op * expr
    | Binop of expr * op * expr

(* Record for a function call *)
type func_call = { 
    fname : string;
    args : expr list;
}

type stmt =
    | Expr of expr
    | Call of func_call

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
    | Or -> "|"
    | And -> "&"
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
        | Unop(op, e) -> "(<Unop> " ^ string_of_op op ^ string_of_expr e ^ ")"
        | Binop(e1, op, e2) -> "(<Binop> " ^ string_of_expr e1 ^ " " ^
                                string_of_op op ^ " " ^ string_of_expr e2 ^ ")"
    in 
        "(<Expr> " ^ value ^ ")"

let string_of_call call =
    let concat a b = 
        a ^ ", " ^ string_of_expr b
    in
        "(<Call> name:" ^ call.fname ^ " args:[" ^ 
        List.fold_left concat "" call.args ^ "])"
        
let string_of_stmt stmt =
    let value =
        match stmt with
        | Expr e -> string_of_expr e
        | Call call -> string_of_call call
    in 
        "(<Stmt> " ^ value ^ ")"

let string_of_program program =
    let rec prgm_s = function
        | [] -> ""
        | stmt :: l -> "\t" ^ string_of_stmt stmt ^ "\n" ^ prgm_s l
    in
        "(<Prgm>\n" ^ prgm_s program ^ ")\n"
