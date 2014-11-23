(* ast.ml defines a set of disjoint unions or algebraic types that appear
 * in our parse tree. The parser is responsible for assembling a series
 * of tokens into our tree, and ultimately specifies the complete grammar
 * for our language. But the AST can be thought of as specifying the higher-
 * level structure of our grammar, once all tokens have been parsed into
 * a type. ast.ml defines all of those types, i.e. every type that will appear
 * in our tree. 
 *
 * NOTE: In this file a type cannot be used before it has been declared. *)

(* Standard operations of any arity *)
type op = Add | Minus | Multiply | Divide | Equal | Notequal | Lt | Gt | Ltoe
            | Gtoe | Or | And | Not 

type expr =
      Number of int
    | String of string
    | Boolean of bool
    | Id of string
    | Unop of op * expr
    | Binop of expr * op * expr

(* Record for a function call *)
type func_call = { 
    fname : string;
    formals : expr list;
}

type stmt =
      Expr of expr
    | Call of func_call

type program = stmt list
