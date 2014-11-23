(* ast.ml defines a set of disjoint unions or algebraic types that appear
 * in our parse tree. The parser is responsible for assembling a series
 * of tokens into our tree, and ultimately specifies the complete grammar
 * for our language. But the AST can be thought of as specifying the higher-
 * level structure of our grammar, once all tokens have been parsed into
 * a type. ast.ml defines all of those types, i.e. every type that will appear
 * in our tree. *)

type op = Add | Minus | Multiply | Divide | Equal | Notequal | Lt | Gt | Ltoe
            | Gtoe | Or | And | Unop 

type expr =
      Number of int
    | String of string
    | Boolean of bool
    | Id of string
    | Unop of op * expr
    | Binop of expr * op * expr

type stmt =
      Expr of expr

(*
type stmt =
      Block of stmt list
    | If of expr * stmt
    | IfElse of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt
    | Until of stmt * expr
    | SimpLoop of stmt * expr
    | Control of string                 (* e.g. break, continue *)
    | Call of func_call
    | Assign of string * expr

(* Record for a function call *)
type func_call = { 
    fname : string;
    formals : expr list;
}

(* Record for a function declaration *)
type func_decl = {
    fname : string; (* Name of the function *)
    formals : (string * string) list; (* Arguments to the function *)
    (* locals : string list; WE WILL ADD THIS LATER, IT'S COMPLICATED *)
    body : stmt list;
}
*)

(* Missing at the moment: configure, variable decl, type adding *)
type program = stmt list

(* STUFF TO ADD LATER:
    | Decl of string * string * expr
    | HasCalled of string * string * string   (* Type has Type called Id *)
    | Config of string * expr

*)
