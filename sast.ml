(* sast.ml contains our semantically analyzed abstract syntax tree. Basically,
 * it is our ast but with type information attached. 
 *
 * Many of the AST types have to redeclared here even if they haven't
 * changed. If they incorporate a type that has itself changed, the whole
 * type has to reclared so that the new version is incorporated rather than
 * the old version. *)
open Ast

type datatype = Boolean | Number | String | Card | Set | Player

type simple_expr =
    | Number of int
    | String of string
    | Boolean of bool
    | Var of string * datatype
    | Unop of op * expr
    | Binop of expr * op * expr

type expr = simple_expr * datatype

type config_decl = {
    config_id : string;
    config_value : expr;
    config_type : datatype;
}

type field_decl = {
    expanded_type : datatype;
    field_type : datatype;
    field_id : string;
}

type header = config_decl list * field_decl list

type var_decl = {
    var_decl_id : string;
    var_decl_type : datatype;
    var_decl_value : expr;
}

type func_call = {
    fname : string;
    args : expr list;
}

type update =
    | Assign of var * expr
    | VarDecl of var_decl

type stmt =
    | Update of update
    | Call of func_call
    | If of expr * stmt list * stmt list
    | While of expr * stmt list
    | For of update * expr * update * stmt list
    | Break
    | Continue
    | TimesLoop of stmt * expr
    | Prepend of expr * expr * draw_source
    | Append of expr * expr * draw_source

type formal = {
    formal_id : string;
    formal_type : datatype;
}

type func_decl = {
    decl_name : string;
    formals : formal list;
    body : stmt list;
}

type program = {
    configs : config_decl list;
    field_decls : field_decl list;
    vars : update list;
    funcs : func_decl list;
}
