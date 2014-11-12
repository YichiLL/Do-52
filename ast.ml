type op = Add | Minus | Multiply | Divide | Equal | Notequal | Lt | Gt | Ltoe
            | Gtoe | Or | And

type un_op = Not

type expr =
      Number of int
    | String of string
    | Boolean of bool
    | Id of string
    | Unop of un_op * expr
    | Binop of expr * op * expr
    | Assign of string * expr
    | Call of string * expr list

type stmt =
      Block of stmt list
    | Expr of expr
    | If of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt
    | Until of stmt * expr
    | SimpLoop of stmt * expr
    | Control of string                 (* e.g. break, continue *)
    | Decl of string * string * expr
    | HasCalled of string * string * string   (* Type has Type called Id *)
    | Config of string * expr

type func_call = { 
    fname : string;
    formals : string list;
}

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
}