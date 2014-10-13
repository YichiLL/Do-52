type op = Add | Minus | Multiply | Divide | Equal | Notequal | Lt | Rt | Ltoe
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

