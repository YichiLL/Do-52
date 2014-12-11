open Ast

(* this program modifies checks the semantics of Ast *)

(* env has a field called funcs which is a list of function declarations *)
type env = { 
mutable funcs: func_decl list; 
}

(* check whether a user assigns a string type to a function name *)
let check_func_name name = function
| func -> func.decl_name = name

(* this function checks whether a function's name already exists in env *)
let func_exist_in_env func env = List.exists (check_func_name func.decl_name) env.funcs