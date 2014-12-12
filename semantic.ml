(* semantic.ml creates an sast from our ast. If you want, you can think of it
 * as yet another pretty print, but instead of turning the ast into a
 * printable string, it turns it into a sast, which is very similar to the ast,
 * but it has type information. semantic.ml also raises errors as it does this
 * "printing" to the sast, making sure that nothing is mismatched or not yet
 * declared. *)

(* this program modifies checks the semantics of Ast *)

(* env has a field called funcs which is a list of function declarations *)
(*type env = { 
mutable funcs: func_decl list; 
}*)

(* check whether a user assigns a string type to a function name *)
let check_func_name name = function
| func -> func.decl_name = name

(* this function checks whether a function's name already exists in env *)
(*let func_exist_in_env func env = List.exists (check_func_name func.decl_name)
env.funcs *)

type symbol_table = {
    (* configs: Sast.config_decl list; *)
    vars: Sast.var_decl list;
    funcs: Sast.func_decl list;
    (* parent: symbol_table options *)
}

type env = {
objects : symbol_table option
}
(*
let find_variable_name_global(scope : env.objects) name =
  let global_table = 
   { vars: []; funcs: [] }
   in 
   { 
    if List.fold_left List.find (fun (v_name,_) -> name = v_name) scope.funcs.formals then List.append ()


 try 
    List.find (fun (v_name,_) -> name = v_name) scope.vars
 with Not_found ->
      match scope.parent with 
    raise(Failure("Variable Not Found"))
*)
let find_variable_type (scope : env.objects) dtype = 
 try 
    List.find (fun (_,v_type) -> dtype = v_type) scope.vars
 with Not_found -> 
    raise(Failure("Variable Type Error"))

