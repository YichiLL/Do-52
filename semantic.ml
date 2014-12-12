(* semantic.ml creates an sast from our ast. It basically resolves each item
 * in the tree to a type and raises errors if there is a type mismatch
 * or unknown ID reference. *)
open Ast
open Sast

(* --------------------------- Context ------------------------------------- *)
(* A symbol_table maps ids -> var_decls and allows us to verify that a variable
 * has been declared and that it is the right type. *)
type symbol_table = {
    parent : symbol_table option;
    vars: Sast.var_decl list;
}

(* An environment represents the current context for a particular node in our
 * tree. *)
type environment = {
    scope : symbol_table;
    fields: Sast.field_decl list;
    can_break : bool; (* If a break statement makes sense. *)
    can_continue: bool; (* If a continue statement makes sense. *)
}

(* -------------------------- Exceptions ----------------------------------- *)
exception UndeclaredID of string
exception MismatchedType of string
exception WrongType of string

(* ------------------------ Helper Functions ------------------------------- *)
(* Looks for a var in the current scope. If it isn't there, checks the next
 * scope. If we've reach global scope and we still haven't found the var, throw
 * an error. *)
let rec find_var scope id =
    try
        List.find (fun vdecl -> id = vdecl.var_decl_id) scope.vars
    with Not_found ->
        match scope.parent with
        | Some(parent) -> find_var parent id
        | _ -> raise Not_found

(* Checks if a type has a field called id by looking through the fields
 * available in the current program. *)
let find_field env (_type, id) =
    List.find (fun field_decl -> 
                ((_type, id) = (field_decl.parent_type, field_decl.field_id)))
                    env.fields

(* ========================================================================= *)
(*                          Semantic Analysis                                *)
(* ========================================================================= *)
(* Recursively checks each ID in a DotId node after the first.
 * We need to check each ID to verify it's a valid field
 * in the type of the previous ID. *)
let rec check_field env last_type id_ls =
    match id_ls with
    | id :: ls ->
        let id = List.hd id_ls
        in let field_decl =
            try
                find_field env (last_type, id)
            with Not_found ->
                raise (UndeclaredID("Undeclared identifier: " ^ id))
        in
            check_field env field_decl.field_type ls
    | [] -> last_type

(* Takes a node of type var in our AST and checks to make sure it refers
 * to something in the current scope. Then returns a node of type simple_expr
 * in our SAST, basically a normal var with attached type information retrieved
 * from the current scope or environment. *)
let check_var env = function
    | Ast.SimpleId(id) -> 
        let vdecl =
            try 
                find_var env.scope id
            with Not_found -> 
                raise (UndeclaredID("Undeclared identifier: " ^ id))
        in
            Sast.Var(id, vdecl.var_decl_type)
    | Ast.DotId(id_list) ->
        (* Check if the first id is a valid var, then check if subsequent ids
         * are valid fields. Final type is type of last field. *)
        let first_id = List.hd id_list
        in let vdecl = 
            try
                find_var env.scope first_id
            with Not_found -> 
                raise (UndeclaredID("Undeclared identifier: " ^ first_id))
        in let final_id =
            String.concat "." id_list
        in
            Sast.Var(final_id, 
                      check_field env vdecl.var_decl_type (List.tl id_list))


