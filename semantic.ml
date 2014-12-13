(* semantic.ml creates an sast from our ast. It basically resolves each item
 * in the tree to a type and raises errors if there is a type mismatch
 * or unknown ID reference. Types propogate up the tree to the highest
 * level that makes sense. *)
open Ast
open Sast

(* --------------------------- Context ------------------------------------- *)
(* A symbol_table maps ids -> var_decls and allows us to verify that a variable
 * has been declared and that it is the right type. *)
type symbol_table = {
    parent : symbol_table option;
    mutable vars: Sast.var_decl list;
}

(* An environment represents the current context for a particular node in our
 * tree. *)
type environment = {
    scope : symbol_table;
    fields: Sast.field_decl list;
    mutable unchecked_calls: Sast.func_call list;
    mutable func_decls: Sast.func_decl list;
    can_break : bool; (* If a break statement makes sense. *)
    can_continue: bool; (* If a continue statement makes sense. *)
}

(* -------------------------- Exceptions ----------------------------------- *)
exception UnknownType of string
exception UndeclaredID of string
exception TypeMismatch of string
exception WrongType of string
exception Redeclaration of string

(* For syntax-ish errors that we're only catching now. *)
exception IllegalUsage of string

(* ------------------------ Helper Functions ------------------------------- *)
(* Converts a string representing a type to a type, or throws an error if the
 * type is unrecognized. *)
let type_of_string type_str = 
    match type_str with
    | "Boolean" -> BooleanType
    | "Number" -> NumberType
    | "String" -> StringType
    | "Card" -> CardType
    | "Set" -> SetType
    | "Player" -> PlayerType
    | _ -> raise (UnknownType("The type \"" ^ type_str ^ "\" is not valid."))

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

(* Checks to see if a var is in the current local scope. *)
let exists_var_local scope id =
    List.exists (fun vdecl -> id = vdecl.var_decl_id) scope.vars

(* Checks if a type has a field called id by looking through the fields
 * available in the current program. *)
let find_field env (_type, id) =
    List.find (fun field_decl -> 
                ((_type, id) = (field_decl.parent_type, field_decl.field_id)))
                    env.fields

(* Tries to match a function call to an func_decl in the environment. *)
let find_func_decl env fname = 
    List.find (fun func_decl -> fname = func_decl.decl_name) env.func_decls

(* Checks if each arg matches its formal. *)
let rec match_args args formals =
    match args, formals with
    | (arg :: args_rest), (formal :: formals_rest) -> 
        let _, arg_type = arg
        in let form_type = formal.formal_type
        in if (arg_type = form_type) then
            match_args args_rest formals_rest
        else
            false
    | (_ :: _), []
    | [], (_ :: _) -> false
    | [], [] -> true

(* ========================================================================= *)
(*                          Semantic Analysis                                *)
(* ========================================================================= *)
(* Recursively checks each ID in a DotId node after the first.
 * We need to check each ID to verify it's a valid field
 * in the type of the previous ID. *)
let rec check_fields env last_type id_ls =
    match id_ls with
    | id :: ls ->
        let id = List.hd id_ls
        in let field_decl =
            try
                find_field env (last_type, id)
            with Not_found ->
                raise (UndeclaredID("Undeclared identifier: \"" ^ id ^ ".\""))
        in
            check_fields env field_decl.field_type ls
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
                raise (UndeclaredID("Undeclared identifier: \"" ^ id ^ ".\""))
        in
            (id, vdecl.var_decl_type)
    | Ast.DotId(id_list) ->
        (* Check if the first id is a valid var, then check if subsequent ids
         * are valid fields. Final type is type of last field. *)
        let first_id = List.hd id_list
        in let vdecl = 
            try
                find_var env.scope first_id
            with Not_found -> 
                raise (UndeclaredID("Undeclared identifier: \"" ^ first_id ^
                        ".\""))
        in let final_id =
            String.concat "." id_list
        in
            (final_id, check_fields env vdecl.var_decl_type (List.tl id_list))

(* Takes a node of type Ast.expr and converts to Sast.expr, i.e. an expr
 * with an associated type. Also checks to make sure than all ops are used
 * with appropriate and matching types. *)
let rec check_expr env = function
    | Ast.Number(num) -> Sast.Number(num), NumberType
    | Ast.String(str) -> Sast.String(str), StringType
    | Ast.Boolean(b) -> Sast.Boolean(b), BooleanType
    | Ast.Var(var) -> 
        let var_id, var_type =
            check_var env var
        in
            Sast.Var(var_id), var_type
    | Ast.Unop(op, expr) -> 
        let _, _type =
            check_expr env expr
        in begin match _type with
        | BooleanType -> Sast.Unop(op, expr), _type
        | _ -> raise (WrongType(string_of_type _type ^ " cannot be used with" ^
                      " the \"" ^ string_of_op op ^ "\" operator.")) end
    | Ast.Binop(expr1, op, expr2) ->
        let (_, type1) = 
            check_expr env expr1
        in let (_, type2) =
            check_expr env expr2
        in 
            if (not (type1 = type2)) then
                raise (TypeMismatch(string_of_type type1 ^ " does not match "
                                      ^ string_of_type type2 ^ "."))
            else
                let raise_error _type op =
                    raise (WrongType(string_of_type _type ^ " cannot " ^
                           "be used with the \"" ^ string_of_op op ^ 
                           "\" operator."))
                in let _type =
                    match op with
                    | Add ->
                        begin match type1 with
                        | NumberType | StringType -> type1
                        | _ -> raise_error type1 op end
                    | Minus | Multiply | Divide ->
                        begin match type1 with
                        | NumberType -> type1
                        | _ -> raise_error type1 op end
                    | Equal | NotEqual ->
                        begin match type1 with
                        | NumberType | StringType | CardType -> type1
                        | _ -> raise_error type1 op end
                    | Lt | Gt | Ltoe | Gtoe ->
                        begin match type1 with
                        | NumberType | CardType -> type1
                        | _ -> raise_error type1 op end
                    | Disj | Conj -> 
                        begin match type1 with
                        | BooleanType -> type1
                        | _ -> raise_error type1 op end
                    | _ -> raise (Failure("Illegal operator."))
                in
                    Sast.Binop(expr1, op, expr2), _type
               
(* Takes a var_decl node and checks to see if the var has already been declared
 * in the current scope. Raise an error if it has. Then checks to make sure
 * the var decl has the type that it is supposed to have. If it does, we then 
 * add it to the current scope and return a Sast.var_decl. *)
let check_var_decl env (vdecl : Ast.var_decl) =
    if exists_var_local env.scope vdecl.var_decl_id then
        raise (Redeclaration("The variable \"" ^ vdecl.var_decl_id ^ "\" has" ^
               " already been declared in its scope."))
    else
        let _, _type =
            check_expr env vdecl.var_decl_value
        in
            if ((type_of_string vdecl.var_decl_type) =  _type) then
                let checked_vdecl =
                    { var_decl_id = vdecl.var_decl_id;
                      var_decl_type = _type;
                      var_decl_value = vdecl.var_decl_value; }
                in 
                    (* Add to scope then return *)
                    env.scope.vars <- checked_vdecl :: env.scope.vars; 
                    checked_vdecl
            else
                raise (TypeMismatch("You have assigned an expression of type" ^
                        "\"" ^ string_of_type _type ^ "\" to a variable of " ^
                        "type \"" ^ vdecl.var_decl_type ^ ".\""))

(* Takes a call and adds it to the environment to be checked later --
 * see check_call and check_prgm. Also checks the arguments to the call. *)
let add_call env (call : Ast.func_call) =
    let unchecked_call =
        { fname = call.fname;
          args = List.map (check_expr env) call.args }
    in
        env.unchecked_calls <- unchecked_call :: env.unchecked_calls;
        unchecked_call

(* Checks to see if a call corresponds to a declared function. If not, throw
 * an error. Check argument types match formal types in the func_decl. *)
let check_call env (call : Sast.func_call) =
    let func_decl = 
        try
            find_func_decl env call.fname 
        with Not_found ->
            raise (UndeclaredID("The procedure \"" ^ call.fname ^ "\" has " ^
                   "not been declared."))
    in
        if (match_args call.args func_decl.formals) then
            call
        else
            raise (TypeMismatch("The arguments to \"" ^ call.fname ^ "\" do" ^
                    " not match the procedure's declaration."))

(* Checks an update by checking its subtypes. Also makes sure assignments
 * are valid. *)
let check_update env = function
    | Ast.Assign(var, expr) ->
        let var_id, var_type = 
            check_var env var
        in let _, expr_type =
            check_expr env expr
        in
            if (var_type = expr_type) then
                Sast.Assign(var_id, expr, var_type)
            else
                raise (TypeMismatch("Cannot assign an expression of type \"" ^
                        string_of_type expr_type ^ "\" to a variable of " ^
                        "type \"" ^ string_of_type var_type ^ ".\""))
    | Ast.VarDecl(vdecl) ->
        Sast.VarDecl(check_var_decl env vdecl)

(* Checks statements for semantic errors. Statements themselves don't have
 * types. Scoping largely implemented here. *)
let rec check_stmt env = function
    | Ast.Update(update) -> Sast.Update(check_update env update)
    | Ast.Call(call) -> Sast.Call(add_call env call)
    | Ast.If(expr, tblock, fblock) ->
        let checked_expr, expr_type = check_expr env expr in
        begin match expr_type with 
        | NumberType | BooleanType -> 
            let new_scope =
                { parent = Some(env.scope);
                  vars = []; }
            in let new_env =
                { scope = new_scope;
                  fields = env.fields;
                  unchecked_calls = env.unchecked_calls;
                  func_decls = env.func_decls;
                  can_break = false;
                  can_continue = false; }
            in
                let checked_tblock = 
                    check_block new_env tblock
                in let checked_fblock =
                    check_block new_env fblock
                in
                    Sast.If((checked_expr, expr_type), checked_tblock, 
                             checked_fblock)
        | _ -> raise (WrongType("The type \"" ^ string_of_type expr_type ^ 
                      "\" cannot appear in the predicate of an if statement."))
        end
    | Ast.While(expr, block) ->
        let checked_expr, expr_type = check_expr env expr in 
        begin match expr_type with 
        | NumberType | BooleanType -> 
            let new_scope =
                { parent = Some(env.scope);
                  vars = []; }
            in let new_env =
                { scope = new_scope;
                  fields = env.fields;
                  unchecked_calls = env.unchecked_calls;
                  func_decls = env.func_decls;
                  can_break = true;
                  can_continue = true; }
            in
                let checked_block =
                    check_block new_env block
                in
                    Sast.While((checked_expr, expr_type), checked_block)
        | _ -> raise (WrongType("The type \"" ^ string_of_type expr_type ^ 
                      "\" cannot appear in the predicate of an if statement."))
        end
    | Ast.For(setup, expr, update, block) ->
        let checked_update = check_update env update in
        let checked_setup = check_update env setup in
        let checked_expr, expr_type = check_expr env expr in
        begin match expr_type with
        | NumberType | BooleanType ->
           begin match checked_update with
           | Sast.Assign(_,_,_) -> 
               let new_scope =
                   { parent = Some(env.scope);
                     vars = []; }
               in let new_env =
                   { scope = new_scope;
                     fields = env.fields;
                     unchecked_calls = env.unchecked_calls;
                     func_decls = env.func_decls;
                     can_break = true;
                     can_continue = true; }
               in
                   let checked_block =
                       check_block new_env block
                   in
                       Sast.For(checked_update, (checked_expr, expr_type),
                                checked_setup, checked_block)
           | _ -> raise (IllegalUsage("You cannot declare a variable in the"
                         ^ " update section of a for loop header.")) end
        | _ -> raise (WrongType("The type \"" ^ string_of_type expr_type ^ 
                     "\" cannot appear in the predicate of an if statement."))
        end
    | Ast.Break ->
        if env.can_break then
            Sast.Break
        else
            raise (IllegalUsage("You can only use a break statement " ^
                   "inside of a while or for loop."))
    | Ast.Continue ->
        if env.can_continue then
            Sast.Continue
        else
            raise (IllegalUsage("You can only use a continue statement " ^
                   "inside of a while or for loop."))
    | Ast.TimesLoop(stmt, expr) ->
        let checked_stmt =
            check_stmt env stmt
        in let checked_expr, expr_type =
            check_expr env expr
        in
            begin match expr_type with 
            | NumberType -> 
                Sast.(TimesLoop(checked_stmt, (checked_expr, expr_type)))
            | _ -> 
                raise (WrongType("You can only use expressions of type " ^
                           "Number to repeat a statement."))
            end
    | Ast.Prepend(e1, e2, draw_source) ->
        let checked_expr1, expr_type1 =
            check_expr env e1
        in let checked_expr2, expr_type2 =
            check_expr env e2
        in begin match expr_type1, expr_type2 with
        | SetType, SetType ->
            Sast.Prepend((checked_expr1, expr_type1), 
                          (checked_expr2, expr_type2), draw_source)
        | _ -> raise (WrongType("The prepend operator can only be used with " ^
                      "variables of type Set."))
        end
     | Ast.Append(e1, e2, draw_source) ->
        let checked_expr1, expr_type1 =
            check_expr env e1
        in let checked_expr2, expr_type2 =
            check_expr env e2
        in match expr_type1, expr_type2 with
        | SetType, SetType ->
            Sast.Append((checked_expr1, expr_type1), 
                         (checked_expr2, expr_type2), draw_source)
        | _ -> raise (WrongType("The append operator can only be used with " ^
                       "variables of type Set."))
and check_block env block =
    List.map (check_stmt env) block
