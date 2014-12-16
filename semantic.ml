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
    configs: Sast.config_decl list;
    mutable fields: Sast.field_decl list;
    scope : symbol_table;
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
exception BadProgram of string (* No setup or round procedures. *)

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

(* Looks for a matching config_decl and returns it. *)
let find_config env id =
    List.find (fun config_decl -> id = config_decl.config_id) env.configs

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

(* Checks to see if a field called id exists in the type _type. *)
let exists_field env (_type, id) =
    List.exists (fun field_decl -> 
                 ((_type, id) = (field_decl.parent_type, field_decl.field_id)))
                    env.fields

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

(* Tries to match a function call to a func_decl in the environment. Matches
 * with both id and arg types so our overloaded output function works. *)
let find_func_decl env fname args = 
    List.find (fun func_decl -> (fname = func_decl.decl_name) &&
                                (match_args args func_decl.formals)) 
                                    env.func_decls

(* Checks if a function already exists using an ID only. So programmers using
 * our language cannot overload functions themselves. *)
let exists_func_decl env fname =
    List.exists (fun func_decl -> (fname = func_decl.decl_name)) env.func_decls

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
        let id, _type =
            check_var env var
        in
            Var(id), _type 
    | Ast.Unop(op, expr) -> 
        let checked_expr =
            check_expr env expr
        in let _, _type =
            checked_expr
        in begin match _type with
        | BooleanType | NumberType -> Sast.Unop(op, checked_expr), _type
        | _ -> raise (WrongType(string_of_type _type ^ " cannot be used with" ^
                      " the \"" ^ string_of_op op ^ "\" operator.")) end
    | Ast.Binop(expr1, op, expr2) ->
        let checked_expr1 = 
            check_expr env expr1
        in let _, type1 =
            checked_expr1
        in let checked_expr2 =
            check_expr env expr2
        in let _, type2 = 
            checked_expr2
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
                        BooleanType
                    | Lt | Gt | Ltoe | Gtoe ->
                        begin match type1 with
                        | NumberType | CardType -> BooleanType
                        | _ -> raise_error type1 op end
                    | Disj | Conj -> 
                        begin match type1 with
                        | BooleanType -> BooleanType
                        | _ -> raise_error type1 op end
                    | _ -> raise (Failure("Illegal operator."))
                in
                    Sast.Binop(checked_expr1, op, checked_expr2), _type
               
(* Takes a var_decl node and checks to see if the var has already been declared
 * in the current scope. Raise an error if it has. Then checks to make sure
 * the var decl has the type that it is supposed to have. If it does, we then 
 * add it to the current scope and return an Sast.var_decl. *)
let check_var_decl env (vdecl : Ast.var_decl) =
    if exists_var_local env.scope vdecl.var_decl_id then
        raise (Redeclaration("The variable \"" ^ vdecl.var_decl_id ^ "\" has" ^
               " already been declared in its scope."))
    else
        let checked_expr =
            check_expr env vdecl.var_decl_value
        in let _, _type =
            checked_expr
        in
            if ((type_of_string vdecl.var_decl_type) =  _type) then
                let checked_vdecl =
                    { var_decl_id = vdecl.var_decl_id;
                      var_decl_type = _type;
                      var_decl_value = checked_expr; }
                in 
                    (* Add to scope then return *)
                    env.scope.vars <- checked_vdecl :: env.scope.vars; 
                    checked_vdecl
            else
                raise (TypeMismatch("You have assigned an expression of type" ^
                        "\"" ^ string_of_type _type ^ "\" to a variable of " ^
                        "type \"" ^ vdecl.var_decl_type ^ ".\""))

(* Checks an update by checking its subtypes. Also makes sure assignments
 * are valid. *)
let check_update env = function
    | Ast.Assign(var, expr) ->
        let var_id, var_type = 
            check_var env var
        in let checked_expr =
            check_expr env expr
        in let _, expr_type =
            checked_expr
        in
            if (var_type = expr_type) then
                Sast.Assign(var_id, checked_expr)
            else
                raise (TypeMismatch("Cannot assign an expression of type \"" ^
                        string_of_type expr_type ^ "\" to a variable of " ^
                        "type \"" ^ string_of_type var_type ^ ".\""))
    | Ast.VarDecl(vdecl) ->
        Sast.VarDecl(check_var_decl env vdecl)

(* Takes a call and adds it to the environment to be checked later --
 * see check_call and check_prgm. Also checks the arguments to the call. *)
let add_call env (call : Ast.func_call) =
    let unchecked_call =
        { fname = call.fname;
          args = List.map (check_expr env) call.args }
    in
        env.unchecked_calls <- unchecked_call :: env.unchecked_calls;
        unchecked_call

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
                { configs = env.configs; 
                  fields = env.fields;
                  scope = new_scope;
                  unchecked_calls = env.unchecked_calls;
                  func_decls = env.func_decls;
                  can_break = env.can_break;
                  can_continue = env.can_continue; }
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
                { configs = env.configs;
                  fields = env.fields;
                  scope = new_scope;
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
        let checked_setup = check_update env setup in
        let checked_expr, expr_type = check_expr env expr in
        let checked_update = check_update env update in
        begin match expr_type with
        | NumberType | BooleanType ->
           begin match checked_update with
           | Sast.Assign(_,_) -> 
               let new_scope =
                   match checked_setup with
                   | Sast.VarDecl(var_decl) ->
                       { parent = Some(env.scope);
                         vars = [ var_decl ]; }
                   | _ ->
                       { parent = Some(env.scope);
                         vars = []; }
               in let new_env =
                   { configs = env.configs;
                     fields = env.fields;
                     scope = new_scope;
                     unchecked_calls = env.unchecked_calls;
                     func_decls = env.func_decls;
                     can_break = true;
                     can_continue = true; }
               in
                   let checked_block =
                       check_block new_env block
                   in
                       Sast.For(checked_setup, (checked_expr, expr_type),
                                checked_update, checked_block)
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

(* Checks that a config_decl refers to an existing configurable variable and
 * that the expression is of the right type. *)
let check_config env (config_decl : Ast.config_decl) =
    let real_config = (* i.e. the existing config *)
        try
            find_config env config_decl.config_id
        with Not_found ->
            raise (UndeclaredID("There is no configurable variable with the " ^
                    "id \"" ^ config_decl.config_id ^ ".\""))
    in let checked_expr =
        check_expr env config_decl.config_value
    in let _, expr_type =
        checked_expr
    in
        if (expr_type = real_config.config_type) then
            { config_id = config_decl.config_id; 
              config_value = checked_expr;
              config_type = expr_type; } (* Returning Sast.config_decl *)
        else
            raise (TypeMismatch("The configurable \"" ^ config_decl.config_id ^
                   "\"" ^ " has type \"" ^ 
                   string_of_type real_config.config_type ^ "\" and cannot " ^
                   "be configured with an expression of type \"" ^
                   string_of_type expr_type ^ ".\""))

(* Checks that a field_decl is adding to type Player, since we decided that
 * it didn't make sense to extend Card or Set. Then makes sure that the
 * ID doesn't match a field that already exists. Finally, adds the field
 * to the environment. *)
let check_field_decl env (field_decl : Ast.field_decl) =
    match (type_of_string field_decl.parent_type) with
    | PlayerType ->
        begin match (type_of_string field_decl.field_type) with
        | NumberType | BooleanType | StringType | SetType ->
            if (not (exists_field env (PlayerType, field_decl.field_id))) then
                let checked_field =
                    { parent_type = type_of_string field_decl.parent_type;
                      field_type = type_of_string field_decl.field_type;
                      field_id = field_decl.field_id; }
                in
                    env.fields <- checked_field :: env.fields;
                    checked_field (* Returning Sast.field_decl *)
            else
                raise (Redeclaration("You cannot add the field \"" ^
                        field_decl.field_id ^ " to Player, because Player " ^
                        "already has a field by that name."))
        | _ -> raise (WrongType("You cannot add a field of type \"" ^
        field_decl.field_type ^ "\" to Player."))
        end
    | _ -> raise (WrongType("You cannot add a field to any type except Player."))

(* Converts an Ast.formal to an Sast.formal. This won't be necessary if we
 * decided to parse type strings into proper types in the first place. *)
let check_formal (formal : Ast.formal) =
    { formal_id = formal.formal_id;
      formal_type = (type_of_string formal.formal_type); }

(* Converts a formal parameter to a variable declaration. *)
let var_of_formal formal = 
    { var_decl_id = formal.formal_id;
      var_decl_type = formal.formal_type;
      var_decl_value = (Sast.Number(0), NumberType); } (* This shouldn't ever be accessed. *)

(* Checks the body of a function declaration before adding the function to the
 * environment. Also checks to make sure we aren't redeclaring a function. 
 * This check is performed simpy with IDs, so overloading is not possible. *)
let check_func_decl env (func_decl : Ast.func_decl) =
    if (not (exists_func_decl env func_decl.decl_name)) then begin
        (* Adds formals to scope before checking body. *)
        let checked_formals = 
            List.map check_formal func_decl.formals
        in let new_scope = 
            { parent = Some(env.scope);
              vars = List.map var_of_formal checked_formals; }
        in let new_env =
            { configs = env.configs; 
              fields = env.fields;
              scope = new_scope;
              unchecked_calls = env.unchecked_calls;
              func_decls = env.func_decls;
              can_break = false;
              can_continue = false; }
        in let checked_fdecl = 
            { decl_name = func_decl.decl_name; 
              formals = List.map check_formal func_decl.formals;
              body = List.map (check_stmt new_env) func_decl.body; }
        in
            env.func_decls <- checked_fdecl :: env.func_decls;
            checked_fdecl 
        end
    else
        raise (Redeclaration("The procedure \"" ^ func_decl.decl_name
                ^ "\" already exists and cannot be redeclared."))

(* Checks to see if setup and round are declared in a program. *)
let rec has_setup_and_round has_setup has_round func_decls =
    match func_decls with
    | [] -> has_setup && has_round
    | fdecl :: rest -> 
        match (fdecl.decl_name, List.length fdecl.formals) with
        | "setup", 0 -> has_setup_and_round true has_round rest
        | "round", 0 -> has_setup_and_round has_setup true rest
        | _ -> has_setup_and_round has_setup has_round rest

(* Checks to see if a call corresponds to a declared function. If not, throw
 * an error. Since a call only matches if it has been given the right args,
 * a call using the correct ID but wrong arg types will not work.
 * This function has type unit. *)
let check_call env (call : Sast.func_call) =
    let _ = 
        try
            find_func_decl env call.fname call.args
        with Not_found ->
            raise (UndeclaredID("The procedure \"" ^ call.fname ^ "\" has " ^
                   "not been declared with the given parameters.."))
    in
        match call.fname with
        | "input" ->
            (* Input only ever takes one arg. *)
            let expr, _ =
                List.hd call.args
            in
                begin match expr with
                | Var(_) -> ()
                | _ -> raise (IllegalUsage("You can only use a variable " ^
                                "expression with \"input.\""))
                end
        | _ -> () (* Returns unit. *)

(* Performs semantic analysis on a program. Also makes sure that a program
 * has a setup and a round procedure. Finally, ensures that all calls are
 * matched with a func_decl. 
 *
 * In other words, takes an AST and makes an SAST. 
 *
 * The variables, configurations, fields, and functions provided by stdlib.ml
 * are added to the environment here. *)
let check_prgm (prgm : Ast.program) = 
    let global_scope =
        { parent = None;
          vars = (List.map fst Stdlib.vars); }
    in let env =
        { configs = Stdlib.configs;
          fields = (List.map fst Stdlib.fields);
          scope = global_scope;
          unchecked_calls = [];
          func_decls = [];  (* func_decls = Stdlib.func_decls *)
          can_break = false;
          can_continue = false; }
    in
        let checked_prgm = 
            { configs = List.map (check_config env) prgm.configs;
              field_decls = List.map (check_field_decl env) prgm.field_decls;
              vars = List.map (check_update env) prgm.vars;
              funcs = List.map (check_func_decl env) prgm.funcs; }
        in
            if (has_setup_and_round false false checked_prgm.funcs) then begin
                List.iter (check_call env) env.unchecked_calls;
                checked_prgm
                end
            else
                raise (BadProgram("You must have a setup and round procedure" ^
                        " in your program. They must both take 0 arguments."))
