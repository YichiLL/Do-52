(* ast.ml defines a set of disjoint unions or algebraic types that appear
 * in our parse tree. The parser is responsible for assembling a series
 * of tokens into our tree, and ultimately specifies the complete grammar
 * for our language. But the AST can be thought of as specifying the higher-
 * level structure of our grammar, once all tokens have been parsed into
 * a type. ast.ml defines all of those types, i.e. every type that will appear
 * in our tree. 
 *
 * NOTE: In this file a type cannot be used before it has been declared.
 * NOTE: If you change anything about a type, please update the matching pretty
 * print function below. *)

(* ========================================================================= *)
(*                          Abstract Syntax Tree                             *)
(* ========================================================================= *)
(* Standard operations of any arity. *)
type op = Add | Minus | Multiply | Divide | Equal | NotEqual | Lt | Gt | Ltoe
            | Gtoe | Disj | Conj | Not | Dot

type expr =
    | Number of int
    | String of string
    | Boolean of bool
    | Id of string
    | Unop of op * expr
    | Binop of expr * op * expr

(* Record for a function call *)
type func_call = { 
    fname : string;
    args : expr list;
}

(* Record for variable declaration.
 * Here we're using "_type" because "type" is reserved in OCaml *)
type var_decl = {
    id : string;
    _type : string;
    value : expr;
}

(* Record for a configuration declaration, i.e. assignment to environment 
 * variable. *)
type config_decl = {
    id : string;
    value : expr;
}

(* An "update" is a kind of statement that you can put in the initial 
 * assignment and update sections of a for-loop:
 *
 *      for (update; condition; update) ...
 *
 * An update can only be a variable declaration or an assignment. You can't
 * have other kinds of statements--like if statements or while loops—-in a
 * for-loop header. *)
type update = 
    | Assign of string * expr
    | VarDecl of var_decl

(* Whether a card is drawn form the top or bottom of a deck. *)
type draw_source = Top | Bottom

(* None of our statements are also expressions. They do not evaluate to
 * anything; they only have side-effects. *)
type stmt =
    | Expr of expr
    | Update of update (* Ensures Assigns and VarDecls are statements *)
    | Call of func_call
    | If of expr * stmt list * stmt list
    | While of expr * stmt list
    | For of update * expr * update * stmt list
    | Break
    | Continue
    | Prepend of expr * expr * draw_source
    | Append of expr * expr * draw_source

(* A formal argument has a type and an ID, but no assigned value. *)
type formal = {
    id : string;
    _type : string;
}

(* Record for a function declaration. *)
type func_decl = {
    fname : string;
    formals : formal list;
    body : stmt list;
}

(* A program consists of a series of variable declarations followed by a series
 * of function declarations. *)
type program = {
    configs : config_decl list;
    vars : update list;
    funcs: func_decl list;
}

(* ========================================================================= *)
(*                             Pretty Printing                               *)
(* ========================================================================= *)
(* The printed tree has a (<type>, val) tuple for each node in the AST. *)
let string_of_op = function
    | Add -> "+"
    | Minus -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Equal -> "="
    | NotEqual -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Ltoe -> "<="
    | Gtoe -> ">="
    | Disj -> "|"
    | Conj -> "&"
    | Dot -> "."
    | Not -> "!"

let rec string_of_expr expr = 
    let value = 
        match expr with
        | Number num -> "(<Number> " ^ string_of_int num ^ ")"
        | String str -> "(<String> " ^ str ^ ")"
        | Boolean boolean -> 
            let b =
                if boolean then
                    "true"
                else
                    "false"
            in
                "(<Boolean> " ^ b ^ ")"
        | Id id -> "(<Id> " ^ id ^ ")"
        | Unop(op, e) -> "(<Unop> " ^ string_of_op op ^ string_of_expr e ^ ")"
        | Binop(e1, op, e2) -> "(<Binop> " ^ string_of_expr e1 ^ " " ^
                                string_of_op op ^ " " ^ string_of_expr e2 ^ ")"
    in 
        "(<Expr> " ^ value ^ ")"

(* e.g. (<Call> name:foo args:[expr, expr]) *)
 let string_of_call call =
    let args_s = 
        String.concat ", " (List.map (fun arg -> string_of_expr arg) call.args)
    in
        "(<Call> id:" ^ call.fname ^ " args:[" ^ args_s ^ "])" 

let rec string_of_update update =
    let value = 
        match update with
        | Assign(id, e) -> "(<Assign> id:" ^ id ^ " expr:" ^ string_of_expr e
                            ^ ")"
        | VarDecl(var) -> "(<VarDecl> id:" ^ var.id ^ " type:" ^ var._type ^
                          " value:" ^ string_of_expr var.value ^ ")"
    in
        "(<Update> " ^ value ^ ")" 

let rec string_of_stmt stmt =
    let value =
        match stmt with
        | Expr e -> string_of_expr e
        | Call call -> string_of_call call
        | Update(update) -> string_of_update update
        | If(e, tb, fb) ->  (* expr, true-block, false-block *)
                "(<If> p:" ^ string_of_expr e ^ " t-block:[\n  " ^ 
                string_of_block tb ^ "\n] f-block:[\n  " ^ 
                string_of_block fb ^ "\n])"
        | While(e, b) ->  (* expr, block *)
                "(<While> p:" ^ string_of_expr e ^ " loop:[\n  " ^
                string_of_block b ^ "\n])"
        | Break -> "(<Break>)"
        | Continue -> "(<Continue>)"
        | For(a, e, u, b) ->  (* assign, expr, update, block *)
                "(<For> assign:" ^ string_of_update a ^  " p:" ^ 
                string_of_expr e ^ " update:" ^ 
                string_of_update u ^ " loop:[\n  " ^ 
                string_of_block b ^ "\n])"
        | Prepend(e1, e2, draw_source) ->
                let op = 
                    match draw_source with
                    | Top -> "t>"
                    | Bottom -> "b>"
                in
                    "(<Prepend> " ^ string_of_expr e1 ^ " " ^ op ^ " " ^
                    string_of_expr e2 ^ ")"
        | Append(e1, e2, draw_source) ->
                let op =
                    match draw_source with
                    | Top -> "<t"
                    | Bottom -> "<b"
                in
                    "(<Append> " ^ string_of_expr e1 ^ " " ^ op ^ " " ^
                    string_of_expr e2 ^ ")"
    in 
        "(<Stmt> " ^ value ^ ")"
and string_of_block block =
    String.concat ",\n  " (List.map string_of_stmt block)

let string_of_function func = 
    let formals_s =
        String.concat ", " (List.map (fun formal -> formal._type ^ 
                                " " ^ formal.id) func.formals)
    in
        "(<Func> fname:" ^ func.fname ^ " formals:[" ^ formals_s 
        ^ "] body:\n  " ^ string_of_block func.body ^ "\n)"

let string_of_config (config : config_decl) =
    "(<Configure> id:" ^ config.id ^ " value:" ^ 
    string_of_expr config.value ^ ")"

let string_of_program program =
    let append_nl s1 s2 =
        s1 ^ s2 ^ "\n"
    in let configs_s = 
        List.fold_left append_nl "" (List.map string_of_config program.configs)
    in let vars_s = 
        List.fold_left append_nl "" (List.map string_of_update program.vars)
    in let funcs_s = 
        List.fold_left append_nl "" (List.map string_of_function program.funcs)
    in
        "(<Prgm>\n" ^ configs_s ^ vars_s ^ funcs_s ^ ")\n"
