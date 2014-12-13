(* this is runtime.ml  has a list of (function decl, java form) tuples and a list of field decls for everything "default" we support *)

(* One list is a list of (func_decl, java_form) tuples that are always available in every program. 
That way whenever you need the Java you just look through the list using a call.fname
The other list is so that we can type check with Ids that involve dots. It's a list of field decls
Basically we need to know about the public instance vars in each Java class for each type *)

(* according to sast.ml
type func_decl = {
decl_name : string;
formals : formal list;
body : stmt list;
}
*)

open Sast

type java_call = {
java_fname : string;
}

(* a list of tuples: ((funcs:Sast.func_decl),(java:java_call)) *)
let func_decl_to_java = 
[ (("Card",[Sast.expr;Sast.expr;Sast.expr;Sast.expr]),"Card");
  (("toString",[],[]),"toString")
]