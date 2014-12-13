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

(* a list of tuples: ((funcs:Sast.func_decl),(java:java_call)) *)
let funcs = 
[
   ( {decl_name = "output";
      formals = [];
      body = []; },
      "System.out.println()" ) ;
   ( {decl_name = "output";
      formals = 
          [ { formal_id = "str";
              formal_type = StringType; }];
      body = []; },
      "System.out.println(str)" ) ;
   ({ decl_name = "output";
      formals =
          [{ formal_id = "number";
             formal_type = NumberType; }];
      body = []; },
      "System.console.println(number)" ) ;
   ({ decl_name = "output";
      formals =
          [{ formal_id = "boolean";
             formal_type = BooleanType; }];
      body = []; },
      "System.console.println(boolean)") ;
   ( {decl_name = "output";
      formals = 
          [ { formal_id = "card";
              formal_type = CardType; }];
      body = []; }
      "System.out.println(card) ") ;
   ({ decl_name = "output";
      formals =
          [{ formal_id = "player";
             formal_type = PlayerType; }];
      body = []; },
      "System.console.println(player)") ;
   ({ decl_name = "output";
      formals =
          [{ formal_id = "set";
             formal_type = SetType; }];
      body = []; },
      "System.console.println(set)") ;
   ({ decl_name = "input";
      formals =
          [{ formal_id = "str";
             formal_type = StringType; }];
      body = []; },
      "java form of input with new String in")
   ({ decl_name = "turn";
      formals =
          [{ formal_id = "player";
             formal_type = PlayerType; }];
      body = []; },
      "java form of turn with player: player1") ;
   ({ decl_name = "evaluate";
      formals = [];
      body = []; },
      "java form of evaluate") ; 
   ({ decl_name = "quit";
      formals = [];
      body = []; },
      "java form of quit") ;
