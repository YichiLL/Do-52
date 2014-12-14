(* this is runtime.ml  has a list of (function decl, java form) tuples and a list of field decls for everything "default" we support *)

(* One list is a list of (func_decl, java_form) tuples that are always available in every program. 
That way whenever you need the Java you just look through the list using a call.fname
The other list is so that we can type check with Ids that involve dots. It's a list of field decls
Basically we need to know about the public instance vars in each Java class for each type *)

(* For your reference on sast.ml
type datatype = BooleanType | NumberType | StringType | CardType | SetType | PlayerType

type func_decl = {
decl_name : string;
formals : formal list;
body : stmt list;
}
type field_decl = {
parent_type : datatype;
field_type : datatype;
field_id = string;
}

Also based on the examples at runtime_example.ml
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
              formal_type = Sast.StringType; }];
      body = []; },
      "System.out.println(str)" ) ;
   ({ decl_name = "output";
      formals =
          [{ formal_id = "number";
             formal_type = Sast.NumberType; }];
      body = []; },
      "System.console.println(number)" ) ;
   ({ decl_name = "output";
      formals =
          [{ formal_id = "boolean";
             formal_type = Sast.BooleanType; }];
      body = []; },
      "System.console.println(boolean)") ;
   ( {decl_name = "output";
      formals = 
          [ { formal_id = "card";
              formal_type = Sast.CardType; }];
      body = []; }
      "System.out.println(card) ") ;
   ({ decl_name = "output";
      formals =
          [{ formal_id = "player";
             formal_type = Sast.PlayerType; }];
      body = []; },
      "System.console.println(player)") ;
   ({ decl_name = "output";
      formals =
          [{ formal_id = "set";
             formal_type = Sast.SetType; }];
      body = []; },
      "System.console.println(set)") ;
   ({ decl_name = "input";
      formals =
          [{ formal_id = "str";
             formal_type = Sast.StringType; }];
      body = []; },
      "import java.util.Scanner; str = new Scanner(System.in)")
   (* ({ decl_name = "turn";
      formals =
          [{ formal_id = "player";
             formal_type = Sast.PlayerType; }];
      body = []; },
      "java form of turn with player: player1") ;  *)
   (* ({ decl_name = "evaluate";
      formals = [];
      body = []; },
      "java form of evaluate") ; *) 
   ({ decl_name = "quit";
      formals = [];
      body = []; },
      "System.exit(0);\n") 
]

let fields = 
[
   { parent_type = CardType; field_type = NumberType; field_id = "rank"; } ;
   { parent_type = CardType; field_type = NumberType; field_id = "suit"; } ;
   { parent_type = CardType; field_type = StringType; field_id = "desc" } ;
   { parent_type = SetType; field_type = NumberType; field_id = "size" } ;
   { parent_type = SetType; field_type = StringType; field_id = "desc" } ;
   { parent_type = SetType; field_type = CardType; field_id = "top" } ;
   { parent_type = SetType; field_type = CardType; field_id = "bottom" } ;
   { parent_type = PlayerType; field_type = SetType; field_id = "hand" } ;
   { parent_type = PlayerType; field_type = StringType; field_id = "desc" } 
]