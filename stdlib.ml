(* stdlib.ml contains the API for our runtime system. You can think of it as
 * do's stdlib.h or stdio.h. It allows programmers to interact with the runtime
 * system we've set up with our java classes. 
 *
 * This API is in the form of a whole bunch of different declaration types.
 * In C, this is accomplished by including a C header file full of declarations
 * in C. We aren't using include statements, so we just need a list of all
 * the declarations to pre-load into our environment as if they had actually
 * been declared. That way different variables and functions can be set and
 * called in a do program without confusing the semantic analyzer. 
 *
 * See check_prgm in semantic.ml. That's where these lists are loaded into the
 * type-checking environment. *)

(* For your reference on sast.ml
type datatype = BooleanType | NumberType | StringType | CardType | SetType 
                | PlayerType

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

*)

open Sast

(* A list of var_decls that correspond to variables in our environment. The
 * list has the form [(var_decl, java) ...], i.e. it's a list of tuples with
 * the equivalent java code that a reference to a var will be converted to
 * at compile time. All of the var_decl_value fields are 0 because we don't
 * need them. *)
let vars = [
    ( { var_decl_id = "player1";
        var_decl_type = PlayerType;
        var_decl_value = Ast.Number(0); },
      "players[0]");
    ( { var_decl_id = "player2";
        var_decl_type = PlayerType;
        var_decl_value = Ast.Number(0); },
      "players[1]");
    ( { var_decl_id = "player3";
        var_decl_type = PlayerType;
        var_decl_value = Ast.Number(0); },
      "players[2]");
    ( { var_decl_id = "player4";
        var_decl_type = PlayerType;
        var_decl_value = Ast.Number(0); },
      "players[3]");
    ( { var_decl_id = "jack";
        var_decl_type = NumberType;
        var_decl_value = Ast.Number(0); },
      "Card.JACK");
    ( { var_decl_id = "queen";
        var_decl_type = NumberType;
        var_decl_value = Ast.Number(0); },
      "Card.QUEEN");
    ( { var_decl_id = "king";
        var_decl_type = NumberType;
        var_decl_value = Ast.Number(0); },
      "Card.KING");
    ( { var_decl_id = "ace";
        var_decl_type = NumberType;
        var_decl_value = Ast.Number(0); },
      "Card.ACE");
    ( { var_decl_id = "diamond";
        var_decl_type = NumberType;
        var_decl_value = Ast.Number(0); },
      "Card.DIAMOND");
    ( { var_decl_id = "club";
        var_decl_type = NumberType;
        var_decl_value = Ast.Number(0); },
      "Card.CLUB");
    ( { var_decl_id = "heart";
        var_decl_type = NumberType;
        var_decl_value = Ast.Number(0); },
      "Card.HEART");
    ( { var_decl_id = "spade";
        var_decl_type = NumberType;
        var_decl_value = Ast.Number(0); },
      "Card.SPADE");
]

(* A list of config_decls that correspond to configurable environment variables
 * in our runtime environment. *)

(*
(* a list of tuples: ((funcs:Sast.func_decl),(java:java_call)) *)
let funcs = 
[
   (* ============== *)
   (* Main Functions *)
   (* ============== *)
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

   (* ======================= *)
   (* MyPlayer type functions *)
   (* ======================= *)
   ({ decl_name = "";
      formals =
          [{ formal_id = "card";
             formal_type = CardType; };  ];
      body = []; },
      "drawCard(card)" ) ;
   ({ decl_name = "";
      formals =
          [{ formal_id = "set";
             formal_type = SetType; }];
      body = []; },
      "drawCard(set)") ;
   ({ decl_name = "";
      formals =
          [{ formal_id = "i";
             formal_type = NumberType; }];
      body = []; },
      "playCard(i)") ;
   ({ decl_name = "";
      formals = [];
      body = []; },
      "getScore()") ;
   ({ decl_name = "";
      formals = [];
      body = []; },
      "selectCard()")

   (* ================== *)
   (* Set type functions *)
   (* ================== *)
   ({ decl_name = "";
      formals = [];
      body = []; },
      "shuffle()") ;
   ({ decl_name = "";
      formals = List.concat ({ formal_id = "card"; formal_type = CardType; } list) [ { formal_id = "i"; formal_type = NumberType; }; 
      { formal_id = "j"; formal_type = NumberType; } ] ; 
      body = []; },
      "swap(deck, i ,j)") ;
   ({ decl_name = "";
      formals = [];
      body = []; },
      "draw()") ;
   ({ decl_name = "";
      formals =
          [{ formal_id = "n";
             formal_type = NumberType; }];
      body = []; },
      "draw_hand(n)") ;
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
]*)
