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

open Sast

(* A list of var_decls that correspond to variables in our environment. The
 * list has the form [(var_decl, java) ...], i.e. it's a list of tuples with
 * the equivalent java code that a reference to a var will be converted to
 * at compile time. All of the var_decl_value fields are 0 because we don't
 * need them. *)
let vars = [
    ( { var_decl_id = "player1";
        var_decl_type = PlayerType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "players.get(0)");
    ( { var_decl_id = "player2";
        var_decl_type = PlayerType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "players.get(1)");
    ( { var_decl_id = "player3";
        var_decl_type = PlayerType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "players.get(2)");
    ( { var_decl_id = "player4";
        var_decl_type = PlayerType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "players.get(3)");
    ( { var_decl_id = "jack";
        var_decl_type = NumberType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "Card.JACK");
    ( { var_decl_id = "queen";
        var_decl_type = NumberType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "Card.QUEEN");
    ( { var_decl_id = "king";
        var_decl_type = NumberType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "Card.KING");
    ( { var_decl_id = "ace";
        var_decl_type = NumberType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "Card.ACE");
    ( { var_decl_id = "diamond";
        var_decl_type = NumberType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "Card.DIAMOND");
    ( { var_decl_id = "club";
        var_decl_type = NumberType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "Card.CLUB");
    ( { var_decl_id = "heart";
        var_decl_type = NumberType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "Card.HEART");
    ( { var_decl_id = "spade";
        var_decl_type = NumberType;
        var_decl_value = (Sast.Number(0), NumberType); },
      "Card.SPADE");
]

(* A list of config_decls that correspond to configurable environment variables
 * in our runtime environment. Here config_value matters and is given a
 * default value. *)
let configs = [
    { config_id = "numberOfPlayers";
      config_value = (Sast.Number(4), NumberType);
      config_type = NumberType; };
    { config_id = "highestCard";
      config_value = (Sast.Number(12), NumberType);
      config_type = NumberType; };
    { config_id = "ascendingOrder";
      config_value = (Sast.Boolean(true), BooleanType);
      config_type = BooleanType; }
]

(* A list of fields for each of our aggregate types. IDs containing dots get
 * checked against this list to make sure they are valid. See check_fields
 * in semantic.ml *)
let fields = [
   ({ parent_type = CardType; field_type = NumberType; field_id = "rank"; },
        "val");
   ({ parent_type = CardType; field_type = NumberType; field_id = "suit"; },
        "suit");
   ({ parent_type = CardType; field_type = StringType; field_id = "desc"; },
        "toString()");
   ({ parent_type = SetType; field_type = NumberType; field_id = "size" },
        "size()");
   ({ parent_type = SetType; field_type = CardType; field_id = "top" },
        "top()");
   ({ parent_type = SetType; field_type = CardType; field_id = "bottom" },
        "bottom()");
   ({ parent_type = SetType; field_type = StringType; field_id = "desc"; },
        "toString()");
   ({ parent_type = PlayerType; field_type = SetType; field_id = "hand" },
        "hand");
   ({ parent_type = PlayerType; field_type = StringType; field_id = "desc"; },
        "toString()");
]

(* a list of tuples: ((funcs:Sast.func_decl),(java:java_call)) *)
let funcs = [
   (* ======================================================== *)
   (*                   Main Functions                         *)
   (* ======================================================== *)
   (* Print a line *)
   { decl_name = "output";
       formals = [];
       body = []; };

   (* Print a string *)
   { decl_name = "output";
     formals = [ { formal_id = "str";
                     formal_type = Sast.StringType; } ];
     body = []; };

   (* Print a number *)
   { decl_name = "output";
     formals = [ { formal_id = "number";
                   formal_type = Sast.NumberType; } ];
     body = []; };

   (* Print a boolean *)
   { decl_name = "output";
     formals = [ { formal_id = "boolean";
                   formal_type = Sast.BooleanType; } ];
     body = []; };

   (* Print a card *)
   { decl_name = "output";
     formals = [ { formal_id = "card";
                   formal_type = Sast.CardType; } ];
     body = []; };

   (* Print a set *)
   { decl_name = "output";
     formals = [ { formal_id = "set";
                   formal_type = Sast.SetType; } ];
     body = []; };

   (* Print a player *)
   { decl_name = "output";
     formals = [ { formal_id = "player";
                   formal_type = Sast.PlayerType; } ];
     body = []; };

   (* Input Bool *)
   { decl_name = "input";
     formals = [ { formal_id = "bool";
                   formal_type = Sast.BooleanType; } ];
     body = []; };

   (* Input Int *)
   { decl_name = "input";
     formals = [ { formal_id = "num";
                   formal_type = Sast.NumberType; } ];
     body = []; };

   (* Input String *)
   { decl_name = "input";
     formals = [ { formal_id = "str";
                   formal_type = Sast.StringType; } ];
     body = []; };

   (* Quit *)
   { decl_name = "quit";
     formals = [];
     body = []; };

(*

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
      *)
]
