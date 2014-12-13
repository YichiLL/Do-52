open Sast

let funcs = [
    ({ decl_name = "output";
      formals = [];
      body = []; (* Can just leave blank, not used. *) },
      "System.console.println()");
    ({ decl_name = "output";
      formals =
          [{ formal_id = "str";
             formal_type = StringType; }];
      body = []; },
      "System.console.println(str)")
]


