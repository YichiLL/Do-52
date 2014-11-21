open Ast

(* let vars = Array.create 10 0 *)

let rec eval = function
    | Number(x) -> x
    | Binop(e1, op, e2) ->
            let v1 = eval e1 and v2 = eval e2 in
            begin match op with
            | Add -> v1 + v2
            | Minus -> v1 - v2
            | Multiply -> v1 * v2
            | Divide -> v1/v2
            | Lt -> if v1 < v2 then 1 else 0
            | Ltoe -> if v1 <= v2 then 1 else 0
            | Gt -> if v1 > v2 then 1 else 0
            | Gtoe -> if v1 >= v2 then 1 else 0
            | Equal -> if v1 == v2 then 1 else 0
            | Notequal -> if v1 != v2 then 1 else 0
            end

let _ =
	let lexbuf = Lexing.from_channel stdin in
		let program = Parser.program Scanner.token lexbuf in
			let result = eval program in
				print_endline(string_of_int result)
(*
		let result = eval e in
		(fun nil1 nil2 nil3 nil4 i -> vars.(i)) 
			(Printf.fprintf stdout "\n")
			(Array.iter print_var vars)
			(Printf.fprintf stdout "%s vars:\n" 
				("$" ^ string_of_int var_n ^ " assigned " ^ string_of_int result ^ "."))
			(vars.(var_n) <- result)
			var_n*)
