open Ast

(* let vars = Array.create 10 0 *)

let print_var i =
	Printf.fprintf stdout "[%d] " i

let rec eval = function
	| Number(x) -> x

(*
	| Var(var_n) -> vars.(var_n)
	| Binop(e1, op, e2) ->
		let v1 = eval e1 and v2 = eval e2 in
		begin match op with
		| Add -> v1 + v2
		| Sub -> v1 - v2
		| Mul -> v1 * v2
		| Div -> v1 / v2
		end
	| Seq(e1, e2) -> (fun i j -> i) (eval e2) (eval e1)
	| Asn(var_n, e) -> vars.(var_n) <- eval e; vars.(var_n)

*)

let _ =
	let lexbuf = Lexing.from_channel stdin in
		let program = Parser.program Scanner.token lexbuf in
			let result = eval program in
				print_endline (string_of_int result)
(*
		let result = eval e in
		(fun nil1 nil2 nil3 nil4 i -> vars.(i)) 
			(Printf.fprintf stdout "\n")
			(Array.iter print_var vars)
			(Printf.fprintf stdout "%s vars:\n" 
				("$" ^ string_of_int var_n ^ " assigned " ^ string_of_int result ^ "."))
			(vars.(var_n) <- result)
			var_n*)
