open Ast
	
(* let _ =
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.expr Scanner.token lexbuf in
	print_endline (expr);; *)

let lexbuf = Lexing.from_channel stdin in
		Parser.program Scanner.token lexbuf;;