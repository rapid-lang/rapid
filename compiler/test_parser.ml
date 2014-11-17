open Ast;;


(*
let lexbuf = Lexing.from_channel stdin in
    Parser.program Scanner.token lexbuf;;
*)


let lexbuf = Lexing.from_channel stdin in
let program = Parser.program Scanner.token lexbuf in
let program_str = string_of_program program in
    print_endline program_str;;

print_endline "TEST_SUCCESS";;

