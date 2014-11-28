
let lexbuf = Lexing.from_channel stdin in
let program = Parser.program Scanner.token lexbuf in
let program_str = Ast_helper.program_s program in
    print_endline program_str;;

print_endline "TEST_SUCCESS";;

