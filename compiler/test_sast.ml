

let lexbuf = Lexing.from_channel stdin in
let program = Parser.program Scanner.token lexbuf in
let sast = Semantic_check.sast_from_ast program in
let sast = List.rev sast in
let program_str = Semantic_check.string_of_sast sast in
    print_endline program_str;;

print_endline "TEST_SUCCESS";;

