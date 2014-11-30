

let lexbuf = Lexing.from_channel stdin in
let ast = Parser.program Scanner.token lexbuf in
let () = print_endline "\nAST" in
let () = print_endline (Ast_helper.program_s ast) in
let sast = Semantic_check.sast_from_ast ast in
let () = print_endline "\nSAST" in
let program_str = Semantic_check.string_of_sast sast in
    print_endline program_str;;

print_endline "TEST_SUCCESS";;

