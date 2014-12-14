exception SyntaxError of int * int * string;;


let lexbuf = Lexing.from_channel stdin in
let ast = try
    Parser.program Scanner.token lexbuf
with except ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let col = curr.Lexing.pos_cnum in
    let tok = Lexing.lexeme lexbuf in
    raise (SyntaxError (line, col, tok))
in
let program_str = Ast_printer.program_s ast in
    print_endline program_str;;

print_endline "TEST_SUCCESS";;

