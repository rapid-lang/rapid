type action = Ast | Sast | Compile
exception SyntaxError of int * int * string;;

let translate ast =
    let sast = Semantic_check.sast_from_ast ast in
    let code = Generate.build_prog sast in
    code

let _ =
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [
            ("-a", Ast);
            ("-s", Sast);
            ("-c", Compile);
        ]
    else Compile in (* Assume compiling *)
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
    let lexbuf = Lexing.from_channel stdin in
    match action with
        | Ast -> print_string (Ast_printer.program_s ast)
        | Sast -> let sast = Semantic_check.sast_from_ast ast in
            print_string (Sast_printer.string_of_sast sast)
        | Compile -> let code = translate ast in
            print_string code
