type action = Ast | Sast | Compile

let translate ast =
    let sast = Semantic_check.sast_from_ast ast in
    (*Ignoring funcs for now when generating code.*)
    let (s, _) = sast in
    let code = Generate.build_prog s in
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
    let ast = Parser.program Scanner.token lexbuf in
    match action with
        | Ast -> print_string (Ast_printer.program_s ast)
        | Sast -> let sast = Semantic_check.sast_from_ast ast in
            print_string (Sast_printer.string_of_sast sast)
        | Compile -> let code = translate ast in
            print_string code