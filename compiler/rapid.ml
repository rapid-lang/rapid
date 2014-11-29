open Compile;;

type action = Raw | Ast | Compile

let _ =
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [
            ("-r", Raw);
            ("-a", Ast);
            ("-c", Compile);
        ]
    else Compile in (* Assume compiling *)
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Scanner.token lexbuf in
        match action with
            | Raw -> print_string (Ast_helper.program_s program)
            | Ast -> let listing = Ast_helper.string_of_program program in
                print_string listing
            | Compile -> let code = (Compile.translate program) in
                print_string code

