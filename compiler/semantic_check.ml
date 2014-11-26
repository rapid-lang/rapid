

exception RepeatDeclarationError of string


(* checks if any variables are redeclared *)
let check_repeat_var_decl stmts =
    let ids = List.map (fun (_, id, _) -> id) stmts in
    let sorted = List.sort String.compare ids in
    let rec compare_last = fun last rest -> match rest with
        | hd :: tl -> (match last = hd with
            | true -> raise(RepeatDeclarationError hd)
            | _ -> compare_last hd tl )
        | [] -> ()
    in match sorted with
        | hd :: tl -> compare_last hd tl
        | [] -> ()


let program_from_ast ast =
    (* ignore functions for now *)
    let (stmts, _) = ast in
        stmts


let string_of_sast sast =
    "program goes here"



