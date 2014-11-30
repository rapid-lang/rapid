open Sast
open Format

exception UnsupportedSexpr



let int_expr_s = function
    | SIntExprLit i -> sprintf "(Lit %d)" i

let sexpr_s = function
    | SExprInt i -> int_expr_s i
    | _ -> raise UnsupportedSexpr

let soutput_s = function
    | SPrintln xpr_l -> sprintf "Println(%s)" (String.concat ", " (List.map sexpr_s xpr_l))
    | _ -> "Unsupported output"

let svar_assign_s = function
    | IntAssign(id, e) -> sprintf "(Assign (%s) to %s)" id (int_expr_s e)
    | _ -> "UNSUPPORTED ASSIGNMENT"

let svar_decl_s = function
    | IntAssignDecl(id, e_opt) -> (match e_opt with
        | Some e -> sprintf "(Declare (%s) to %s)" id (int_expr_s e)
        | _      -> sprintf "(Declare (%s))" id)
    | _ -> "UNSUPPORTED ASSIGNMENT"

let semantic_stmt_s = function
    | SAssign a -> svar_assign_s a ^ "\n"
    | SDecl d -> svar_decl_s d ^ "\n"
    | SOutput(o) -> sprintf "(Output (%s))" (soutput_s o)
    | _ -> "Unsupported statement"

