open Sast
open Format


let id_from_assign = function
    | IntAssignDecl(id, _) -> id
    | IntAssign(id, _) -> id

let int_expr_s = function
    | SIntExprLit i -> sprintf "(Lit %d)" i

let svar_assign_s = function
    | IntAssignDecl(id, e_opt) -> (match e_opt with
        | Some e -> sprintf "(Declare (%s) to %s)" id (int_expr_s e)
        | _      -> sprintf "(Declare (%s))" id)
    | IntAssign(id, e) -> sprintf "(Assign (%s) to %s)" id (int_expr_s e)
    | _ -> "Unsupported assignment"


let semantic_stmt_s = function
    | SAssign sv -> svar_assign_s sv ^ "\n"
    | _ -> "Unsupported statement"
