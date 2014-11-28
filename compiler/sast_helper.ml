open Sast

exception UnsupportedAssignExpr

let id_from_assign = function
    | IntAssignDecl(id, _) -> id
    | IntAssign(id, _) -> id
    | _ -> raise UnsupportedAssignExpr
