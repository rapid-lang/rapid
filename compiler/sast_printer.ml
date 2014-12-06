open Sast
open Format


exception UnsupportedSexpr
exception UnsupportedSOutput


let bool_expr_s = function
    | SBoolExprLit b -> sprintf "(Bool lit: %b)" b
    | SBoolVar id -> sprintf "(Bool Var: %s)" id

let string_expr_s = function
    | SStringExprLit s -> sprintf "(String Lit: %s)" s
    | SStringVar id -> sprintf "(String Var: %s)" id

let int_expr_s = function
    | SIntExprLit i -> sprintf "(Int Lit: %d)" i
    | SIntVar id -> sprintf "(Int Var: %s)" id

let float_expr_s = function
    | SFloatExprLit f -> sprintf "(Lit %f)" f
    | SFloatVar id -> sprintf "(Float Var %s)" id

let sexpr_s = function
    | SExprInt i -> int_expr_s i
    | SExprString s -> string_expr_s s
    | SExprFloat s -> float_expr_s s
    | SExprBool b -> bool_expr_s b
    | _ -> raise UnsupportedSexpr

let soutput_s = function
    | SPrintln xpr_l -> sprintf "(Println(%s))"
        (String.concat ", " (List.map sexpr_s xpr_l))
    | SPrintf(s, xpr_l) -> sprintf "(Printf(%s, %s))"
        (sexpr_s s)
        (String.concat ", " (List.map sexpr_s xpr_l))
    | _ -> raise UnsupportedSOutput

let svar_assign_s (id, xpr) =
    sprintf "(Assign (%s) to %s)" id (sexpr_s xpr)

let svar_decl_s t (id, xpr) =
    sprintf "(Declare %s (%s) to %s)" id (Ast_printer.string_of_t t) (sexpr_s xpr)

let semantic_stmt_s = function
    | SAssign a -> svar_assign_s a ^ "\n"
    | SDecl(t, vd) -> svar_decl_s t vd ^ "\n"
    | SOutput o -> sprintf "(Output %s)" (soutput_s o)
    | _ -> "Unsupported statement"

let sattr_s = function
    | SNonOption (t, name, Some(xpr)) -> sprintf "(SATTR: %s of %s = %s)"
        name
        (Ast_printer.string_of_t t)
        (sexpr_s xpr)
    | SNonOption (t, name, None) -> sprintf "(SATTR: %s of %s)"
        name
        (Ast_printer.string_of_t t)
    | SOptional (t, name) -> sprintf "(OPTIONAL SATTR: %s of %s)"
        name
        (Ast_printer.string_of_t t)



let sclass_s (name, sattrs) =
    sprintf "(SCLASS %s:\n%s)"
        name
        (String.concat "\n" (List.map (fun a -> "\t" ^ a)
            (List.map sattr_s sattrs)))

let string_of_sast (semantic_stmts, sclasses) =
    let semantic_stmts_strs = List.map semantic_stmt_s semantic_stmts in
    let sclasses_strs = List.map sclass_s sclasses in
    (String.concat "" sclasses_strs) ^ (String.concat "" semantic_stmts_strs)

