open Sast
open Format


exception UnsupportedSexpr
exception UnsupportedSOutput


let string_expr_s = function
    | SStringExprLit s -> sprintf "(String Lit: %s)" s
    | SStringVar id -> sprintf "(String Var: %s)" id
    | SStringNull -> "(String NULL)"

let int_expr_s = function
    | SIntExprLit i -> sprintf "(Int Lit: %d)" i
    | SIntVar id -> sprintf "(Int Var: %s)" id
    | SIntNull -> "(Int NULL)"

let float_expr_s = function
    | SFloatExprLit f -> sprintf "(Lit %f)" f
    | SFloatVar id -> sprintf "(Float Var %s)" id
    | SFloatNull -> "(Float NULL)"

let rec bool_expr_s = function
    | SBoolExprLit b -> sprintf "(Bool lit: %b)" b
    | SBoolVar id -> sprintf "(Bool Var: %s)" id
    | SBoolCast e -> sprintf "(Cast (%s) to boolean)" (sexpr_s e)
    | SBoolNull -> "(Bool NULL)"
and sexpr_s = function
    | SExprInt i -> int_expr_s i
    | SExprString s -> string_expr_s s
    | SExprFloat s -> float_expr_s s
    | SExprBool b -> bool_expr_s b
    | NullExpr -> "(NULL EXPR)"
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

let string_of_sast sast =
    let strs = List.map semantic_stmt_s sast in
    String.concat "" strs

