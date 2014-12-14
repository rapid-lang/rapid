open Sast
open Format


exception UnsupportedSexpr
exception UnsupportedSOutput
exception UntypedVariableReference of string
exception UntypedAccess of string

let bool_expr_s = function
    | SBoolExprLit b -> sprintf "(Bool lit: %b)" b
    | SBoolVar id -> sprintf "(Bool Var: %s)" id
    | SBoolAcc(cls, mem) -> sprintf "(Bool Access: %s.%s)" cls mem
    | SBoolNull -> "(Bool NULL)"

let string_expr_s = function
    | SStringExprLit s -> sprintf "(String Lit: %s)" s
    | SStringVar id -> sprintf "(String Var: %s)" id
    | SStringAcc(cls, mem) -> sprintf "(String Access: %s.%s)" cls mem
    | SStringNull -> "(String NULL)"

let int_expr_s = function
    | SIntExprLit i -> sprintf "(Int Lit: %d)" i
    | SIntVar id -> sprintf "(Int Var: %s)" id
    | SIntAcc(cls, mem) -> sprintf "(Int Access: %s.%s)" cls mem
    | SIntNull -> "(Int NULL)"

let float_expr_s = function
    | SFloatExprLit f -> sprintf "(Lit %f)" f
    | SFloatVar id -> sprintf "(Float Var %s)" id
    | SFloatAcc(cls, mem) -> sprintf "(Float Access: %s.%s)" cls mem
    | SFloatNull -> "(Float NULL)"


let rec sexpr_s = function
    | SExprInt i -> int_expr_s i
    | SExprString s -> string_expr_s s
    | SExprFloat s -> float_expr_s s
    | SExprBool b -> bool_expr_s b
    | SExprUserDef u -> user_def_expr_s u
    | SExprAccess (e, m) -> raise(UntypedAccess(
        "Accesses must be wrewritten with type information"))
    | SId _ -> raise(UntypedVariableReference(
        "Variable references must be rewritten with type information"))
    | NullExpr -> "(NULL EXPR)"
    | _ -> raise UnsupportedSexpr
and sactual_s = function
    | SActual(k,v) -> sprintf "(ACTUAL: %s=%s)" k (sexpr_s v)
and user_def_expr_s = function
    | SUserDefInst(UserDef cls, sactls) ->
        sprintf "(INSTANTIATE new UserDef %s(%s))"
            cls
            ("\n\t" ^ (String.concat ",\n\t" (List.map sactual_s sactls)))
    | SUserDefVar(UserDef cls, id) -> sprintf "(UserDef %s %s)" cls id
    | SUserDefAcc(UserDef cls, cl, mem) -> sprintf "(UserDef %s Access: %s.%s)"
        cls cl mem
    | SUserDefNull(UserDef cls) -> sprintf "(UserDef %s NULL)" cls

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

let suser_def_decl_s cls (id, xpr) =
    sprintf "(Declare %s (%s) to %s)" id cls (sexpr_s xpr)

let semantic_stmt_s = function
    | SAssign a -> svar_assign_s a ^ "\n"
    | SDecl(t, vd) -> svar_decl_s t vd ^ "\n"
    | SOutput o -> sprintf "(Output %s)" (soutput_s o)
    | SUserDefDecl(cls, vd) -> suser_def_decl_s cls vd ^ "\n"
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
    sprintf "(SCLASS %s:\n%s)\n"
        name
        (String.concat "\n" (List.map (fun a -> "\t" ^ a)
            (List.map sattr_s sattrs)))

let string_of_sast (semantic_stmts, sclasses) =
    let semantic_stmts_strs = List.map semantic_stmt_s semantic_stmts in
    let sclasses_strs = List.map sclass_s sclasses in
    (String.concat "" sclasses_strs) ^ (String.concat "" semantic_stmts_strs)

