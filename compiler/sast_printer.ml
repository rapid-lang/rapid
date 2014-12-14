open Sast
open Format


exception UnsupportedSexpr
exception UnsupportedSOutput
exception UntypedVariableReference of string

let rec sexpr_s = function
    | SExprInt i -> int_expr_s i
    | SExprString s -> string_expr_s s
    | SExprFloat s -> float_expr_s s
    | SExprBool b -> bool_expr_s b
    | SCallTyped (t, (id, args)) -> sprintf "(Call %s) args = %s returns = %s"
        id
        (String.concat ", " (List.map sexpr_s args))
        (Ast_printer.string_of_t t)
    | NullExpr -> "(NULL EXPR)"
    | SId _ -> raise(UntypedVariableReference(
        "Variable references must be rewritten with type information"))
    | UntypedNullExpr -> "(HARD NULL EXPR)"
    | _ -> raise UnsupportedSexpr
and string_expr_s = function
    | SStringExprLit s -> sprintf "(String Lit: %s)" s
    | SStringVar id -> sprintf "(String Var: %s)" id
    | SStringCast xpr -> sprintf "String Cast (%s)" (sexpr_s xpr)
    | SStringNull -> "(String NULL)"
and int_expr_s = function
    | SIntExprLit i -> sprintf "(Int Lit: %d)" i
    | SIntVar id -> sprintf "(Int Var: %s)" id
    | SIntCast e -> sprintf "(Cast (%s) to int)" (sexpr_s e)
    | SIntBinOp(lhs, o, rhs) -> sprintf "(%s %s %s)"
        (sexpr_s lhs) (Ast_printer.bin_op_s o) (sexpr_s rhs)
    | SIntNull -> "(Int NULL)"
and float_expr_s = function
    | SFloatExprLit f -> sprintf "(Lit %f)" f
    | SFloatVar id -> sprintf "(Float Var %s)" id
    | SFloatCast e -> sprintf "(Cast (%s) to float)" (sexpr_s e)
    | SFloatBinOp(lhs, o, rhs) -> sprintf "(%s %s %s)"
        (sexpr_s lhs) (Ast_printer.bin_op_s o) (sexpr_s rhs)
    | SFloatNull -> "(Float NULL)"
and bool_expr_s = function
    | SBoolExprLit b -> sprintf "(Bool lit: %b)" b
    | SBoolVar id -> sprintf "(Bool Var: %s)" id
    | SBoolCast e -> sprintf "(Cast (%s) to boolean)" (sexpr_s e)
    | SBoolBinOp(lhs, o, rhs) -> sprintf "(%s %s %s)"
        (sexpr_s lhs) (Ast_printer.bin_op_s o) (sexpr_s rhs)
    | SBoolNull -> "(Bool NULL)"

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

let lv_s = function
    | SFuncDecl(t, (id, _)) -> sprintf "%s %s" (Ast_printer.string_of_t t) id 
    | SFuncTypedId(_ , id) -> id
    | _ -> raise UnsupportedSOutput

let semantic_stmt_s = function
    | SAssign a -> svar_assign_s a ^ "\n"
    | SDecl(t, vd) -> svar_decl_s t vd ^ "\n"
    | SOutput o -> sprintf "(Output %s)" (soutput_s o)
    | SReturn s -> sprintf("Return(%s)")
        (String.concat ", " (List.map sexpr_s s))
    | SFuncCall(lv, id, params) -> sprintf "Assign(%s) to Call %s(%s)"
        (String.concat ", " (List.map lv_s lv))
        id
        (String.concat ", " (List.map sexpr_s params)) 
    | _ -> "Unsupported statement"

let semantic_func_s f = 
    let (id, args, rets, body) = f in
    let args_strings = (List.map semantic_stmt_s args) in
    let ret_strings = (List.map Ast_printer.string_of_t rets) in
    let body_strings = (List.map semantic_stmt_s body) in
    sprintf "(func %s(%s) %s{\n %s \n})"
        id
        (String.concat "," args_strings)
        (String.concat ", " ret_strings)
        (String.concat "\n" body_strings)

let string_of_sast sast =
    let (stmts, funcs) = sast in
    let stmt_strings = List.map semantic_stmt_s stmts in
    let func_strings = List.map semantic_func_s funcs in
    String.concat " " (List.append stmt_strings func_strings)



