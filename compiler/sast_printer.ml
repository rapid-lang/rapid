open Sast
open Format
open Datatypes


exception UnsupportedSexpr
exception UnsupportedSOutput
exception UnsupportedSattr
exception UntypedVariableReference of string
exception UntypedAccess of string

let newline = Str.regexp "\n"
let indent_block s = Str.global_replace newline "\n\t" s


let rec sexpr_s = function
    | SExprInt i -> int_expr_s i
    | SExprString s -> string_expr_s s
    | SExprFloat s -> float_expr_s s
    | SExprBool b -> bool_expr_s b
    | SExprError e -> error_def_expr_s e
    | SExprUserDef u -> user_def_expr_s u
    | SCallTyped (t, c) -> scall_typed_s (t, c)
    | SExprAccess (e, m) -> raise(UntypedAccess(
        "Accesses must be rewritten with type information"))
    | SExprList l -> list_expr l
    | SId _ -> raise(UntypedVariableReference(
        "Variable references must be rewritten with type information"))
    | NullExpr -> "(NULL EXPR)"
    | UntypedNullExpr -> "(HARD NULL EXPR)"
    | _ -> raise UnsupportedSexpr
and string_expr_s = function
    | SStringExprLit s -> sprintf "(String Lit: %s)" s
    | SStringVar id -> sprintf "(String Var: %s)" id
    | SStringCast xpr -> sprintf "String Cast (%s)" (sexpr_s xpr)
    | SStringAcc(v, mem) -> sprintf "(String Access: %s.%s)"
        (user_def_expr_s v) mem
    | SStringErrAcc(v, mem) -> sprintf "(Error Access: %s.%s)"
        (error_def_expr_s v) mem
    | SStringNull -> "(String NULL)"
and int_expr_s = function
    | SIntExprLit i -> sprintf "(Int Lit: %d)" i
    | SIntVar id -> sprintf "(Int Var: %s)" id
    | SIntCast e -> sprintf "(Cast (%s) to int)" (sexpr_s e)
    | SIntBinOp(lhs, o, rhs) -> sprintf "(%s %s %s)"
        (sexpr_s lhs) (Ast_printer.bin_op_s o) (sexpr_s rhs)
    | SIntAcc(v, mem) -> sprintf "(Int Access: %s.%s)"
        (user_def_expr_s v) mem
    | SIntErrAcc(v, mem) -> sprintf "(Error Access: %s.%s)"
        (error_def_expr_s v) mem
    | SIntNull -> "(Int NULL)"
and float_expr_s = function
    | SFloatExprLit f -> sprintf "(Lit %f)" f
    | SFloatVar id -> sprintf "(Float Var %s)" id
    | SFloatAcc(v, mem) -> sprintf "(Float Access: %s.%s)"
        (user_def_expr_s v) mem
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
    | SBoolAcc(v, mem) -> sprintf "(Bool Access: %s.%s)"
        (user_def_expr_s v) mem
    | SBoolNull -> "(Bool NULL)"
and scall_typed_s = function
    | (t, SFCall(None, id, args)) -> sprintf
        "(Call %s) args = %s returns = %s"
        id
        (String.concat ", " (List.map sexpr_s args))
        (Ast_printer.string_of_t t)
    | (t, SFCall(Some(xpr), id, args)) -> sprintf
        "(Call %s.%s) args = %s returns = %s"
        (sexpr_s xpr)
        id
        (String.concat ", " (List.map sexpr_s args))
        (Ast_printer.string_of_t t)
and sactual_s = function
    | (k,v) -> sprintf "(ACTUAL: %s=%s)" k (sexpr_s v)
and error_def_expr_s = function
    | SErrorInst(sactls) ->
        sprintf "(INSTANTIATE new Error(\n\t%s))"
            (String.concat ",\n\t" (List.map sactual_s sactls))
    | SErrorDefVar(id) -> sprintf "(Error %s)" id
    | SErrorDefAcc(var, mem) -> sprintf "(Error Access: %s.%s)"
        (error_def_expr_s var)
        mem
    | SErrorDefNull -> sprintf "(Error NULL)"
and user_def_expr_s = function
    | SUserDefInst(UserDef cls, sactls) ->
        sprintf "(INSTANTIATE new UserDef %s(\n\t%s))"
            cls
            (String.concat ",\n\t" (List.map sactual_s sactls))
    | SUserDefVar(UserDef cls, id) -> sprintf "(UserDef %s %s)" cls id
    | SUserDefAcc(UserDef cls, var, mem) -> sprintf "(UserDef %s Access: %s.%s)"
        cls (user_def_expr_s var) mem
    | SUserDefNull(UserDef cls) -> sprintf "(UserDef %s NULL)" cls
and sattr_s = function
    | SNonOption(t, id, Some(xpr)) -> sprintf "\n\t(ATTR %s of %s = %s)"
        id
        (Ast_printer.string_of_t t)
        (sexpr_s xpr)
    | SNonOption(t, id, None) -> sprintf "\n\t(ATTR %s of %s NO_DEFAULT)"
        id
        (Ast_printer.string_of_t t)
    | SOptional(t, id) -> sprintf "\n\t(ATTR OPTIONAL %s of %s)"
        id
        (Ast_printer.string_of_t t)
    | _ -> raise UnsupportedSattr

and list_expr = function
    | SListExprLit(Some(t), l) -> sprintf "(Lit <%s> %s )"
        (Ast_printer.string_of_t t)
        (String.concat ", " (List.map sexpr_s l))
    | SListExprLit(None, l) -> sprintf "(Lit <None> %s )"
        (String.concat ", " (List.map sexpr_s l))
    | SListAccess(xpr_l, xpr_r) -> sprintf "(List access %s at index %s)"
        (sexpr_s xpr_l)
        (sexpr_s xpr_r)
    | SListVar(t, id) -> sprintf "(List Var <%s> %s)" id (Ast_printer.string_of_t t)
    | SListNull -> "(List NULL)"


let slhs_s = function
    | SLhsId id -> id
    | SLhsAcc (xpr, mem) -> sprintf "%s.%s" (sexpr_s xpr) mem

let svar_assign_s (lhs, xpr) =
    sprintf "(Assign (%s) to %s)" (slhs_s lhs) (sexpr_s xpr)

let svar_decl_s t (id, xpr) =
    sprintf "(Declare %s (%s) to %s)" id (Ast_printer.string_of_t t) (sexpr_s xpr)

let error_def_decl_s (id, xpr) =
    sprintf "(Declare %s (%s) to Error)" id (sexpr_s xpr)

let suser_def_decl_s cls (id, xpr) =
    sprintf "(Declare USERDEF %s (%s) to %s)" id cls (sexpr_s xpr)

let lv_s = function
    | SFuncDecl(t, (id, _)) -> sprintf "%s %s" (Ast_printer.string_of_t t) id
    | SFuncTypedId(_ , id) -> id
    | _ -> raise UnsupportedSOutput

let sfcall_s = function
    | SFCall(None, id, args) -> sprintf
        "((FCall %s) args = %s)"
        id
        (String.concat ", " (List.map sexpr_s args))
    | SFCall(Some(xpr), id, args) -> sprintf
        "((FCall %s.%s) args = %s)"
        (sexpr_s xpr)
        id
        (String.concat ", " (List.map sexpr_s args))

let rec semantic_stmt_s = function
    | SAssign (lhs, xpr) -> svar_assign_s (lhs, xpr) ^ "\n"
    | SDecl(t, vd) -> svar_decl_s t vd ^ "\n"
    | SErrorDecl vd -> error_def_decl_s vd ^ "\n"
    | SUserDefDecl(cls, vd) -> suser_def_decl_s cls vd ^ "\n"
    | SReturn s -> sprintf("Return(%s)\n")
        (String.concat ", " (List.map sexpr_s s))
    | SFuncCall(lv, sfc) -> sprintf "Assign(%s) to %s"
        (String.concat ", " (List.map lv_s lv))
        (sfcall_s sfc)
    | SFor(t, string_id, xpr, stmts) ->
        sprintf "(For %s %s in %s {\n%s\n}"
            (Ast_printer.string_of_t t)
            string_id
            (sexpr_s xpr)
            (String.concat "\n" (List.map semantic_stmt_s stmts))
    | SWhile(expr, stmts) -> sprintf "(While(%s){\n%s\n})\n"
        (sexpr_s expr)
        (String.concat "\n" (List.map semantic_stmt_s stmts))
    | _ -> "Unsupported statement"

let semantic_func_s f =
    let (id, selfref_opt, args, rets, body) = f in
    let args_strings = (List.map semantic_stmt_s args) in
    let ret_strings = (List.map Ast_printer.string_of_t rets) in
    let body_strings = (List.map semantic_stmt_s body) in
    sprintf "(func %s%s(%s) %s{\n %s \n})"
        (match selfref_opt with
            | Some(SelfRef(class_id, id)) -> sprintf "(%s %s) " class_id id
            | None -> "")
        id
        (String.concat "," args_strings)
        (String.concat ", " ret_strings)
        (String.concat "\n" body_strings)

let semantic_class_s (classname, sattrs) =
    let actl_strings = String.concat "" (List.map sattr_s sattrs) in
    sprintf "(Class %s %s)" classname actl_strings

let route_s (path, args, ret_type, body) =
    let rec arg_to_s = ( function
        | (t, id, xpr) :: tl -> ((sprintf "%s %s = %s"
            (Ast_printer.string_of_t t)
            id
            (sexpr_s xpr)) :: arg_to_s tl)
        | _ -> []) in
    sprintf "(HTTP: %s (%s) (%s)\n\n{\n%s\n})\n"
        path
        (String.concat ", " (arg_to_s args))
        (Ast_printer.string_of_t ret_type)
        (String.concat ", "
            (List.map semantic_stmt_s body))

let string_of_sast sast =
    let (stmts, classes, funcs, routes) = sast in
    let stmt_strings = List.map semantic_stmt_s stmts in
    let class_strings = List.map semantic_class_s classes in
    let func_strings = List.map semantic_func_s funcs in
    let route_strings = List.map route_s routes in
    String.concat ""  (stmt_strings @ class_strings @ func_strings @ route_strings)

