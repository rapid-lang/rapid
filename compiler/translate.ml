open Sast
open Sast_printer
open Sast_helper
open Datatypes

exception UnsupportedStatementTypeErr of string
exception UnsupportedOutputType of string
exception UnsupportedExpressionType
exception InvalidStringExprType
exception InvalidBoolExprType
exception UnsupportedDeclType
exception InvalidIntExprType
exception InvalidFloatExprType


(* Convert AST.exp to SAST.sexpr *)
let rec translate_expr = function
    (* TODO: a ton more types here, also support recursive expressions *)
    | Ast.IntLit i    -> SExprInt(SIntExprLit i)
    | Ast.StringLit s -> SExprString(SStringExprLit s)
    | Ast.FloatLit f  -> SExprFloat(SFloatExprLit f)
    | Ast.BoolLit b   -> SExprBool(SBoolExprLit b)
    | Ast.ListLit l   ->
        let sexpr_list = List.map translate_expr l in
        SExprList(SListExprLit(None, sexpr_list))
    | Ast.ListAccess(xpr_l, xpr_r) ->
        let sxpr_l = translate_expr xpr_l in
        let sxpr_r = translate_expr xpr_r in
        SExprList(SListAccess(sxpr_l, sxpr_r))
    | Ast.CastBool c  -> SExprBool(SBoolCast (translate_expr c))
    | Ast.Cast(t, xpr) -> translate_cast xpr t
    | Ast.UserDefInst(nm, actls) -> translate_user_def_inst nm actls
    | Ast.Access(e, mem)         -> translate_access e mem
    (* we put a placeholder with the ID in and check after and reclassify *)
    | Ast.Id id       -> SId id
    | Ast.Call(id, expr) -> SCall(id, (List.map translate_expr expr))
    | Ast.Binop(lhs, o, rhs) -> Sast.SBinop(translate_expr lhs, o, translate_expr rhs)
    | Ast.Nullxpr -> UntypedNullExpr
    | _ -> raise UnsupportedExpressionType

and translate_cast xpr = function
    | Int -> SExprInt(SIntCast(translate_expr xpr))
    | Float -> SExprFloat(SFloatCast(translate_expr xpr))
    | Bool -> SExprBool(SBoolCast(translate_expr xpr))
    | String -> SExprString(SStringCast(translate_expr xpr))
and translate_user_def_inst class_id actls =
    SExprUserDef(SUserDefInst(UserDef class_id, (List.map translate_actual actls)))
and translate_actual = function
    | Ast.Actual(nm, xpr) -> (nm, (translate_expr xpr))
and translate_access xpr mem =
    SExprAccess((translate_expr xpr), mem)
and translate_call = function
    | (None, id, exprs) -> SCall(SFCall
        (None, id, (List.map translate_expr exprs)))
    | (Some(xpr), id, exprs) -> SCall(SFCall
        (Some(translate_expr xpr), id, (List.map translate_expr exprs)))
and translate_lhs = function
    | Ast.LhsId(id) -> SLhsId id
    | Ast.LhsAcc(xpr, id) -> SLhsAcc (translate_expr xpr, id)

let translate_assign id xpr = match translate_expr xpr with
    | SExprInt _    -> (id, xpr)
    | SExprString _ -> (id, xpr)
    | SExprBool _   -> (id, xpr)
    | SExprFloat _  -> (id, xpr)
    | SExprList _   -> (id, xpr)
    | SId _         -> (id, xpr)
    | _ -> raise UnsupportedExpressionType

let translate_decl = function
    | (Int | String | Float | Bool) as t, id, i_xpr_opt ->
        SDecl(t, (id, expr_option_map translate_expr i_xpr_opt))
    | ListType t, id, i_xpr_opt ->
        SDecl(ListType t, (id, expr_option_map translate_expr i_xpr_opt))
    | t, _, _ -> raise(UnsupportedStatementTypeErr (Ast_printer.string_of_t t))
    | _ -> raise UnsupportedDeclType

let translate_user_def_decl = function
    | class_id, id, xpr ->
        SUserDefDecl(class_id, (id, (expr_option_map translate_expr xpr)))

let translate_output = function
    | Ast.Println xpr_l -> SPrintln(List.map translate_expr xpr_l)
    | Ast.Printf(format :: xpr_l) -> SPrintf(translate_expr format, List.map translate_expr xpr_l)
    | _ -> raise(UnsupportedOutputType("Not yet implemented"))

let translate_vars = function
    | Ast.ID(s) -> SFuncId(s)
    | Ast.VDecl(vd) ->
        match translate_decl vd with
            | SDecl(t, (id, xpr)) -> SFuncDecl(t,(id, xpr))(*xpr will be None*)

let translate_fcall = function
    | (Some(xpr), id, exprs) ->
        let sexprs = (List.map translate_expr exprs) in
        SFCall(Some(translate_expr xpr), id, sexprs)
    | (None, id, exprs) ->
        let sexprs = (List.map translate_expr exprs) in
        SFCall(None, id, sexprs)

let rec translate_statement = function
    | Ast.VarDecl vd -> translate_decl vd
    | Ast.Assign(lhs, xpr) -> SAssign(translate_lhs lhs, translate_expr xpr)
    | Ast.Output o -> SOutput(translate_output o)
    | Ast.UserDefDecl udd -> translate_user_def_decl udd
    | Ast.FuncCall(vl, fc) -> let sfc = translate_fcall fc in
        SFuncCall((List.map translate_vars vl), sfc)
    | Ast.If(expr, ifstmts, []) -> let ifs = List.map translate_statement ifstmts in
        SIf(translate_expr expr, ifs)
    | Ast.If(expr, ifstmts, else_stmts) -> let ifs = List.map translate_statement ifstmts in
        let es =  List.map translate_statement else_stmts in
        SIfElse(translate_expr expr, ifs, es)
    | Ast.While(expr, stmts) -> SWhile(translate_expr expr, (List.map translate_statement stmts))
    | Ast.For(t, id, xpr, stmts) ->
        let s_id = translate_expr (Ast.Id id) in
        let s_xpr = translate_expr xpr in
        let s_stmts = List.map translate_statement stmts in
        SFor(t, s_id, s_xpr, s_stmts)
    | _ -> raise(UnsupportedStatementTypeErr "type unknown")

let translate_attr = function
    | Ast.NonOption (t, name, Some(xpr)) -> SNonOption(t, name, Some(translate_expr xpr))
    | Ast.NonOption (t, name, None) -> SNonOption(t, name, None)
    | Ast.Optional (t, name) -> SOptional(t, name)

let translate_fstatement = function
    | Ast.FStmt stmt -> translate_statement stmt
    | Ast.Return expr -> SReturn(List.map translate_expr expr)

let translate_function class_opt (f : Ast.func_decl) =
    (
        f.fname,
        class_opt,
        (List.map translate_decl f.args),
        f.return,
        (List.map translate_fstatement f.body)
    )

let rec translate_members class_id sattrs sclass_funcs = function
    | (Ast.Attr a) :: tl ->
        translate_members class_id ((translate_attr a) :: sattrs) sclass_funcs tl
    | (Ast.ClassFunc f) :: tl ->
        let func_name = (class_id ^ "__" ^ f.fname) in
        let sclass_func = (translate_function None
        ({
            fname = func_name;
            args = f.args;
            return = f.return;
            body = f.body
        })) in
        translate_members class_id sattrs (sclass_func :: sclass_funcs) tl
    | [] -> (sattrs, sclass_funcs)


let translate_instance_fn class_id id (f: Ast.func_decl) =
    let func_name = (class_id ^ "__" ^ f.fname) in
    (translate_function (Some (SelfRef (class_id, id)))
        ({
            fname = func_name;
            args = f.args;
            return = f.return;
            body = f.body
        }))
let translate_instance_block class_id (id, fns) =
    (List.map (translate_instance_fn class_id id) fns)

let rec translate_classes sclasses sclass_funcs = function
    | (name, members, Some(Ast.InstanceBlock (id, fns))) :: tl ->
        let sattrs, sclass_fns = translate_members name [] [] members in
        let sinst_fns = translate_instance_block name (id, fns) in
        translate_classes
            ((name, sattrs) :: sclasses)
            (sinst_fns @ sclass_fns @ sclass_funcs)
            tl
    | (name, members, None) :: tl ->
        let sattrs, sclass_fns = translate_members name [] [] members in
        translate_classes
            ((name, sattrs) :: sclasses)
            (sclass_fns @ sclass_funcs)
            tl
    | [] -> sclasses, sclass_funcs
