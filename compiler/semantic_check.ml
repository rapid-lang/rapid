open Sast
open Sast_helper
open Sast_printer
open Datatypes
open Translate

exception RepeatDeclarationErr of string
exception InvalidTypeDeclarationErr of string
exception UncaughtCompareErr of string
exception UnsupportedStatementTypeErr of string
exception UndeclaredVarErr of string
exception InvalidTypeReassignErr of string
exception InvalidTypeErr of string
exception MissingRequiredArgument of string
exception UnsupportedExpressionType
exception UnsupportedSexpr
exception UnsupportedDatatypeErr
exception StringDatatypeRequiredErr
exception InvalidTypeMemberAccess
exception InvalidArgErr
exception InvalidArgOrder
exception InvalidReturnTypeErr
exception NoReturnErr
exception ReturnTypeMismatchErr
exception SfuncIdNotReWritten
exception TooFewArgsErr
exception TooManyArgsErr
exception InvalidBinaryOp
exception BinOpTypeMismatchErr
exception AccessOnNonUserDef
exception AmbiguousContextErr of string
exception NotPrintTypeErr
exception ClassAttrInClassErr
exception UserDefinedTypNeeded
exception UnusedParamArgument

type allowed_types = AllTypes | NumberTypes

(* Takes a type and a typed sexpr and confirms it is the proper type *)
let check_t_sexpr expected_t xpr =
    let found_t = sexpr_to_t expected_t xpr in
    if found_t = expected_t
        then ()
        else raise(InvalidTypeErr(Format.sprintf "Expected %s expression, found %s"
            (Ast_printer.string_of_t expected_t)
            (Ast_printer.string_of_t found_t)))

let is_not_default x = (x = NullExpr)

let check_print_arg = function
    | SExprUserDef(_) | SExprList(SListVar(_,_)) -> raise NotPrintTypeErr
    | _ ->  ()

(*takes a list of args as SDecl(t, xpr) and list of params as sexprs
  Checks the type and if there is some default args not entered, fill them with
  NullExpr*)
let rec check_arg_types lt = function
    | ((ListType AnyList, _) :: tl ), (param :: pl) ->
        let t = sexpr_to_t lt param in
        let r = match param with
            | SExprList _ ->
                if lt = Void or t = lt
                    then param :: check_arg_types lt (tl, pl)
                    else raise InvalidArgErr
            | _ -> raise InvalidArgErr in
            r
    | ((InfiniteArgs, _) :: tl ), (param :: pl) ->
        let () = check_print_arg param in
        param :: check_arg_types lt ([(InfiniteArgs, NullExpr)], pl)
    | ((InfiniteArgs, _ ) :: tl), ([]) -> []
    (*| ((InfiniteArgs, _) :: tl ), (param :: pl) -> *)
    | (((t, _)::tl),(param :: pl)) ->
        let () = check_t_sexpr t param in
        param :: check_arg_types lt (tl, pl)
    | (((_, xpr) :: tl), ([])) -> if (is_not_default xpr)
            then raise TooFewArgsErr
        (*This is the case where the user didn't enter some optional args*)
        else NullExpr :: check_arg_types lt (tl, [])
    | ([], (param :: pl)) -> raise TooManyArgsErr
    | ([],[]) -> []


let get_cast_side = function
    | (Int, Float) -> Left
    | (Float, Int) -> Right
    | (l, r) when l = r -> Neither
    | _ -> raise BinOpTypeMismatchErr

(* Check that for a given attribute, it either exists in the actuals, or
 * it if it isn't place a default value if one exists (or raise if no default
 * value exists). *)
let check_attr sactuals_table = function
    | (name, (t, true, NullExpr)) ->
        if StringMap.mem name sactuals_table
            then let expr = StringMap.find name sactuals_table in
                let () = check_t_sexpr t expr in
                (name, expr)
            else raise(MissingRequiredArgument
                (Format.sprintf "Argument %s is missing" name))
    | (name, (t, false, xpr)) ->
        if StringMap.mem name sactuals_table
            then let expr = StringMap.find name sactuals_table in
                let () = check_t_sexpr t expr in
                (name, expr)
        else (name, xpr)

(* Check that all of the actuals in the instantiation are valid. *)
let check_user_def_inst ct t sactls =
    (* build a table from the explicit actuals *)
    let sactuals_table = add_actls empty_actuals_table sactls in
    let attr_table = get_attr_table t ct in
    let checked_sactuals = List.map
        (check_attr sactuals_table)
        (StringMap.bindings attr_table) in
    SUserDefInst (UserDef t, checked_sactuals)

(* Takes a symbol table and sexpr and rewrites variable references to be typed *)
let rec rewrite_sexpr st ct ft ?t = function
    | SId id -> (
        match get_type id st with
        | Int -> SExprInt(SIntVar id)
        | String -> SExprString(SStringVar id)
        | Float -> SExprFloat(SFloatVar id)
        | Bool -> SExprBool(SBoolVar id)
        | UserDef cls -> SExprUserDef(SUserDefVar ((UserDef cls), id))
        | ListType(ty) -> SExprList(SListVar(ty, id))
        | _ -> raise UnsupportedDatatypeErr)
    | SExprBool(SBoolCast e) ->
        SExprBool(SBoolCast(rewrite_cast st ct ft e AllTypes))
    | SExprInt(SIntCast e) ->
        SExprInt(SIntCast(rewrite_cast st ct ft e NumberTypes))
    | SExprFloat(SFloatCast e) ->
        SExprFloat(SFloatCast(rewrite_cast st ct ft e NumberTypes))
    | SExprString(SStringCast e) ->
        SExprString(SStringCast(rewrite_cast st ct ft e AllTypes))
    | SCall(c) -> (match c with
        | SFCall(Some(xpr), fn_id, xprs) ->
            let xpr = rewrite_sexpr st ct ft xpr in
            let class_id = (match sexpr_to_t Void xpr with
                | UserDef u -> u
                | _         -> raise AccessOnNonUserDef) in
            let id = (class_id  ^ "__" ^ fn_id) in
            let xprs = (List.map (rewrite_sexpr st ct ft) xprs) in
            let xprs = check_arg_types Void ((get_arg_types id ft), xprs) in
            let rt = get_return_type id ft in
            let rt = match rt with
                | ListType(AnyList) -> sexpr_to_t Void (List.hd xprs)
                | _ -> rt in
            SCallTyped(rt, SFCall(Some(xpr), id, xprs))
        | SFCall(None, id, xprs) ->
            let xprs = (List.map (rewrite_sexpr st ct ft) xprs) in
            let xprs = check_arg_types Void ((get_arg_types id ft), xprs) in
            let rt = get_return_type id ft in
            let rt = match rt with
                | ListType(AnyList) -> sexpr_to_t Void (List.hd xprs)
                | _ -> rt in
            SCallTyped(rt, SFCall(None, id, xprs)))
    | SExprList(SListExprLit(None, untyped_l)) ->
        rewrite_sexpr_list st ct ft untyped_l t
    | SExprList(SListAccess(xpr_l, xpr_r)) ->
        let rewritten_l = rewrite_sexpr st ct ft xpr_l in
        let rewritten_r = rewrite_sexpr st ct ft xpr_r in
        (* Verify that index is an int *)
        let () = check_t_sexpr Int rewritten_r in
        SExprList(SListAccess(rewritten_l, rewritten_r))
    | SBinop (lhs, o, rhs) -> let lhs = rewrite_sexpr st ct ft lhs in
        let rhs = rewrite_sexpr st ct ft rhs in
        let lt = sexpr_to_t Void lhs in
        let rt = sexpr_to_t Void rhs in
        let possible_ts = get_op_types o in
        if (List.mem rt possible_ts) && (List.mem lt possible_ts) then
            match o with
            | Ast.Less | Ast.Greater | Ast.Leq | Ast.Geq | Ast.Equal | Ast.Neq ->
                let lhs, rhs =  binop_cast_floats lhs rhs (get_cast_side (lt, rt)) in
                SExprBool(SBoolBinOp(lhs, o, rhs))(*bool exprs allow casting *)
            | Ast.And | Ast.Or -> if(rt = lt && lt = Bool)
                then SExprBool(SBoolBinOp(lhs, o, rhs))
                else raise BinOpTypeMismatchErr
            | _ -> if(rt = lt) then match lt with
                    | Int -> SExprInt(SIntBinOp(lhs, o, rhs))
                    | Float -> SExprFloat(SFloatBinOp(lhs, o, rhs))
                else
                    let lhs, rhs = binop_cast_floats lhs rhs (get_cast_side (lt, rt)) in
                    SExprFloat(SFloatBinOp(lhs, o, rhs))
        else raise InvalidBinaryOp
    | SExprUserDef udf -> (
        match udf with
        | SUserDefInst(UserDef t, sactls) ->
            let rewritten_sactls = List.map (rewrite_sactl st ct ft) sactls in
            let expr = check_user_def_inst ct t rewritten_sactls in
            SExprUserDef(expr)
        | _ -> SExprUserDef udf)
    | SExprAccess(xpr, mem) ->
        let rewritten_sexpr = rewrite_sexpr st ct ft xpr in
        let cls = match rewritten_sexpr with
            | SExprUserDef(
                  SUserDefInst(UserDef s, _)
                | SUserDefVar(UserDef s, _)
                | SUserDefNull(UserDef s)) -> s
            | _ -> raise InvalidTypeMemberAccess in
        let class_var_expr = (match rewritten_sexpr with
            | SExprUserDef(xpr) -> xpr
            | _ -> raise UserDefinedTypNeeded) in
        let t = get_attr_type cls ct mem in
        (match t with
            | Bool -> SExprBool(SBoolAcc(class_var_expr, mem))
            | Int -> SExprInt(SIntAcc(class_var_expr, mem))
            | Float -> SExprFloat(SFloatAcc(class_var_expr, mem))
            | String -> SExprString(SStringAcc(class_var_expr, mem))
            | _ -> raise ClassAttrInClassErr)
    | xpr -> xpr
and rewrite_sactl st ct ft = function
    | (name, xpr) -> (name, rewrite_sexpr st ct ft xpr)
and rewrite_cast st ct ft xpr t_opt =
    let xpr = rewrite_sexpr st ct ft xpr in
    let t = sexpr_to_t Void xpr in
    match (t_opt, t) with
        | (AllTypes, (Int | Float | String | Bool)) -> xpr
        | (NumberTypes, (Int | Float)) -> xpr
        | _ -> raise(InvalidTypeErr(Format.sprintf
            "Cast cannot use %s expression" (Ast_printer.string_of_t t)))
and binop_cast_floats lhs rhs = function
    | Left -> SExprFloat(SFloatCast(lhs)), rhs
    | Right -> lhs, SExprFloat(SFloatCast(rhs))
    | _ -> lhs, rhs

(* typechecks a sexpr *)
and rewrite_sexpr_list st ct ft untyped_l = function
    | Some(ListType(child_type) as ty) ->
        let typed_sexprs = List.map (
            rewrite_sexpr st ct ft ~t:child_type
        ) untyped_l in
        let _ = List.map (fun child ->
            let actual_type = sexpr_to_t child_type child in
            if actual_type <> child_type
                then
                raise(InvalidTypeErr(Format.sprintf
                    "Actual type %s did not match declared child type %s"
                    (Ast_printer.string_of_t actual_type)
                    (Ast_printer.string_of_t child_type)))
        ) typed_sexprs in
        SExprList(SListExprLit(Some(ty), typed_sexprs))
    | None -> raise(AmbiguousContextErr("Type must be passed in for lists"))

(* typechecks a sexpr *)
let rewrite_sexpr_to_t st ct ft xpr t =
    let typed_xpr = rewrite_sexpr st ct ft xpr in
    let () = check_t_sexpr t typed_xpr in
    typed_xpr

(* checks that an assignment has the proper types *)
let check_var_assign_use st id xpr =
    let var_t = (get_type id st) in
    let () = check_t_sexpr var_t xpr in
    st

(*Check that the return statement has expressions with the right types*)
let rec check_return_types = function
    | (xpr :: s),(t :: types) -> let () = (check_t_sexpr t xpr) in
        check_return_types (s, types)
    (*To few vals returned*)
    | ([]),(t::types) -> raise InvalidReturnTypeErr
    (*to many vals returned*)
    | (xpr :: s),([]) -> raise InvalidReturnTypeErr
    | [],[] -> ()

(*Scan all stmts in a function for returns then check the types*)
let rec check_returns r = function
    | SReturn(s) :: tl-> let _ = check_return_types (s, r) in
         SReturn(s) :: check_returns r tl
    | hd :: tl -> hd :: check_returns r tl
    | [] -> []

(*takes an sfunc_lval list * var_type list, gotten the return type list
 * in the funciton table this checks if the left hand side vars or var
 * decls are the same types as the return types. *)
let rec check_lv_types = function
    | (SFuncTypedId(t, _) :: tl), (expected_t :: types) -> if t = expected_t
            then check_lv_types  (tl, types)
        else raise ReturnTypeMismatchErr
    | (SFuncDecl(t, _) :: tl), (expected_t :: types) -> if t = expected_t
            then check_lv_types  (tl, types)
        else raise ReturnTypeMismatchErr
    | (SFuncId(i) :: tl), _ -> raise SfuncIdNotReWritten
    | [], (t::types) -> raise ReturnTypeMismatchErr
    | (s :: tl), [] -> raise ReturnTypeMismatchErr
    | [],[] -> ()

(*rewrite so ids have a type*)
let rewrite_lv st = function
    | SFuncId(i) -> SFuncTypedId((get_type i st), i)
    | SFuncDecl(t, sv) -> SFuncDecl(t, sv)

(*adds any var decls on the left hand side of a function statement to the symbol table.*)
let rec scope_lv st = function
    | SFuncDecl(t, (id, _)) :: tl -> let st = (add_sym t id st) in
        scope_lv st tl
    | SFuncId(i) :: tl -> scope_lv st tl
    | SFuncTypedId(_, _) :: tl -> scope_lv st tl
    | [] -> st

(*
Adds all var decls in a stmt list to the scope and returns the new scope
This does not do any type checking, and ignores the optional expression
*)
let rec add_to_scope st = function
    | SDecl(t, (id, xpr)) :: tl ->
       let st = add_sym t id st in
        add_to_scope st tl
    | SFuncCall (lv, _) :: tl -> let st = scope_lv st lv in
        add_to_scope st tl
    | _ :: tl -> add_to_scope st tl
    | [] -> st

(* Processes an unsafe SAST and returns a type checked SAST *)
let rec var_analysis st ct ft = function
    | SDecl(t, (id, xpr)) :: tl ->
        let expr = rewrite_sexpr st ct ft xpr ~t:t in
        let st = add_sym t id st in
        let () = check_t_sexpr t expr in
            SDecl(t, (id, expr)) :: var_analysis st ct ft tl
    | SAssign(SLhsId(id), xpr) :: tl ->
        let expr = rewrite_sexpr st ct ft xpr in
        let st = check_var_assign_use st id expr in
            SAssign(SLhsId id, expr) :: (var_analysis st ct ft tl)
    | SAssign(SLhsAcc(x, mem), xpr) :: tl ->
        let x = rewrite_sexpr st ct ft x in
        let xpr = rewrite_sexpr st ct ft xpr in
        let lhs_class_id = (match sexpr_to_t Void x with
            | UserDef u -> u
            | _         -> raise AccessOnNonUserDef) in
        let lhs_t = get_attr_type lhs_class_id ct mem in
        let () = check_t_sexpr lhs_t xpr in
            SAssign(SLhsAcc(x, mem), xpr) :: (var_analysis st ct ft tl)
    (* Return stmts are xpr lists, tranlslate all the expressions here *)
    | SReturn(s) :: tl -> let xprs = List.map (rewrite_sexpr st ct ft) s in
         SReturn(xprs) :: (var_analysis st ct ft tl)
    | SFuncCall (lv, SFCall(xpr, id, xprs)) :: tl ->
        let xpr, class_id__ = (
            match xpr with
            | Some(e) ->
                let e = rewrite_sexpr st ct ft e in
                let c_id__ = (
                    match sexpr_to_t Void e with
                    | UserDef u -> u ^ "__"
                    | _         -> raise AccessOnNonUserDef
                ) in
                Some(e), c_id__
            | None    -> None, ""
        ) in
        let id = (class_id__ ^ id) in
        let lv = (List.map (rewrite_lv st) lv) in
        let check_lv ft id = function
            | [] -> () (*ignoring return types so foo(); is always a valid stmnt*)
            (*If there is left hand side to the statement make sure types match*)
            | _ -> check_lv_types (lv, (get_return_type_list id ft)) in
        let () = check_lv ft id lv in
        let xprs = (List.map (rewrite_sexpr st ct ft) xprs) in
        let xprs = check_arg_types Void ((get_arg_types id ft), xprs) in
        let st = scope_lv st lv in
        SFuncCall(lv, SFCall(xpr, id, xprs)) :: (var_analysis st ct ft tl)
    | SUserDefDecl(cls, (id, xpr)) :: tl ->
        let checked_expr = rewrite_sexpr st ct ft xpr in
        let t = UserDef cls in
        let st = add_sym t id st in
        let () = check_t_sexpr t checked_expr in
            SUserDefDecl(cls, (id, checked_expr)) :: var_analysis st ct ft tl
    | SIf(xpr, stmts) :: tl ->
        let expr = rewrite_sexpr st ct ft xpr in
        let () = check_t_sexpr Bool expr in
        let new_scope = new_scope st in
        let stmts = var_analysis new_scope ct ft stmts in
        SIf(expr, stmts) :: (var_analysis st ct ft tl)
    | SIfElse(xpr, stmts, estmts) :: tl ->
        let expr = rewrite_sexpr st ct ft xpr in
        let () = check_t_sexpr Bool expr in
        let if_scope = new_scope st in
        let stmts = var_analysis if_scope ct ft stmts in
        let else_scope = new_scope st in
        let estmts = var_analysis else_scope ct ft estmts in
        SIfElse(expr, stmts, estmts) :: (var_analysis st ct ft tl)
    | SWhile(xpr, stmts) :: tl ->
        let expr = rewrite_sexpr st ct ft xpr in
        let () = check_t_sexpr Bool expr in
        let stmts = var_analysis (new_scope st) ct ft stmts in
        SWhile(expr, stmts) :: var_analysis st ct ft tl
    | SFor (t, string_id, xpr, stmts) :: tl ->
        let scoped_st = new_scope st in
        let scoped_st = add_sym t string_id scoped_st in
        let xpr = rewrite_sexpr scoped_st ct ft xpr ~t:(ListType t) in
        let for_body = var_analysis scoped_st ct ft stmts in
        SFor(t, string_id, xpr, for_body) :: (var_analysis st ct ft tl)
    | [] -> []


(*Called when we see an arg with default val, all the rest must have defaults*)
let rec check_default_args = function
    | SDecl(t, (id, xpr)) :: tl -> if is_not_default xpr
            then raise InvalidArgOrder
        else
            SDecl(t, (id, xpr)) :: check_default_args tl
    | _ :: _ -> raise InvalidArgErr
    | [] -> []

(*Checks to make sure args with default vals come at the end fo the arg list*)
let rec check_arg_order = function
    | SDecl(t, (id, xpr)) :: tl -> if is_not_default xpr
            then SDecl(t, (id, xpr)) :: check_arg_order tl
        else
            SDecl(t, (id, xpr))  :: check_default_args tl
    | _ :: _ -> raise InvalidArgErr
    | [] -> []

let check_for_return body =
    let last_stmt = List.hd (List.rev body) in
    match last_stmt with
        | SReturn(s) -> ()
        | _ -> raise NoReturnErr

let rec check_funcs st ct ft = function
    | (fname, class_opt, args, rets, body) :: tl ->
        let _ = check_arg_order args in
        let scoped_st = new_scope st in
        let targs = var_analysis scoped_st ct ft args in
        (*args are added to function scope*)
        let scoped_st = add_to_scope scoped_st args in
        (* Add the reference to self to the symbol table, if there is one *)
        let scoped_st = (match class_opt with
            | Some(SelfRef (class_id, id)) ->
                add_sym (UserDef class_id) id scoped_st
            | None -> scoped_st) in
        (*typecheck the body and rewrite vars to have type*)
        let tbody = var_analysis scoped_st ct ft body in
        (*check the return type matches the return statement*)
        let _  = check_returns rets tbody in
        (*if no return types then don't worry, else find a return stmnt*)
        if rets = [] then
            (fname, class_opt, targs, rets, tbody) :: check_funcs st ct ft tl
        else
            let () = check_for_return tbody in
            let qq = (fname, class_opt, targs, rets, tbody) :: check_funcs st ct ft tl in
            qq
    | [] -> []

let rec build_function_table ft = function
    | (fname, class_opt, args, rets, body) :: tl ->
        let args_to_type = function
            | SDecl(t, (id, xpr)) -> (t, xpr)
            | _ -> raise InvalidArgErr
        in
        let arg_ts = List.map args_to_type args in
        let ft = (add_func ft fname arg_ts rets) in
        build_function_table ft tl
    | [] -> ft

(* Prcesses unchecked classes, adding them and their attributes to class_tbl *)
let rec class_analysis class_tbl = function
    | (class_id, attrs) :: tl ->
        let attr_tbl = add_attrs empty_attribute_table attrs in
        let class_tbl = new_class class_id attr_tbl class_tbl in
        let lst, class_tbl = class_analysis class_tbl tl in
        ((class_id, attrs) :: lst), class_tbl
    | [] -> [], class_tbl


let gen_class_stmts stmts =
    let sclasses, sclass_fns = translate_classes [] [] stmts in
    let (checked_sclasses, ct) = class_analysis class_table sclasses in
    checked_sclasses, ct, sclass_fns

let rec populate_http_symbol_table st = function
    | (t, id, xpr) :: tl -> populate_http_symbol_table (add_sym t id st) tl
    | [] -> st

(*
 * rt: route table
 *)
let rec validate_http_tree path params rt ctx = function
    | SParam(t, id, tree) :: tl ->
        let rest, rt = validate_http_tree path params rt ctx tl in
        let params = (t, id) :: params in
        let path = Format.sprintf "%s/:%s" path id in
        let rt = add_route path rt in
        let sub_tree, rt = validate_http_tree path params rt ctx tree in
        (rest @ sub_tree), rt
    | SNamespace(name, tree) :: tl ->
        let rest, rt = validate_http_tree path params rt ctx tl in
        let path = Format.sprintf "%s/%s" path name in
        let rt = add_route path rt in
        let sub_tree, rt = validate_http_tree path params rt ctx tree in
        (rest @ sub_tree), rt
    | SEndpoint(name, args, ret_t, body) :: tl ->
        (* takes arguments and path params and confirms they all exist *)
        let rec check_args = (function (* args, required args *)
            (* http arguments must be unpacked *)
            | ((a_t, id, _) :: a_tl), (req :: req_tl) ->
                if (a_t, id) = req then check_args (a_tl, req_tl)
                else raise UnusedParamArgument
            | x, []  -> ()
            | [], x -> raise UnusedParamArgument) in
        let () = check_args (args, params) in
        let rest, rt = validate_http_tree path params rt ctx tl in
        let path = Format.sprintf "%s/%s" path name in
        let st, ct, ft = ctx in
        let st = populate_http_symbol_table st args in
        let body = var_analysis st ct ft body in
        (path, args, ret_t, body) :: rest, rt
    | [] -> [], rt


(* TODO *)
let flatten_tree tree = []

(*The order of the checking and building of symbol tables may need to change
    to allow functions to be Hoisted
    NOTE: route_list is of type: "route"
    *)
let gen_semantic_program stmts classes funcs h_tree =
    (* build an unsafe semantic AST *)
    let s_stmts = List.map translate_statement stmts in
    let s_http_tree = translate_http_tree h_tree in
    let s_funcs = List.map (translate_function None) funcs in
    let checked_classes, ct, sclass_funcs = gen_class_stmts classes in
    let s_funcs = sclass_funcs @ s_funcs in
    let dft = default_ft empty_function_table in
    let ft = build_function_table dft s_funcs in
    (* typecheck and reclassify all variable usage *)
    let checked_stmts = var_analysis symbol_table_list ct ft s_stmts in
    (*Add all the var decls to the global scope*)
    let st = add_to_scope symbol_table_list s_stmts in
    (*typecheck all the functions (including args and returns)*)
    let ctx = (new_scope symbol_table_list, ct, ft) in
    let checked_funcs = check_funcs st ct ft s_funcs in
    let route_list, _ = validate_http_tree "" [] empty_route_table ctx s_http_tree in
    (checked_stmts, checked_classes, checked_funcs, route_list)


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, classes, funcs, h_tree) = ast in
    let stmts = List.rev stmts in
    gen_semantic_program stmts classes funcs h_tree

