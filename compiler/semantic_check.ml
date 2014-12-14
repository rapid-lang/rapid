open Sast
open Sast_helper
open Sast_printer
open Format
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



(* Takes a type and a typed sexpr and confirms it is the proper type *)
let check_t_sexpr expected_t xpr =
    let found_t = sexpr_to_t expected_t xpr in
    if found_t = expected_t
        then ()
        else raise(InvalidTypeErr(Format.sprintf "Expected %s expression, found %s"
            (Ast_printer.string_of_t expected_t)
            (Ast_printer.string_of_t found_t)))

(* Check that the attribute  *)
let check_attr sactuals_table = function
    | (name, (t, true, NullExpr)) ->
        if StringMap.mem name sactuals_table
            then let expr = StringMap.find name sactuals_table in
                let () = check_t_sexpr t expr in
                SActual(name, expr)
            else raise( MissingRequiredArgument
                            (Format.sprintf "Argument %s is missing" name))
    | (name, (t, false, xpr)) ->
        if StringMap.mem name sactuals_table
            then let expr = StringMap.find name sactuals_table in
                let () = check_t_sexpr t expr in
                SActual(name, expr)
            else SActual(name, xpr)


let check_user_def_inst ct t sactls =
    let sactuals_table = add_actls empty_actuals_table sactls in
    let attr_table = get_attr_table t ct in
    let checked_sactuals = List.map
        (check_attr sactuals_table)
        (StringMap.bindings attr_table) in
    SUserDefInst (UserDef t, checked_sactuals)


(* Takes a symbol table and sexpr and rewrites variable references to be typed *)
let rec rewrite_sexpr ct st = function
    | SId id -> (
        match get_type id st with
        | Int -> SExprInt(SIntVar id)
        | String -> SExprString(SStringVar id)
        | Float -> SExprFloat(SFloatVar id)
        | Bool -> SExprBool(SBoolVar id)
        | UserDef cls -> SExprUserDef(SUserDefVar ((UserDef cls), id))
        | _ -> raise UnsupportedDatatypeErr)
    | SExprUserDef udf -> (
        match udf with
        | SUserDefInst(UserDef t, sactls) ->
            let rewritten_sactls = List.map (rewrite_sactl ct st) sactls in
            let expr = check_user_def_inst ct t sactls in
            SExprUserDef(SUserDefInst(UserDef t, rewritten_sactls))
        | _ -> SExprUserDef udf
        )
    | SExprAccess(xpr, mem) ->
        let rewritten_sexpr = rewrite_sexpr ct st xpr in
        let cls = match rewritten_sexpr with
            | SExprUserDef(
                  SUserDefInst(UserDef s, _)
                | SUserDefVar(UserDef s, _)
                | SUserDefNull(UserDef s)) -> s
            | _ -> raise InvalidTypeMemberAccess in
        let t = get_attr_type cls ct mem in
        (match t with
            | Bool -> SExprBool(SBoolAcc(cls, mem))
            | Int -> SExprInt(SIntAcc(cls, mem))
            | Float -> SExprFloat(SFloatAcc(cls, mem))
            | String -> SExprString(SStringAcc(cls, mem))
            | UserDef ud -> SExprUserDef(SUserDefAcc(t, cls, mem)))
    (* TODO: add all new expressions that can contain variable references to be simplified *)
    | xpr -> xpr
and rewrite_sactl ct st = function
    | SActual(name, xpr) -> SActual(name, rewrite_sexpr ct st xpr)


(* typechecks a sexpr *)
let rewrite_sexpr_to_t ct st xpr t =
    let typed_xpr = rewrite_sexpr ct st xpr in
    let () = check_t_sexpr t typed_xpr in
    typed_xpr


(* checks that an assignment has the proper types *)
let check_var_assign_use sym_tbl id xpr =
    let var_t = (get_type id sym_tbl) in
    let () = check_t_sexpr var_t xpr in
    sym_tbl


(* rewrites any sexprs in an SOutput statement *)
let check_s_output ct sym_tbl = function
    | SPrintf(s, xpr_l) ->
        let format_str = rewrite_sexpr_to_t ct sym_tbl s String in
        SPrintf(format_str, List.map (rewrite_sexpr ct sym_tbl) xpr_l)
    | SPrintln(xpr_l) -> SPrintln(List.map (rewrite_sexpr ct sym_tbl) xpr_l)


(* Processes an unsafe SAST and returns a type checked SAST *)
let rec var_analysis st ct = function
    | SDecl(t, (id, xpr)) :: tl ->
        let expr = rewrite_sexpr ct st xpr in
        let st = add_sym t id st in
        let () = check_t_sexpr t expr in
            SDecl(t, (id, expr)) :: var_analysis st ct tl
    | SAssign(id, xpr) :: tl ->
        let expr = rewrite_sexpr ct st xpr in
        let st = check_var_assign_use st id expr in
            SAssign(id, expr) :: (var_analysis st ct tl)
    | SOutput(so) :: tl ->
        let so = check_s_output ct st so in
            SOutput(so) :: (var_analysis st ct tl)
    | SUserDefDecl(cls, (id, xpr)) :: tl ->
        let expr = rewrite_sexpr ct st xpr in
        let t = UserDef cls in
        let st = add_sym t id st in
        let () = check_t_sexpr t expr in
            SUserDefDecl(cls, (id, xpr)) :: var_analysis st ct tl
    | [] -> []


(* Prcesses unchecked classes, adding them and their attributes to class_tbl *)
let rec class_analysis class_tbl = function
    | (class_id, attrs) :: tl ->
        let attr_tbl = add_attrs empty_attribute_table attrs in
        let class_tbl = new_class class_id attr_tbl class_tbl in
        let lst, class_tbl = class_analysis class_tbl tl in
        ((class_id, attrs) :: lst), class_tbl
    | [] -> [], class_tbl


let gen_semantic_stmts ct stmts =
    (* build an unsafe semantic AST *)
    let s_stmts = List.map translate_statement stmts in
    (* typecheck and reclassify all variable usage *)
    let checked_stmts = var_analysis symbol_table_list ct s_stmts in
    checked_stmts


let gen_class_stmts stmts =
    let sclasses = List.map translate_class stmts in
    let (checked_sclasses, ct) = class_analysis class_table sclasses in
    checked_sclasses, ct


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, _, classes) = ast in
    let stmts = List.rev stmts in
    let typechecked_classes, ct = gen_class_stmts classes in
    let typechecked_statements = gen_semantic_stmts ct stmts in
    typechecked_statements, typechecked_classes

