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
exception UnsupportedExpressionType
exception UnsupportedSexpr
exception UnsupportedDatatypeErr
exception StringDatatypeRequiredErr
exception InvalidArgErr



(* Takes a symbol table and sexpr and rewrites variable references to be typed *)
let rec rewrite_sexpr st = function
    | SId id -> (
        match get_type id st with
        | Int -> SExprInt(SIntVar id)
        | String -> SExprString(SStringVar id)
        | Float -> SExprFloat(SFloatVar id)
        | Bool -> SExprBool(SBoolVar id)
        | _ -> raise UnsupportedDatatypeErr)
    (* TODO: add all new expressions that can contain variable references to be simplified *)
    | xpr -> xpr


(* Takes a type and a typed sexpr and confirms it is the proper type *)
let check_t_sexpr expected_t xpr =
    let found_t = sexpr_to_t expected_t xpr in
    if found_t = expected_t
        then ()
        else raise(InvalidTypeErr(Format.sprintf "Expected %s expression, found %s"
            (Ast_printer.string_of_t expected_t)
            (Ast_printer.string_of_t found_t)))


(* typechecks a sexpr *)
let rewrite_sexpr_to_t st xpr t =
    let typed_xpr = rewrite_sexpr st xpr in
    let () = check_t_sexpr t typed_xpr in
    typed_xpr


(* checks that an assignment has the proper types *)
let check_var_assign_use sym_tbl id xpr =
    let var_t = (get_type id sym_tbl) in
    let () = check_t_sexpr var_t xpr in
    sym_tbl


(* rewrites any sexprs in an SOutput statement *)
let check_s_output sym_tbl = function
    | SPrintf(s, xpr_l) ->
        let format_str = rewrite_sexpr_to_t sym_tbl s String in
        SPrintf(format_str, List.map (rewrite_sexpr sym_tbl) xpr_l)
    | SPrintln(xpr_l) -> SPrintln(List.map (rewrite_sexpr sym_tbl) xpr_l)


(* Processes an unsafe SAST and returns a type checked SAST *)
let rec var_analysis st = function
    | SDecl(t, (id, xpr)) :: tl ->
        let expr = rewrite_sexpr st xpr in
        let st = add_sym t id st in
        let () = check_t_sexpr t expr in
            SDecl(t, (id, expr)) :: var_analysis st tl
    | SAssign(id, xpr) :: tl ->
        let expr = rewrite_sexpr st xpr in
        let st = check_var_assign_use st id expr in
            SAssign(id, expr) :: (var_analysis st tl)
    | SOutput(so) :: tl ->
        let so = check_s_output st so in
            SOutput(so) :: (var_analysis st tl)
    | SReturn(s) :: tl -> SReturn(s) :: (var_analysis st tl)
    | [] -> []

let rec add_to_scope st = function
    | SDecl(t, (id, xpr)) :: tl ->
       let st = add_sym t id st in
       let st = add_to_scope st tl in
       st
    | _ :: _ -> raise InvalidArgErr
    | [] -> st

let args_to_type = function
    | SDecl(t, (id, xpr)) -> t
    | _ -> raise InvalidArgErr

let rec check_funcs st ft = function
    | (fname, args, rets, body) :: tl ->
        let arg_ts = List.map args_to_type args in
        let ft = add_func ft fname arg_ts rets in
        let scoped_st = new_scope st in
        let targs = var_analysis scoped_st args in
        let scoped_st = add_to_scope scoped_st args in
        let tbody = var_analysis scoped_st body in
        (*TODO: check the return type matches the return statement*)
        (fname, targs, rets, tbody) :: check_funcs st ft tl
    | [] -> []


let gen_semantic_program stmts funcs =
    (* build an unsafe semantic AST *)
    let s_stmts = List.map translate_statement stmts in
    let s_funcs = List.map translate_function funcs in
    let checked_funcs = check_funcs symbol_table_list empty_function_table s_funcs in
    (* typecheck and reclassify all variable usage *)
    let checked_stmts = var_analysis symbol_table_list s_stmts in
    (checked_stmts, checked_funcs)


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, funcs) = ast in
    let stmts = List.rev stmts in
    gen_semantic_program stmts funcs

