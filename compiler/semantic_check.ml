open Sast
open Sast_helper
open Sast_printer
open Format
open Datatypes
open Translate

exception RepeatDeclarationErr of string
exception UncaughtCompareErr of string
exception UnsupportedStatementTypeErr of string
exception UndeclaredVarErr of string
exception InvalidTypeReassignErr of string
exception UnsupportedExpressionType
exception UnsupportedSexpr
exception UnsupportedDatatypeErr
exception StringDatatypeRequiredErr


(* Takes a symbol table and sexpr and rewrites variable references to be typed *)
let rec rewrite_sexpr st = function
    | SId id -> (
        match get_type id st with
        | Int -> SExprInt(SIntVar id)
        | String -> SExprString(SStringVar id)
        | Bool -> SExprBool(SBoolVar id)
        | _ -> raise UnsupportedDatatypeErr)
    (* TODO: add all new expressions that can contain variable references to be simplified *)
    | xpr -> xpr


let rewrite_string_sexpr st = function
    | SId id -> (
        match get_type id st with
        | String -> SExprString(SStringVar id)
        | _ -> raise StringDatatypeRequiredErr)
    | SExprString xpr -> SExprString xpr
    | _ -> raise UnsupportedSexpr


(* checks that an assignment has the proper types *)
let check_var_assign_use sym_tbl id xpr =
    let t = get_type id sym_tbl in
    match t, xpr with
        | Int, SExprInt _ -> sym_tbl
        | String, SExprString _ -> sym_tbl
        | Bool, SExprBool _ -> sym_tbl
        | t , _ ->  raise(InvalidTypeReassignErr(
            Format.sprintf "Expected %s expression" (Ast_printer.string_of_t t)))


(* rewrites any sexprs in an SOutput statement *)
let check_s_output sym_tbl = function
    | SPrintf(s, xpr_l) -> SPrintf((rewrite_string_sexpr sym_tbl s), List.map (rewrite_sexpr sym_tbl) xpr_l)
    | SPrintln(xpr_l) -> SPrintln(List.map (rewrite_sexpr sym_tbl) xpr_l)


(* Processes an unsafe SAST and returns a type checked SAST *)
let rec var_analysis st = function
    | SDecl(t, (id, xpr)) :: tl ->
        let expr = rewrite_sexpr st xpr in
        let st = add_sym t id st in
            SDecl(t, (id, expr)) :: var_analysis st tl
    | SAssign(id, xpr) :: tl ->
        let expr = rewrite_sexpr st xpr in
        let st = check_var_assign_use st id expr in
            SAssign(id, expr) :: (var_analysis st tl)
    | SOutput(so) :: tl ->
        let so = check_s_output st so in
            SOutput(so) :: (var_analysis st tl)
    | [] -> []


let gen_semantic_stmts stmts =
    (* build an unsafe semantic AST *)
    let s_stmts = List.map translate_statement stmts in
    (* typecheck and reclassify all variable usage *)
    let checked_stmts = var_analysis symbol_table_list s_stmts in
    checked_stmts


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, _) = ast in
    let stmts = List.rev stmts in
    gen_semantic_stmts stmts


