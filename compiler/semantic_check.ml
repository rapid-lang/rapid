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


(* Takes a symbol table and sexpr and rewrites variable references to be typed *)
let rec rewrite_sexpr st = function
    | SId id -> (
        match get_type st id with
        | Int -> SExprInt(SIntVar id)
        | String -> SExprString(SStringVar id)
        | _ -> raise UnsupportedDatatypeErr)
    (* TODO: add all new expressions that can contain variable references to be simplified *)
    | xpr -> xpr


(* checks that an assignment has the proper types *)
let check_var_assign_use sym_tbl id xpr =
    let t = get_type sym_tbl id in
    match t, xpr with
        | Int, SExprInt _ -> sym_tbl
        | String, SExprString _ -> sym_tbl
        | t , _ ->  raise(InvalidTypeReassignErr(Format.sprintf "Expected %s expression" (Ast_helper.string_of_t t)))


let check_s_output sym_tbl = function
    | SPrintf(s, xpr_l) -> SPrintf((rewrite_sexpr sym_tbl s), List.map (rewrite_sexpr sym_tbl) xpr_l)
    | SPrintln(xpr_l) -> SPrintln(List.map (rewrite_sexpr sym_tbl) xpr_l)


let rec var_analysis checked st = function
    | SDecl(t, (id, xpr)) :: tl ->
        let expr = rewrite_sexpr st xpr in
        let st = add_sym st t id in
            var_analysis (SDecl(t, (id, expr)) :: checked) st tl
    | SAssign(id, xpr) :: tl ->
        let expr = rewrite_sexpr st xpr in
        let st = check_var_assign_use st id expr in
            var_analysis (SAssign(id, expr) :: checked) st tl
    | SOutput(so) :: tl ->
        let so = check_s_output st so in
            var_analysis (SOutput(so) :: checked) st tl
    | [] -> checked


let gen_semantic_stmts stmts =
    (* build the semantic AST with generalized variable references *)
    let s_stmts = List.map translate_statement stmts in

    (* build a basic symbol table checking and reclassifying all variable usage *)
    let checked_stmts = var_analysis [] empty_symbol_table s_stmts in

    checked_stmts


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, _) = ast in
    let stmts = List.rev stmts in
    gen_semantic_stmts stmts


let string_of_sast sast =
    let strs = List.map semantic_stmt_s sast in
    String.concat "" strs

