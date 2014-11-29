open Sast;;
open Sast_printer;;
open Sast_helper;;
open Format;;
open Translate;;

exception RepeatDeclarationErr of string
exception UncaughtCompareErr of string
exception UnsupportedStatementTypeErr of string
exception UndeclaredVarErr of string
exception InvalidTypeReassignErr of string
exception UnsupportedExpressionType



let check_var_assign_use sym_tbl assign =
    let id = id_from_assign assign in
    let t = get_type sym_tbl id in
    match (t, assign) with
        | Int, IntAssign _ -> sym_tbl
        | String, StringAssign _ -> sym_tbl
        | Int, _ -> raise(InvalidTypeReassignErr("Expected Int expression"))
        | String, _ -> raise(InvalidTypeReassignErr("Expected String expression"))
        | _ -> raise(UnsupportedExpressionType)


let rec get_symbols st = function
    | SDecl vd :: tl -> get_symbols (add_sym st vd) tl
    | SAssign(a) :: tl   -> get_symbols (check_var_assign_use st a) tl
    | _ :: tl -> get_symbols st tl
    | [] -> st


let gen_semantic_stmts stmts =
    let s_stmts = List.map translate_statement stmts in
    let sym = get_symbols empty_symbol_table s_stmts in
    List.map translate_statement stmts


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, _) = ast in
    let stmts = List.rev stmts in
    gen_semantic_stmts stmts


let string_of_sast sast =
    let strs = List.map semantic_stmt_s sast in
    String.concat "" strs

