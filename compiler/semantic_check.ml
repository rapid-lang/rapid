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



(* checks that an assignment has the proper types *)
let check_var_assign_use sym_tbl assign =
    let (id, _) = assign in
    let t = get_type sym_tbl id in
    match (t, assign) with
        | Int, (_, SExprInt _) -> sym_tbl
        | String, (_, SExprString _) -> sym_tbl
        | t , _ ->  raise(InvalidTypeReassignErr(Format.sprintf "Expected %s expression" (Ast_helper.string_of_t t)))


(* Takes a sexpr and reclassifies SId *)
let classify_sid sym_tbl = function
    | SId id -> (match get_type sym_tbl id with
        | Int    -> SExprInt(SIntVar id)
        | String -> SExprString(SStringVar id)
        (* TODO: remaining datatypes *)
        | _ -> raise UnsupportedSexpr)
    | s -> s


let rec check_types st = function
    | SAssign(a) :: tl   -> check_types (check_var_assign_use st a) tl
    | _ :: tl -> check_types st tl
    | [] -> st


let rec get_decls st = function
    | SDecl(t, (id, _)) :: tl -> get_decls (add_sym st t id) tl
    | _ :: tl -> get_decls st tl
    | [] -> st


let gen_semantic_stmts stmts =
    (* build the semantic AST *)
    let s_stmts = List.map translate_statement stmts in

    (* build a basic symbol table checking for redeclarations *)
    let sym = get_decls empty_symbol_table s_stmts in

    (* reclassify Sid into appropriate sexpr *)
    let s_stmts = s_stmts in
    s_stmts



let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, _) = ast in
    let stmts = List.rev stmts in
    gen_semantic_stmts stmts


let string_of_sast sast =
    let strs = List.map semantic_stmt_s sast in
    String.concat "" strs

