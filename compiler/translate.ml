open Sast;;
open Sast_printer;;
open Sast_helper;;
open Datatypes;;

exception UnsupportedStatementTypeErr of string
exception UnsupportedOutputType of string
exception UnsupportedExpressionType
exception InvalidStringExprType
exception UnsupportedDeclType
exception InvalidIntExprType


let translate_int_xpr = function
    | Ast.IntLit i -> SIntExprLit i
    | _ -> raise InvalidIntExprType

let translate_string_xpr = function
    | Ast.StringLit s -> SStringExprLit s
    | _ -> raise InvalidStringExprType

(* we put a placeholder with the ID in and check after *)
(*
let translate_id_xpr id = match get_type st id with
    | Int -> SExprInt(SIntVar id)
    | String -> SExprString(SStringVar id)
    | _ -> raise UnsupportedExpressionType
*)

let rec translate_expr = function
    (* TODO: a ton more types here, also support recursive expressions *)
    | Ast.IntLit i    -> SExprInt(SIntExprLit i)
    | Ast.StringLit s -> SExprString(SStringExprLit s)
    | Ast.Id id       -> SId id
    | _ -> raise UnsupportedExpressionType

let translate_assign id xpr = match translate_expr xpr with
    | SExprInt _    -> (id, xpr)
    | SExprString _ -> (id, xpr)
    | SId _         -> (id, xpr)
    | _ -> raise UnsupportedExpressionType

let translate_decl = function
    | Int, id, i_xpr_opt -> SDecl(Int, (id, expr_option_map translate_expr i_xpr_opt))
    | String, id, s_xpr_opt -> SDecl(String, (id, expr_option_map translate_expr s_xpr_opt))
    | t, _, _ -> raise(UnsupportedStatementTypeErr (Ast_helper.string_of_t t))
    | _ -> raise UnsupportedDeclType

let translate_output = function
    | Ast.Println xpr_l -> SPrintln(List.map translate_expr xpr_l)
    | _ -> raise(UnsupportedOutputType("Not yet implemented"))

let translate_statement = function
    | Ast.VarDecl vd -> translate_decl vd
    | Ast.Assign(id, xpr) -> SAssign(id, translate_expr xpr)
    | Ast.Output o -> SOutput(translate_output o)
    | _ -> raise(UnsupportedStatementTypeErr "type unknown")

