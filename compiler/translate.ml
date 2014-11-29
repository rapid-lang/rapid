open Sast;;
open Sast_printer;;
open Sast_helper;;

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

let rec translate_expr = function
    (* TODO: a ton more types here, also support expressions *)
    | Ast.IntLit i    -> SExprInt(SIntExprLit i)
    | Ast.StringLit s -> SExprString(SStringExprLit s)
    | _ -> raise UnsupportedExpressionType

let translate_assign id xpr = match translate_expr xpr with
    | SExprInt i    -> IntAssign(id, i)
    | SExprString s -> StringAssign(id, s)
    | _ -> raise UnsupportedDeclType

let translate_decl = function
    | (Ast.Int, id, i_xpr) -> IntAssignDecl(id, translate_if_exists translate_int_xpr i_xpr)
    | (Ast.String, id, s_xpr) -> StringAssignDecl(id, translate_if_exists translate_string_xpr s_xpr)
    | (t, _, _) -> raise(UnsupportedStatementTypeErr (Ast_helper.string_of_t t))
    | _ -> raise UnsupportedDeclType

let translate_output = function
    | Ast.Println(xpr_l) -> SPrintln(List.map translate_expr xpr_l)
    | _ -> raise(UnsupportedOutputType("Not yet implemented"))

let translate_statement = function
    | Ast.VarDecl vd -> SDecl(translate_decl vd)
    | Ast.Assign(id, xpr) -> SAssign(translate_assign id xpr)
    | Ast.Output o -> SOutput(translate_output o)
    | _ -> raise(UnsupportedStatementTypeErr "type unknown")

