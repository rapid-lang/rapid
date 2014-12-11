open Sast;;
open Sast_printer;;
open Sast_helper;;
open Datatypes;;

exception UnsupportedStatementTypeErr of string
exception UnsupportedOutputType of string
exception UnsupportedExpressionType
exception InvalidStringExprType
exception InvalidBoolExprType
exception UnsupportedDeclType
exception InvalidIntExprType
exception InvalidFloatExprType


let translate_int_xpr = function
    | Ast.IntLit i -> SIntExprLit i
    | _ -> raise InvalidIntExprType

let translate_float_xpr = function
    | Ast.FloatLit i -> SFloatExprLit i
    | _ -> raise InvalidIntExprType

let translate_string_xpr = function
    | Ast.StringLit s -> SStringExprLit s
    | _ -> raise InvalidStringExprType

let translate_bool_xpr = function
    | Ast.BoolLit b -> SBoolExprLit b
    | Ast.CastBool c  -> SBoolCast c
    | _ -> raise InvalidBoolExprType

let rec translate_expr = function
    (* TODO: a ton more types here, also support recursive expressions *)
    | Ast.IntLit i    -> SExprInt(SIntExprLit i)
    | Ast.StringLit s -> SExprString(SStringExprLit s)
    | Ast.FloatLit f  -> SExprFloat(SFloatExprLit f)
    | Ast.BoolLit b   -> SExprBool(SBoolExprLit b)
    | Ast.CastBool c  -> SExprBool(SBoolCast c)
    (* we put a placeholder with the ID in and check after and reclassify *)
    | Ast.Id id       -> SId id
    | _ -> raise UnsupportedExpressionType

let translate_assign id xpr = match translate_expr xpr with
    | SExprInt _    -> (id, xpr)
    | SExprString _ -> (id, xpr)
    | SExprBool _ -> (id, xpr)
    | SExprFloat _ ->  (id, xpr)
    | SId _         -> (id, xpr)
    | _ -> raise UnsupportedExpressionType

let translate_decl = function
    | (Int | String | Float | Bool) as t, id, i_xpr_opt ->
            SDecl(t, (id, expr_option_map translate_expr i_xpr_opt))
    | t, _, _ -> raise(UnsupportedStatementTypeErr (Ast_printer.string_of_t t))
    | _ -> raise UnsupportedDeclType

let translate_output = function
    | Ast.Println xpr_l -> SPrintln(List.map translate_expr xpr_l)
    | Ast.Printf(format :: xpr_l) -> SPrintf(translate_expr format, List.map translate_expr xpr_l)
    | _ -> raise(UnsupportedOutputType("Not yet implemented"))

let translate_statement = function
    | Ast.VarDecl vd -> translate_decl vd
    | Ast.Assign(id, xpr) -> SAssign(id, translate_expr xpr)
    | Ast.Output o -> SOutput(translate_output o)
    | _ -> raise(UnsupportedStatementTypeErr "type unknown")

