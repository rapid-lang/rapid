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

let rec translate_expr = function
    (* TODO: a ton more types here, also support recursive expressions *)
    | Ast.IntLit i    -> SExprInt(SIntExprLit i)
    | Ast.StringLit s -> SExprString(SStringExprLit s)
    (* we put a placeholder with the ID in and check after and reclassify *)
    | Ast.Id id       -> SId id
    | _ -> raise UnsupportedExpressionType

let translate_decl = function
    | Int, id, i_xpr_opt -> SDecl(Int, (id, expr_option_map translate_expr i_xpr_opt))
    | String, id, s_xpr_opt -> SDecl(String, (id, expr_option_map translate_expr s_xpr_opt))
    | t, _, _ -> raise(UnsupportedStatementTypeErr (Ast_printer.string_of_t t))
    | _ -> raise UnsupportedDeclType

let translate_output = function
    | Ast.Println xpr_l -> SPrintln(List.map translate_expr xpr_l)
    | Ast.Printf(s, xpr_l) -> SPrintf(translate_expr s, List.map translate_expr xpr_l)
    | _ -> raise(UnsupportedOutputType("Not yet implemented"))

let translate_statement = function
    | Ast.VarDecl vd -> translate_decl vd
    | Ast.Assign(id, xpr) -> SAssign(id, translate_expr xpr)
    | Ast.Output o -> SOutput(translate_output o)
    | _ -> raise(UnsupportedStatementTypeErr "type unknown")

let translate_fstatement = function
    | Ast.FStmt stmt -> translate_statement stmt
    | Ast.Return expr -> SReturn(translate_expr expr)

let translate_function (f : Ast.func_decl) = 
    (
        f.fname, 
        (List.map translate_decl f.formals), 
        f.return, 
        (List.map translate_fstatement f.body)
    )
    

