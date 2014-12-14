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
    | _ -> raise InvalidBoolExprType


(* TODO: a ton more types here, also support recursive expressions *)
let rec translate_expr = function
    | Ast.IntLit i               -> SExprInt(SIntExprLit i)
    | Ast.StringLit s            -> SExprString(SStringExprLit s)
    | Ast.FloatLit f             -> SExprFloat(SFloatExprLit f)
    | Ast.BoolLit b              -> SExprBool(SBoolExprLit b)
    | Ast.UserDefInst(nm, actls) -> translate_user_def_inst nm actls
    | Ast.Access(e, mem)         -> translate_access e mem
    (* we put a placeholder with the ID in and check after and reclassify *)
    | Ast.Id id                  -> SId id
    | _ -> raise UnsupportedExpressionType
and translate_user_def_inst class_id actls =
    SExprUserDef (SUserDefInst
        (UserDef class_id, (List.map translate_actual actls)))
and translate_actual = function
    | Ast.Actual(nm, xpr) -> SActual(nm, (translate_expr xpr))
and translate_access xpr mem =
    SExprAccess((translate_expr xpr), mem)


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

let translate_user_def_decl = function
    | class_id, id, xpr -> SUserDefDecl (class_id, (id, (expr_option_map translate_expr xpr)))

let translate_output = function
    | Ast.Println xpr_l -> SPrintln(List.map translate_expr xpr_l)
    | Ast.Printf(format :: xpr_l) -> SPrintf(translate_expr format, List.map translate_expr xpr_l)
    | _ -> raise(UnsupportedOutputType("Not yet implemented"))

let translate_statement = function
    | Ast.VarDecl vd -> translate_decl vd
    | Ast.Assign(id, xpr) -> SAssign(id, translate_expr xpr)
    | Ast.Output o -> SOutput(translate_output o)
    | Ast.UserDefDecl udd -> translate_user_def_decl udd
    | _ -> raise(UnsupportedStatementTypeErr "type unknown")

let translate_attr = function
    | Ast.NonOption (t, name, Some(xpr)) -> SNonOption(t, name, Some(translate_expr xpr))
    | Ast.NonOption (t, name, None) -> SNonOption(t, name, None)
    | Ast.Optional (t, name) -> SOptional(t, name)

let translate_class (name, attrs) =
    name,
    (List.map translate_attr attrs)


