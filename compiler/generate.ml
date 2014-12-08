open Format
open Sast
open Sast_printer
open Datatypes
open Str

exception UnsupportedSemanticExpressionType of string
exception UnsupportedSemanticStatementType
exception UnsupportedStringExprType
exception UnsupportedIntExprType
exception UnsupportedFloatExprType
exception UnsupportedOutputType
exception UnsupportedSExprType
exception UnsupportedBoolExprType
exception UnsupportedDeclType of string
exception StringExpressionsRequired


let get_string_literal_from_sexpr = function
    | SExprString s -> (match s with
        | SStringExprLit s -> sprintf "(\"%s\")" s
        | SStringVar id -> sprintf "(%s.val)" id
        | SStringNull -> "NULL")
    | _ -> raise StringExpressionsRequired


let int_expr_to_code = function
    | SIntExprLit i -> sprintf "(IntOpt{val:%d})" i
    | SIntVar id -> sprintf "(%s)" id
    | _ -> raise UnsupportedIntExprType

let float_expr_to_code = function
    | SFloatExprLit f -> sprintf "(FloatOpt{val:%f})" f
    | SFloatVar id -> sprintf "(%s)" id
    | _ -> raise UnsupportedFloatExprType

let string_expr_to_code = function
    | SStringExprLit s -> sprintf "(StringOpt{val:\"%s\"})" s
    | SStringVar id -> sprintf "(%s)" id
    | _ -> raise UnsupportedStringExprType

let bool_expr_to_code = function
    | SBoolExprLit b -> sprintf "(BoolOpt{val:%b})" b
    | SBoolVar id -> sprintf "(%s)" id
    | _ -> raise UnsupportedBoolExprType

let sexpr_to_code = function
    | SExprInt i -> int_expr_to_code i
    | SExprString s -> string_expr_to_code s
    | SExprFloat f -> float_expr_to_code f
    | SExprBool b -> bool_expr_to_code b
    | _ -> raise UnsupportedSExprType

let sassign_to_code = function
    | (id, xpr) -> sprintf "%s = %s" id (sexpr_to_code xpr)
    | a -> raise(UnsupportedSemanticExpressionType(
        sprintf "Assignment expression not yet supported -> %s"
        (svar_assign_s a)))

let sdecl_to_code (id, xpr) t = match t, xpr with
    | Int, SExprInt xpr -> sprintf "%s := %s\n_ = %s"
        id (int_expr_to_code xpr) id
    | Float, SExprFloat xpr -> sprintf "%s := %s\n_ = %s"
        id (float_expr_to_code xpr) id
    | String, SExprString xpr -> sprintf "%s := %s\n_ = %s"
        id (string_expr_to_code xpr) id
    | Bool, SExprBool xpr -> sprintf "%s := %s\n_ = %s"
        id (bool_expr_to_code xpr) id
    | _ -> raise(UnsupportedDeclType(svar_decl_s t (id, xpr)))

let soutput_to_code = function
    | SPrintln xpr_l -> sprintf "fmt.Println(%s)"
        (String.concat ", " (List.map sexpr_to_code xpr_l))
    | SPrintf(s, xpr_l) -> sprintf "fmt.Printf(properFormatString(%s), %s)"
        (get_string_literal_from_sexpr s)
        (String.concat ", " (List.map sexpr_to_code xpr_l))
    | _ -> raise UnsupportedOutputType

let sast_to_code = function
    | SDecl(t, (id, xpr)) -> sdecl_to_code (id, xpr) t
    | SAssign a -> sassign_to_code a
    | SOutput p -> soutput_to_code p
    | _ -> raise(UnsupportedSemanticStatementType)

let skeleton = "package main\n" ^
    "import (\"fmt\")\n" ^
    "var _ = fmt.Printf\n" ^
    "func main() {\n"

let build_prog sast =
    let code_lines = List.map sast_to_code sast in
    let gen_code = String.concat "\n" code_lines in
    skeleton ^ gen_code ^ "\n}\n"

