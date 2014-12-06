open Format
open Sast
open Sast_printer
open Datatypes

exception UnsupportedSemanticExpressionType of string
exception UnsupportedSemanticStatementType
exception UnsupportedStringExprType
exception UnsupportedIntExprType
exception UnsupportedFloatExprType
exception UnsupportedOutputType
exception UnsupportedSExprType
exception UnsupportedBoolExprType
exception UnsupportedDeclType of string



let int_expr_to_code = function
    | SIntExprLit i -> sprintf "(%d)" i
    | SIntVar id -> sprintf "(%s)" id
    | _ -> raise UnsupportedIntExprType


let float_expr_to_code = function
    | SFloatExprLit f -> sprintf "(%f)" f
    | SFloatVar id -> sprintf "(%s)" id
    | _ -> raise UnsupportedFloatExprType


let string_expr_to_code = function
    | SStringExprLit s -> sprintf "(\"%s\")" s
    | SStringVar id -> sprintf "(%s)" id
    | _ -> raise UnsupportedStringExprType

let bool_expr_to_code = function
    | SBoolExprLit b -> sprintf "(%b)" b
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
    | Int, SExprInt xpr -> sprintf "var %s int = %s\n_ = %s"
        id (int_expr_to_code xpr) id
    | Float, SExprFloat xpr -> sprintf "var %s float64 = %s\n_ = %s"
        id (float_expr_to_code xpr) id
    | String, SExprString xpr -> sprintf "var %s string = %s\n_ = %s"
        id (string_expr_to_code xpr) id
    | Bool, SExprBool xpr -> sprintf "var %s bool = %s\n_ = %s"
        id (bool_expr_to_code xpr) id
    | _ -> raise(UnsupportedDeclType(svar_decl_s t (id, xpr)))


let soutput_to_code = function
    | SPrintln xpr_l -> sprintf "fmt.Println(%s)"
        (String.concat ", " (List.map sexpr_to_code xpr_l))
    | SPrintf(s, xpr_l) -> sprintf "fmt.Printf(%s, %s)"
        (sexpr_to_code s)
        (String.concat ", " (List.map sexpr_to_code xpr_l))
    | _ -> raise UnsupportedOutputType


let stmts_to_code = function
    | SDecl(t, (id, xpr)) -> sdecl_to_code (id, xpr) t
    | SAssign a -> sassign_to_code a
    | SOutput p -> soutput_to_code p
    | _ -> raise(UnsupportedSemanticStatementType)


let sattr_to_code = function
    | SOptional(t, id) | SNonOption(t, id, _) -> sprintf "%s %s"
        id
        (Ast_printer.string_of_t t)

let class_to_code (name, sattrs) =
    sprintf "type %s struct {\n%s\n}\n"
        name
        (String.concat "\n" (List.map sattr_to_code sattrs))


let skeleton = "package main\n" ^
    "import (\"fmt\")\n" ^
    "var _ = fmt.Printf\n" ^
    "func main() {\n"


let build_prog (stmts, classes) =
    let stmt_lines = List.map stmts_to_code stmts in
    let class_lines = List.map class_to_code classes in
    let gen_code_stmts = String.concat "\n" stmt_lines in
    let gen_code_classes = String.concat "\n" class_lines in
    skeleton ^ gen_code_classes ^ gen_code_stmts ^ "\n}\n"

