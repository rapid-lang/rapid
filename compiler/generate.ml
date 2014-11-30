open Format
open Sast
open Sast_printer
open Datatypes

exception UnsupportedSemanticExpressionType of string
exception UnsupportedSemanticStatementType
exception UnsupportedStringExprType
exception UnsupportedIntExprType
exception UnsupportedOutputType
exception UnsupportedSExprType
exception UnsupportedDeclType of string



let int_expr_to_code = function
    | SIntExprLit i -> sprintf "(%d)" i
    | _ -> raise UnsupportedIntExprType


let string_expr_to_code = function
    | SStringExprLit s -> sprintf "(\"%s\")" s
    | _ -> raise UnsupportedStringExprType


let sexpr_to_code = function
    | SExprInt i -> int_expr_to_code i
    | SExprString s -> string_expr_to_code s
    | _ -> raise UnsupportedSExprType


let sassign_to_code = function
    | (id, xpr) -> sprintf "%s = %s" id (sexpr_to_code xpr)
    | a -> raise(UnsupportedSemanticExpressionType(
        sprintf "Assignment expression not yet supported -> %s"
        (svar_assign_s a)))


let sdecl_to_code (id, xpr) t = match t, xpr with
    | Int, SExprInt xpr -> sprintf "var %s int = %s\n_ = %s"
        id (int_expr_to_code xpr) id
    | String, SExprString xpr -> sprintf "var %s string = %s\n_ = %s"
        id (string_expr_to_code xpr) id
    | _ -> raise(UnsupportedDeclType(svar_decl_s t (id, xpr)))


let soutput_to_code = function
    | SPrintln(xpr_l) -> sprintf "fmt.Println(%s)"
        (String.concat ", " (List.map sexpr_to_code xpr_l))
    | _ -> raise UnsupportedOutputType


let sast_to_code = function
    | SDecl(t, (id, xpr)) -> sdecl_to_code (id, xpr) t
    | SAssign a -> sassign_to_code a
    | SOutput p -> soutput_to_code p
    | _ -> raise(UnsupportedSemanticStatementType)


let skeleton = sprintf "%s\n%s\n%s\n%s"
    "package main"
    "import (\"fmt\")"
    "var _ = fmt.Printf"
    "func main() {\n"


let build_prog sast =
    let code_lines = List.map sast_to_code sast in
    let gen_code = String.concat "\n" code_lines in
    (* ERROR: sprintf skeleton gen_code *)
    skeleton ^ gen_code ^ "\n}\n"

