open Format;;
open Sast;;
open Sast_printer;;


exception UnsupportedSemanticExpressionType of string
exception UnsupportedSemanticStatementType
exception UnsupportedIntExprType
exception UnsupportedSExprType
exception UnsupportedOutputType
exception UnsupportedDeclType of string


let skeleton = sprintf "%s\n%s\n%s\n%s"
    "package main"
    "import (\"fmt\")"
    "var _ = fmt.Printf"
    "func main() {\n"


let int_expr_to_code = function
    | SIntExprLit i -> sprintf "(%d)" i
    | _ -> raise UnsupportedIntExprType


let sexpr_to_code = function
    | SExprInt(i) -> int_expr_to_code i
    | _ -> raise UnsupportedSExprType


let sassign_to_code = function
    | IntAssign(id, xpr) -> sprintf "%s = %s" id (int_expr_to_code xpr)
    | a -> raise(UnsupportedSemanticExpressionType(sprintf "Assignment expression not yet supported -> %s" (svar_assign_s a)))


let sdecl_to_code = function
    | IntAssignDecl(id, Some xpr) -> sprintf "var %s int = %s\n_ = %s" id (int_expr_to_code xpr) id
    | IntAssignDecl(id, None) -> sprintf "var %s int\n_ = %s" id id
    | a -> raise(UnsupportedDeclType(svar_decl_s a))


let soutput_to_code = function
    | SPrintln(xpr_l) ->
        sprintf "fmt.Println(%s)" (String.concat ", " (List.map sexpr_to_code xpr_l))
    | _ -> raise UnsupportedOutputType


let sast_to_code = function
    | SDecl d -> sdecl_to_code d
    | SAssign a -> sassign_to_code a
    | SOutput p -> soutput_to_code p
    | _ -> raise(UnsupportedSemanticStatementType)


let build_prog sast =
    let code_lines = List.map sast_to_code sast in
    let gen_code = String.concat "\n" code_lines in
    (* ERROR: sprintf skeleton gen_code *)
    skeleton ^ gen_code ^ "\n}\n"

