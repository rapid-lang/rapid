open Format;;
open Sast;;
open Sast_helper;;


exception UnsupportedSemanticExpressionType of string
exception UnsupportedSemanticStatementType
exception UnsupportedIntExprType of string


let skeleton = sprintf "%s\n%s\n%s\n%s"
    "package main"
    "import (\"fmt\")"
    "var _ = fmt.Printf"
    "func main() {"


let int_expr_to_code = function
    | SIntExprLit i -> sprintf "(%d)" i
    | _ -> raise(UnsupportedIntExprType "test")


let sassign_to_code = function
    | IntAssignDecl(id, Some xpr) -> sprintf "var %s int = %s\n_ = %s" id (int_expr_to_code xpr) id
    | IntAssignDecl(id, None) -> sprintf "var %s int\n_ = %s" id id
    | IntAssign(id, xpr) -> sprintf "%s = %s" id (int_expr_to_code xpr)
    | a -> raise(UnsupportedSemanticExpressionType(sprintf "Assignment expression not yet supported -> %s" (svar_assign_s a)))


let sast_to_code = function
    | SAssign(asgn) -> sassign_to_code asgn
    | _ -> raise(UnsupportedSemanticStatementType)


let build_prog sast =
    let code_lines = List.map sast_to_code sast in
    let gen_code = String.concat "\n" code_lines in
    (* ERROR: sprintf skeleton gen_code *)
    skeleton ^ gen_code ^ "\n}"

