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
exception UnsupportedDatatypeErr
exception StringExpressionsRequired


let rand_var_gen _ = "tmp_" ^ Int64.to_string (Random.int64 Int64.max_int)

let go_type_from_type = function
    | Int -> "Int"
    | Float -> "Float"
    | Bool -> "Bool"
    | String -> "String"
    | _ -> raise UnsupportedDatatypeErr

(* must return a direct reference to a string *)
let get_string_literal_from_sexpr = function
    | SExprString(SStringExprLit s) -> sprintf "(\"%s\")" s
    | SExprString(SStringVar id) -> sprintf "*%s" id
    | _ -> raise StringExpressionsRequired

(* returns a reference to a integer *)
let int_expr_to_code = function
    | SIntExprLit i ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := %d" tmp_var i,
            sprintf "&%s" tmp_var
    | SIntVar id ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := *%s" tmp_var id,
            sprintf "&%s" tmp_var
    | SIntNull -> "", "nil"
    | _ -> raise UnsupportedIntExprType

(* returns a reference to a float *)
let float_expr_to_code = function
    | SFloatExprLit f ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := %f" tmp_var f,
            sprintf "&%s" tmp_var
    | SFloatVar id ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := *%s" tmp_var id,
            sprintf "&%s" tmp_var
    | SFloatNull -> "", "nil"
    | _ -> raise UnsupportedFloatExprType

(* returns a reference to a string *)
let string_expr_to_code = function
    | SStringExprLit s ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := \"%s\"" tmp_var s,
            sprintf "&%s" tmp_var
    | SStringVar id ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := *%s" tmp_var id,
            sprintf "&%s" tmp_var
    | SStringNull -> "", "nil"
    | _ -> raise UnsupportedStringExprType

(* returns a reference to a boolean *)
let bool_expr_to_code = function
    | SBoolExprLit b ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := %b" tmp_var b,
            sprintf "&%s" tmp_var
    | SBoolVar id ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := *%s" tmp_var id,
            sprintf "&%s" tmp_var
    | SBoolNull -> "", "nil"
    | _ -> raise UnsupportedBoolExprType

let sexpr_to_code = function
    | SExprInt i -> int_expr_to_code i
    | SExprString s -> string_expr_to_code s
    | SExprFloat f -> float_expr_to_code f
    | SExprBool b -> bool_expr_to_code b
    | _ -> raise UnsupportedSExprType

let sassign_to_code = function
    | (id, xpr) ->
        let setup, ref = sexpr_to_code xpr in
        sprintf "%s\n%s = %s" setup id ref
    | a -> raise(UnsupportedSemanticExpressionType(
        sprintf "Assignment expression not yet supported -> %s"
        (svar_assign_s a)))


let sdecl_to_code (id, xpr) t = match t, xpr with
    | Int, SExprInt xpr ->
        let setup, ref = int_expr_to_code xpr in
            sprintf "var %s Int\n_=%s\n%s\n%s = %s"
                id id setup id ref
    | Float, SExprFloat xpr ->
        let setup, ref = float_expr_to_code xpr in
            sprintf "var %s Float\n_=%s\n%s\n%s = %s"
                id id setup id ref
    | String, SExprString xpr ->
        let setup, ref = string_expr_to_code xpr in
            sprintf "var %s String\n_=%s\n%s\n%s = %s"
                id id setup id ref
    | Bool, SExprBool xpr ->
        let setup, ref = bool_expr_to_code xpr in
            sprintf "var %s Bool\n_=%s\n%s\n%s = %s"
                id id setup id ref
    | (Int | Float | String | Bool ) as t, NullExpr ->
        sprintf "var %s %s\n_ = %s" id (go_type_from_type t) id
    | _ -> raise(UnsupportedDeclType(svar_decl_s t (id, xpr)))

let soutput_to_code = function
    | SPrintln xpr_l ->
        let trans = List.map sexpr_to_code xpr_l in
        let setups = List.map (fun (s, _) -> s) trans in
        let refs = List.map (fun (_, r) -> r) trans in
            sprintf "%s\nfmt.Println(%s)"
                (String.concat "\n" setups)
                (String.concat "," (List.map (fun r -> "*"^r) refs))
    | SPrintf(s, xpr_l) ->
        let format_str = get_string_literal_from_sexpr s in
        let trans = List.map sexpr_to_code xpr_l in
        let setups = List.map (fun (s, _) -> s) trans in
        let refs = List.map (fun (_, r) -> r) trans in
            sprintf "%s\nfmt.Printf(%s, %s)"
                (String.concat "\n" setups)
                format_str
                (String.concat "," (List.map (fun r -> "*"^r) refs))
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

