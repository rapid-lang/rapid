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
exception UnsupportedSExprType of string
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

let go_type_from_sexpr = function
    | SExprInt _ -> go_type_from_type Int
    | SExprFloat _ -> go_type_from_type Float
    | SExprBool _ -> go_type_from_type Bool
    | SExprString _ -> go_type_from_type String

(* must return a direct reference to a string *)
let get_string_literal_from_sexpr = function
    | SExprString(SStringExprLit s) -> sprintf "(\"%s\")" s
    | SExprString(SStringVar id) -> sprintf "*%s" id
    | _ -> raise StringExpressionsRequired

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


(* returns a reference to an integer *)
let rec int_expr_to_code = function
    | SIntExprLit i ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := %d" tmp_var i,
            sprintf "&%s" tmp_var
    | SIntVar id ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := *%s" tmp_var id,
            sprintf "&%s" tmp_var
    | SIntNull -> "", "nil"
    | SIntCast c -> int_cast_to_code c
    | _ -> raise UnsupportedIntExprType

(* returns a reference to a float *)
and float_expr_to_code = function
    | SFloatExprLit f ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := %f" tmp_var f,
            sprintf "&%s" tmp_var
    | SFloatVar id ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := *%s" tmp_var id,
            sprintf "&%s" tmp_var
    | SFloatCast c -> float_cast_to_code c
    | SFloatNull -> "", "nil"
    | _ -> raise UnsupportedFloatExprType

(* returns a reference to a boolean *)
and bool_expr_to_code = function
    | SBoolExprLit b ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := %b" tmp_var b,
            sprintf "&%s" tmp_var
    | SBoolVar id ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := *%s" tmp_var id,
            sprintf "&%s" tmp_var
    | SBoolCast c -> bool_cast_to_code c
    | SBoolNull -> "", "nil"
    | _ -> raise UnsupportedBoolExprType
and bool_cast_to_code xpr =
    let go_type = go_type_from_sexpr xpr in
    let setup, var = sexpr_to_code xpr in
    let cast = sprintf "%sToBool(%s(%s))" go_type go_type var in
    setup, cast
and float_cast_to_code xpr =
    let go_type = go_type_from_sexpr xpr in
    let setup, var = sexpr_to_code xpr in
    let cast = sprintf "%sToFloat(%s(%s))" go_type go_type var in
    setup, cast
and int_cast_to_code xpr =
    let go_type = go_type_from_sexpr xpr in
    let setup, var = sexpr_to_code xpr in
    let cast = sprintf "%sToInt(%s(%s))" go_type go_type var in
    setup, cast

and sexpr_to_code = function
    | SExprInt i -> int_expr_to_code i
    | SExprString s -> string_expr_to_code s
    | SExprFloat f -> float_expr_to_code f
    | SExprBool b -> bool_expr_to_code b
    | NullExpr -> "", "nil"
    | s -> raise(UnsupportedSExprType(Sast_printer.sexpr_s s))

let sassign_to_code = function
    | (id, xpr) ->
        let setup, ref = sexpr_to_code xpr in
        sprintf "%s\n%s = %s" setup id ref
    | a -> raise(UnsupportedSemanticExpressionType(
        sprintf "Assignment expression not yet supported -> %s"
        (svar_assign_s a)))

let soutput_to_code = function
    | SPrintln xpr_l ->
        let trans = List.map sexpr_to_code xpr_l in
        let setups = List.map (fun (s, _) -> s) trans in
        let refs = List.map (fun (_, r) -> "*"^r) trans in
            sprintf "%s\nfmt.Println(%s)"
                (String.concat "\n" setups)
                (String.concat "," refs)
    | SPrintf(s, xpr_l) ->
        let trans = List.map sexpr_to_code xpr_l in
        let setups = List.map (fun (s, _) -> s) trans in
        let refs = List.map (fun (_, r) -> "*"^r) trans in
            sprintf "%s\nfmt.Printf(%s, %s)"
                (String.concat "\n" setups)
                (get_string_literal_from_sexpr s)
                (String.concat "," refs)
    | _ -> raise UnsupportedOutputType

let sast_to_code = function
    | SDecl(_, (id, xpr)) -> sassign_to_code (id, xpr)
    | SAssign a -> sassign_to_code a
    | SOutput p -> soutput_to_code p
    | _ -> raise(UnsupportedSemanticStatementType)

let rec grab_decls = function
    | SDecl(t, (id, _)) :: tl ->
        sprintf "var %s %s" id (go_type_from_type t) :: grab_decls tl
    | _ :: tl -> grab_decls tl
    | [] -> []

let skeleton decls main fns = "package main\n import (\"fmt\")\n" ^
    "var _ = fmt.Printf\n" ^ decls ^ "\nfunc main() {\n" ^
    main ^ "\n}\n " ^ fns

let build_prog sast =
    let decls = String.concat "\n" (grab_decls sast) in
    let code_lines = List.map sast_to_code sast in
    let gen_code = String.concat "\n" code_lines in
    skeleton decls gen_code ""

