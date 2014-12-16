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
exception UnsupportedListExprType
exception UnsupportedDeclType of string
exception UnsupportedDatatypeErr
exception StringExpressionsRequired


let rand_var_gen _ = "tmp_" ^ Int64.to_string (Random.int64 Int64.max_int)

let rec go_type_from_type = function
    | Int -> "Int"
    | Float -> "Float"
    | Bool -> "Bool"
    | String -> "String"
    | ListType(t) ->
        sprintf "%sList" (go_type_from_type t)
    | _ -> raise UnsupportedDatatypeErr
and go_tmp_type_from_type = function
    | ListType(t) ->
        sprintf "[]%s" (go_type_from_type t)
    | other_t -> go_type_from_type other_t

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

let op_to_code o = Ast_printer.bin_op_s o

(* top level function for generating sexprs *)
let rec sexpr_to_code = function
    | SExprInt i -> int_expr_to_code i
    | SExprString s -> string_expr_to_code s
    | SExprFloat f -> float_expr_to_code f
    | SExprBool b -> bool_expr_to_code b
    | SExprList l -> list_expr_to_code l
    | SCallTyped(t, (id, args)) -> func_expr_to_code id args
    | NullExpr -> "", "nil"
    | s -> raise(UnsupportedSExprType(Sast_printer.sexpr_s s))
(* returns a reference to a string *)
and string_expr_to_code = function
    | SStringExprLit s ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := \"%s\"" tmp_var s,
            sprintf "&%s" tmp_var
    | SStringVar id ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := *%s" tmp_var id,
            sprintf "&%s" tmp_var
    | SStringCast c -> cast_to_code String c
    | SStringNull -> "", "nil"
    | _ -> raise UnsupportedStringExprType
(* returns a reference to an integer *)
and int_expr_to_code = function
    | SIntExprLit i ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := %d" tmp_var i,
            sprintf "&%s" tmp_var
    | SIntVar id ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := *%s" tmp_var id,
            sprintf "&%s" tmp_var
    | SIntBinOp(lhs, o, rhs) -> bin_op_to_code lhs o rhs
    | SIntNull -> "", "nil"
    | SIntCast c -> cast_to_code Int c
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
    | SFloatBinOp(rhs, o, lhs) -> bin_op_to_code rhs o lhs
    | SFloatNull -> "", "nil"
    | SFloatCast c -> cast_to_code Float c
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
    | SBoolCast c -> cast_to_code Bool c
    (*once there is float expr to code then use that insdead of the first sexpr to code in 1rst 2 cases*)
    | SBoolBinOp(lhs, o, rhs) -> bin_op_to_code lhs o rhs
    | SBoolNull -> "", "nil"
    | _ -> raise UnsupportedBoolExprType
(* takes the destination type and the expression and creates the cast statement *)
and cast_to_code t xpr =
    let src_type = go_type_from_sexpr xpr in
    let dest_type = go_type_from_type t in
    let setup, var = sexpr_to_code xpr in
    let cast = sprintf "%sTo%s(%s)" src_type dest_type var in
    setup, cast
and func_expr_to_code id arg_xrps =
    let (tmps, refs) = (list_of_sexpr_to_code "" arg_xrps) in
    let call = sprintf "%s(%s)" id (String.concat ", " refs) in
    (String.concat "\n" tmps), call
(* Helper function that turns a list of expressions into code *)
and list_of_sexpr_to_code deref_string xpr_l =
    let trans = List.map sexpr_to_code xpr_l in
    let setups = List.map (fun (s, _) -> s) trans in
    let refs = List.map (fun (_, r) -> deref_string^r) trans in
    setups, refs
and bin_op_to_code lhs o rhs  =
    let setup1, lefts = sexpr_to_code lhs in
    let setup2, rights = sexpr_to_code rhs in
    let os = op_to_code o in
    let tmp_var = (rand_var_gen ()) in
    let new_tmps = sprintf "%s := *%s %s *%s" tmp_var lefts (op_to_code o) rights in
    (setup1 ^ "\n" ^ setup2 ^ "\n" ^ new_tmps) , (sprintf "&%s" tmp_var)
(* Takes a single expr of type list and converts to code *)
and list_expr_to_code = function
    | SListExprLit(Some(t), l) ->
        let tmp_var = rand_var_gen () in
        let setups, refs = list_of_sexpr_to_code "" l in
        let setups = String.concat "\n" setups in
        let list_setup = sprintf "\n%s := %s{%s}"
            tmp_var
            (go_tmp_type_from_type t)
            (String.concat ", " refs) in
        setups ^ list_setup,
        sprintf "&%s" tmp_var
    | SListVar(t, id) ->
        let tmp_var = rand_var_gen () in
            sprintf "%s := %s" tmp_var id,
            sprintf "%s"
                tmp_var
    | SListAccess(xpr_l, xpr_r) ->
        let setup_l, ref_l = sexpr_to_code xpr_l in
        let setup_r, ref_r = sexpr_to_code xpr_r in
        sprintf "%s\n%s"
            setup_l
            setup_r,
        sprintf "(*%s)[*%s]"
            ref_l
            ref_r
    | _ -> raise UnsupportedListExprType

let sassign_to_code = function
    | (id, xpr) ->
        let setup, ref = sexpr_to_code xpr in
        sprintf "%s\n%s = %s" setup id ref
    | a -> raise(UnsupportedSemanticExpressionType(
        sprintf "Assignment expression not yet supported -> %s"
        (svar_assign_s a)))

let soutput_to_code = function
    | SPrintln xpr_l ->
        let (setups, refs) = list_of_sexpr_to_code "*" xpr_l in
            sprintf "%s\nfmt.Println(%s)"
                (String.concat "\n" setups)
                (String.concat "," refs)
    | SPrintf(s, xpr_l) ->
        let (setups, refs) = list_of_sexpr_to_code "*" xpr_l in
            sprintf "%s\nfmt.Printf(%s, %s)"
                (String.concat "\n" setups)
                (get_string_literal_from_sexpr s)
                (String.concat "," refs)
    | _ -> raise UnsupportedOutputType


let sreturn_to_code xprs =
    let (tmps, refs) = list_of_sexpr_to_code "" xprs in
    sprintf "%s\n return %s" (String.concat "\n" tmps) (String.concat ", " refs)

let decls_from_lv = function
    | SFuncDecl(t, (id, _)) -> sprintf "var %s %s" id (go_type_from_type t)
    | _ -> ""
let get_ids = function
    | SFuncDecl(_, (id, _ ) ) -> id
    | SFuncTypedId (_, id) -> id

let lv_to_code lv =
    String.concat ", " (List.map get_ids lv)

let sfunccall_to_code lv id xprs =
    let lhs = lv_to_code lv in
    let (tmps, refs) = list_of_sexpr_to_code "" xprs in
    let tmps = String.concat "\n" tmps in
    let refs = String.concat "," refs in
    if lhs = "" then sprintf "%s\n%s( %s )" tmps id refs
        else sprintf "%s\n%s = %s( %s )" tmps lhs id refs

let rec sast_to_code = function
    | SDecl(_, (id, xpr)) -> sassign_to_code (id, xpr)
    | SAssign a -> sassign_to_code a
    | SOutput p -> soutput_to_code p
    | SReturn xprs -> sreturn_to_code xprs
    | SFuncCall (lv, id, xprs) -> sfunccall_to_code lv id xprs
    | SFor(t, SId(id), xpr, stmts) -> sfor_to_code t id xpr stmts
    | _ -> raise(UnsupportedSemanticStatementType)

and sfor_to_code t id xpr stmts =
    let body = (List.map sast_to_code stmts) in
    let s_tmp, s_ref = sexpr_to_code xpr in
    sprintf "%s\nfor _, %s := range *%s {\n%s\n}"
        s_tmp
        id
        s_ref
        (String.concat "\n" body)

let arg_to_code = function
    | SDecl(t, (id, _) ) -> sprintf "%s %s"
        id
        (go_type_from_type t)

let defaults_to_code = function
    | SDecl(_, (id, xpr)) -> if xpr = NullExpr then ""
        else sprintf "if %s == nil {\n %s\n}"
                id
                (sassign_to_code (id, xpr))

let rec grab_decls = function
    | SDecl(t, (id, _)) :: tl ->
        sprintf "var %s %s" id (go_type_from_type t) :: grab_decls tl
    | SFuncCall(lv, _,_) :: tl ->
        (String.concat "\n" (List.map decls_from_lv lv)) :: grab_decls tl
    | _ :: tl -> grab_decls tl
    | [] -> []

let func_to_code f =
    let (id, args, rets, body) = f in
    sprintf "func %s( %s ) (%s){\n%s\n%s\n%s\n}"
        id
        (String.concat "," (List.map arg_to_code args))
        (String.concat ", " (List.map go_type_from_type rets))
        (String.concat "\n" (List.map defaults_to_code args))
        (String.concat "\n"  (grab_decls body))
        (String.concat "\n" (List.map sast_to_code body))



let skeleton decls main fns = "package main\nimport (\"fmt\")\n" ^
    "var _ = fmt.Printf\n" ^ decls ^ "\nfunc main() {\n" ^
    main ^ "\n}\n " ^ fns



let build_prog sast =
    (* Ignore classes for now *)
    let (stmts, _, funcs) = sast in
    let decls = String.concat "\n" (grab_decls stmts) in
    let code_lines = List.map sast_to_code stmts in
    let gen_code = String.concat "\n" code_lines in
    let func_lines = List.map func_to_code funcs in
    let func_code = String.concat "\n\n" func_lines in
    skeleton decls gen_code func_code

