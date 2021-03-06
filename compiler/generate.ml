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
exception UnsupportedSExprType of string
exception UnsupportedBoolExprType
exception UnsupportedListExprType
exception UnsupportedDeclType of string
exception UnsupportedDatatypeErr
exception InvalidClassInstantiation
exception StringExpressionsRequired
exception InvalidUserDefExpr

module  StringMap = Map.Make(String)


let need_dereference_funcs = let sm = StringMap.empty in
    let sm = StringMap.add "append" true sm in
    let sm = StringMap.add "len" true sm in
    let sm = StringMap.add "println" true sm in
    let sm = StringMap.add "printf" true sm in
    sm

let rand_var_gen _ = "tmp_" ^ Int64.to_string (Random.int64 Int64.max_int)

let rec go_type_from_type = function
    | Int -> "Int"
    | Float -> "Float"
    | Bool -> "Bool"
    | String -> "String"
    | ListType(t) ->
        sprintf "%sList" (go_type_from_type t)
    | UserDef(name) -> name
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
    | x -> raise(UnsupportedSemanticExpressionType(Sast_printer.sexpr_s x))

(* must return a direct reference to a string *)
let get_string_literal_from_sexpr = function
    | SExprString(SStringExprLit s) -> sprintf "(\"%s\")" s
    | SExprString(SStringVar id) -> sprintf "*%s" id
    | _ -> raise StringExpressionsRequired

let op_to_code o = Ast_printer.bin_op_s o

(* top level function for generating sexprs
 * returns a tuple of (setup code, reference code) *)
let rec sexpr_to_code = function
    | SExprInt i -> int_expr_to_code i
    | SExprString s -> string_expr_to_code s
    | SExprFloat f -> float_expr_to_code f
    | SExprBool b -> bool_expr_to_code b
    | SCallTyped(t, c) -> func_expr_to_code c
    | SExprList l -> list_expr_to_code l
    | SExprUserDef u -> user_def_expr_to_code u
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
    | SStringAcc(ud_xpr, attr) ->  ud_access_to_code ud_xpr attr
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
    | SIntAcc(ud_xpr, attr) -> ud_access_to_code ud_xpr attr
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
    | SFloatAcc(ud_xpr, attr) -> ud_access_to_code ud_xpr attr
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
    | SBoolAcc(ud_xpr, attr) -> ud_access_to_code ud_xpr attr
    | SBoolCast c -> cast_to_code Bool c
    | SBoolBinOp(lhs, o, rhs) -> bin_op_to_code lhs o rhs
    | SBoolNull -> "", "nil"
    | _ -> raise UnsupportedBoolExprType
(* takes the destination type and the expression and creates the cast statement *)
and cast_to_code t xpr =
    let src_type = go_type_from_sexpr xpr in
    let dest_type = go_type_from_type t in
    let setup, ref = sexpr_to_code xpr in
    let cast = sprintf "%sTo%s(%s)" src_type dest_type ref in
    setup, cast

and func_expr_to_code = function
    | SFCall(None, id, arg_xrps) ->
        let (tmps, refs) = (list_of_sexpr_to_code "" arg_xrps) in
        let s = if StringMap.mem id (need_dereference_funcs) then "*"
           else "" in
        let refs = List.map (fun str -> s ^ str ) refs in
        let tmps, call = if StringMap.mem id (need_dereference_funcs) then
                let tmp = rand_var_gen () in
                let dots = if id = "append" then "..." else "" in
                tmps @ [(sprintf "\n%s := %s(%s%s)" tmp id
                    (String.concat ", " refs) dots )] , "&" ^ tmp
            else
                tmps ,sprintf "%s(%s)" id (String.concat ", " refs) in
        (String.concat "\n" tmps), call
    | SFCall(Some(xpr), id, arg_xrps) ->
        let setup, ref = sexpr_to_code xpr in
        let (tmps, refs) = (list_of_sexpr_to_code "" arg_xrps) in
        let call = sprintf "%s.%s(%s)" ref id (String.concat ", " refs) in
        ((String.concat "\n" tmps) ^ "\n" ^ setup^ "\n" ), call

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
            sprintf "%s := %s" tmp_var id, tmp_var
    | SListAccess(xpr_l, xpr_r) ->
        let setup_l, ref_l = sexpr_to_code xpr_l in
        let setup_r, ref_r = sexpr_to_code xpr_r in
        sprintf "%s\n%s" setup_l setup_r,
        sprintf "(*%s)[*%s]" ref_l ref_r
    | _ -> raise UnsupportedListExprType
(* translates a user_def_expr to code
 * returns a tuple of (setup code, reference) *)
and user_def_expr_to_code = function
    | SUserDefInst(UserDef(class_id), act_list) ->
        let expand (attr, xpr) =
            let setup, ref = sexpr_to_code xpr in
            setup, sprintf "%s: %s," attr ref in
        let trans = List.map expand act_list in
        (String.concat "\n" (List.map fst trans),
         sprintf "%s{\n%s\n}\n" class_id (String.concat "\n" (List.map snd trans)))
    | SUserDefVar(_, id) ->
        let tmp_var = rand_var_gen () in
        sprintf "%s := &%s" tmp_var id, sprintf "(*%s)" tmp_var
    | SUserDefNull _ ->
        "", "nil"
    | _ -> raise(InvalidUserDefExpr)
and ud_access_to_code ud_expr attr_id =
    let tmp_var = rand_var_gen () in
    let setup, ref = user_def_expr_to_code ud_expr in
        sprintf "%s\n%s := %s.%s\n" setup tmp_var ref attr_id,
        tmp_var

let sassign_to_code = function
    | (SLhsId id, xpr) ->
        let setup, ref = sexpr_to_code xpr in
        sprintf "%s\n%s = %s\n" setup id ref
    | (SLhsAcc(e, mem), xpr) ->
        let setup, ref = sexpr_to_code xpr in
        let lhs_setup, lhs_ref = sexpr_to_code e in
        sprintf "%s\n%s.%s = %s\n" (setup ^ "\n" ^ lhs_setup) lhs_ref mem ref
    | a -> raise(UnsupportedSemanticExpressionType(
        sprintf "Assignment expression not yet supported -> %s"
        (svar_assign_s a)))


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

let sfunccall_to_code lv c =
    let lhs = lv_to_code lv in
    match c with
    | SFCall(None, id, arg_xprs) ->
        let (tmps, refs) = list_of_sexpr_to_code "" arg_xprs in
        let tmps = String.concat "\n" tmps in
        let s, id = if StringMap.mem id (need_dereference_funcs) then "*", "" ^ id
           else "", id in
        let refs = List.map (fun str -> s ^ str ) refs in
        let refs = if s = "Println" then
            String.sub (List.hd refs) 1
                ((String.length (List.hd refs)) - 1) :: (List.tl refs)
            else refs in
        let refs = String.concat "," refs in
        if lhs = "" then sprintf "%s\n%s( %s )" tmps id refs
            else sprintf "%s\n%s = %s( %s )" tmps lhs id refs
    | SFCall(Some(xpr), id, arg_xprs) ->
        let setup, call = func_expr_to_code c in
        if lhs = "" then sprintf "%s\n%s" setup call
            else sprintf "%s\n%s = %s" setup lhs call

let class_instantiate_to_code class_id (id, inst_xpr) =
    let setups, attrs = user_def_expr_to_code inst_xpr in
    sprintf "%s\n%s := %s\n_ = %s" setups id attrs id

let rec grab_decls = function
    | SDecl(t, (id, _)) :: tl ->
        sprintf "var %s %s" id (go_type_from_type t) :: grab_decls tl
    | SFuncCall(lv, _) :: tl ->
        (String.concat "\n" (List.map decls_from_lv lv)) :: grab_decls tl
    | _ :: tl -> grab_decls tl
    | [] -> []

type control_t = | IF | WHILE

(*b is a bool that tells if an if or a for loop. true = if, false = while loop*)
let rec control_code b expr stmts =
    let tmps, exprs = sexpr_to_code expr in
    let body = String.concat "\n" (List.map sast_to_code stmts) in
    let decls = String.concat "\n" (grab_decls stmts) in
    match b with
        |IF -> sprintf "%s\nif *(%s){%s\n%s}" tmps exprs decls body
        |WHILE -> sprintf "for{\n%s\nif !(*(%s)){\nbreak\n}\n%s\n%s}\n"
            tmps exprs decls body

and sast_to_code = function
    | SDecl(_, (id, xpr)) -> sassign_to_code (SLhsId id, xpr)
    | SAssign (lhs, xpr) -> sassign_to_code (lhs, xpr)
    | SReturn xprs -> sreturn_to_code xprs
    | SFuncCall (lv, c) -> sfunccall_to_code lv c
    | SUserDefDecl(class_id, (id, SExprUserDef(xpr))) ->
        class_instantiate_to_code class_id (id, xpr)
    | SUserDefDecl(class_id, (id, NullExpr)) ->
        sprintf "var %s %s\n_ = %s" id class_id id
    | SIf(expr, stmts) -> (control_code IF expr stmts) ^ "\n"
    | SWhile (expr, stmts) -> control_code WHILE expr stmts
    | SIfElse(expr, stmts, estmts) ->
        sprintf "%selse{\n%s\n%s}\n"
        (control_code IF expr stmts)
        (String.concat "\n" (grab_decls estmts))
        (String.concat "\n" (List.map sast_to_code estmts))
    | SFor(t, id, xpr, stmts) -> sfor_to_code t id xpr stmts
    | _ -> raise(UnsupportedSemanticStatementType)

and sfor_to_code t id xpr stmts =
    let body = (List.map sast_to_code stmts) in
    let s_tmp, s_ref = sexpr_to_code xpr in
    sprintf "%s\nfor _, %s := range *%s {\n%s\n%s\n}"
        s_tmp
        id
        s_ref
        (String.concat "\n" (grab_decls stmts))
        (String.concat "\n" body)

let arg_to_code = function
    | SDecl(t, (id, _) ) -> sprintf "%s %s"
        id
        (go_type_from_type t)

let defaults_to_code = function
    | SDecl(_, (id, xpr)) -> if xpr = NullExpr then ""
        else sprintf "if %s == nil {\n %s\n}"
                id
                (sassign_to_code (SLhsId id, xpr))

let func_to_code f =
    let (id, selfref_opt, args, rets, body) = f in
    sprintf "func %s%s( %s ) (%s){\n%s\n%s\n%s\n}"
        (match selfref_opt with
            | Some(SelfRef(class_id, id)) -> sprintf "(%s *%s) " id class_id
            | None -> "")
        id
        (String.concat "," (List.map arg_to_code args))
        (String.concat ", " (List.map go_type_from_type rets))
        (String.concat "\n" (List.map defaults_to_code args))
        (String.concat "\n"  (grab_decls body))
        (String.concat "\n" (List.map sast_to_code body))


let class_def_to_code (class_id, attr_list) =
    let attr_to_code_attr = function
        | SNonOption(t, id, _) | SOptional(t, id) ->
            sprintf "%s %s" id (go_type_from_type t) in
    let attrs = List.map attr_to_code_attr attr_list in
    sprintf "type %s struct{\n%s\n}" class_id (String.concat "\n" attrs)


(* rewrites returns as writes to the connection *)
let http_fstmt_to_code = function
    | SReturn(xpr :: _) ->
        let setup, ref  = cast_to_code String xpr in
        sprintf "%s\nw.Write([]byte(*%s))\nreturn"
            setup
            ref
    | s -> sast_to_code s

let strip_route_re = Str.regexp "[:/]"
let strip_path s = Str.global_replace strip_route_re "" s

let generate_route_registrations routes =
    let routes = List.map (fun (r, _, _, _) ->
        let fname = strip_path r in
        sprintf "router.GET(\"%s\", HTTP%s)" r fname) routes in
    let regs = (String.concat "\n" routes) in
    if regs = ""
        then ""
        else  "router := httprouter.New()\n" ^ regs ^
           "\nlog.Fatal(http.ListenAndServe(\":8080\", router))\n"

let endpoint_to_code (path, args, ret_type, body) =
    let decl_vars = grab_decls body in
    (* grabs the parameters from the request and instantiates the variables *)
    let grab_param (t, name, default) =
        let setup = if default = NullExpr then
            let tmp = rand_var_gen () in
            sprintf "%s := XXX.ByName(\"%s\")\n%s := StringTo%s(&%s)"
                tmp name name (go_type_from_type t) tmp
        else
            let xpr_setup, ref = sexpr_to_code default in
            sprintf "%s\n%s := &%s" xpr_setup name ref in
        sprintf "%s\n_ = %s" setup name in
    let func_start = sprintf "func HTTP%s(w http.ResponseWriter, r *http.Request, "
        (strip_path path) in
    let func_end = sprintf "XXX httprouter.Params){\n%s\n%s\n}\n"
        ((String.concat "\n" decl_vars) ^ "\n\n" ^
            (String.concat "\n" (List.map grab_param args)))
        (String.concat "\n" (List.map http_fstmt_to_code body)) in
    func_start ^ func_end


let skeleton decls http_funcs classes main fns router =
    let packages = ("fmt", "fmt.Printf") ::
        ("net/http", "http.StatusOK") ::
        ("log", "log.Fatal") ::
        ("github.com/julienschmidt/httprouter", "httprouter.CleanPath") :: [] in
    let processed = List.map (fun (p, ref) ->
        sprintf "\"%s\"" p, sprintf "var _ = %s" ref) packages in
    let imports, references = List.map fst processed, List.map snd processed in
    let imports = String.concat "\n" imports in
    let references = String.concat "\n" references in
    "package main\n" ^
    "import (\n" ^ imports ^ "\n)\n" ^
    references ^ "\n\n" ^
    http_funcs ^ "\n\n" ^
    classes ^ "\n\n" ^
    decls ^
    "\nfunc main() {\n" ^
    main ^ "\n\n" ^
    router ^ "\n" ^
    "}\n " ^ fns


let build_prog sast =
    let (stmts, classes, funcs, route_list) = sast in
    let decls = String.concat "\n" (grab_decls stmts) in
    let code_lines = List.map sast_to_code stmts in
    let stmt_code = String.concat "\n" code_lines in
    let func_code = String.concat "\n\n" (List.map func_to_code funcs) in
    let class_struct_defs = String.concat "\n\n" (List.map class_def_to_code classes) in
    let http_funcs = String.concat "\n" (List.map endpoint_to_code route_list) in
    let router_reg = generate_route_registrations route_list in
    skeleton decls http_funcs class_struct_defs stmt_code func_code router_reg

