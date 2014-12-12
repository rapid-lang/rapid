open Sast
open Sast_helper
open Sast_printer
open Format
open Datatypes
open Translate

exception RepeatDeclarationErr of string
exception InvalidTypeDeclarationErr of string
exception UncaughtCompareErr of string
exception UnsupportedStatementTypeErr of string
exception UndeclaredVarErr of string
exception InvalidTypeReassignErr of string
exception InvalidTypeErr of string
exception UnsupportedExpressionType
exception UnsupportedSexpr
exception UnsupportedDatatypeErr
exception StringDatatypeRequiredErr
exception InvalidArgErr
exception InvalidArgOrder
exception InvalidReturnTypeErr
exception NoReturnErr


(* Takes a symbol table and sexpr and rewrites variable references to be typed *)
let rec rewrite_sexpr st ft = function
    | SId id -> (
        match get_type id st with
        | Int -> SExprInt(SIntVar id)
        | String -> SExprString(SStringVar id)
        | Float -> SExprFloat(SFloatVar id)
        | Bool -> SExprBool(SBoolVar id)
        | _ -> raise UnsupportedDatatypeErr)
    | SExprBool(SBoolCast e) ->
        let xpr = (rewrite_sexpr st ft e) in
           SExprBool(SBoolCast(xpr))
    | SCall(id, xprs) ->
        SCallTyped((get_return_type id ft), (id, (List.map (rewrite_sexpr st ft) xprs)))
    (* TODO: add all new expressions that can contain variable references to be simplified *)
    | xpr -> xpr


(* Takes a type and a typed sexpr and confirms it is the proper type *)
let check_t_sexpr expected_t xpr =
    let found_t = sexpr_to_t expected_t xpr in
    if found_t = expected_t
        then ()
        else raise(InvalidTypeErr(Format.sprintf "Expected %s expression, found %s"
            (Ast_printer.string_of_t expected_t)
            (Ast_printer.string_of_t found_t)))


(* typechecks a sexpr *)
let rewrite_sexpr_to_t st ft xpr t =
    let typed_xpr = rewrite_sexpr st ft xpr in
    let () = check_t_sexpr t typed_xpr in
    typed_xpr


(* checks that an assignment has the proper types *)
let check_var_assign_use sym_tbl id xpr =
    let var_t = (get_type id sym_tbl) in
    let () = check_t_sexpr var_t xpr in
    sym_tbl


(* rewrites any sexprs in an SOutput statement *)
let check_s_output sym_tbl ft = function
    | SPrintf(s, xpr_l) ->
        let format_str = rewrite_sexpr_to_t sym_tbl ft s String in
        SPrintf(format_str, List.map (rewrite_sexpr sym_tbl ft ) xpr_l)
    | SPrintln(xpr_l) -> SPrintln(List.map (rewrite_sexpr sym_tbl ft) xpr_l)

(*Check that the return statement has expressions with the right types*)
let rec check_return_types = function 
    | ((xpr :: s),(t :: types)) -> let () = (check_t_sexpr t xpr) in
        check_return_types (s, types)
    (*To few vals returned*)
    | (([]),(t::types)) -> raise InvalidReturnTypeErr
    (*to many vals returned*)
    | ((xpr :: s),([])) -> raise InvalidReturnTypeErr
    | [],[] -> ()

(*Scan all stmts in a function for returns then check the types*)
let rec check_returns r = function 
    | SReturn(s) :: tl-> let _ = check_return_types (s, r) in
         SReturn(s) :: check_returns r tl
    | hd :: tl -> hd :: check_returns r tl
    | [] -> []

(* Processes an unsafe SAST and returns a type checked SAST *)
let rec var_analysis st ft = function
    | SDecl(t, (id, xpr)) :: tl ->
        let expr = rewrite_sexpr st ft xpr in
        let st = add_sym t id st in
        let () = check_t_sexpr t expr in
            SDecl(t, (id, expr)) :: var_analysis st ft tl
    | SAssign(id, xpr) :: tl ->
        let expr = rewrite_sexpr st ft xpr in
        let st = check_var_assign_use st id expr in
            SAssign(id, expr) :: (var_analysis st ft tl)
    | SOutput(so) :: tl ->
        let so = check_s_output st ft so in
            SOutput(so) :: (var_analysis st ft tl)
    (*Return stmts are xpr lists, tranlslate all the expressions here*)
    | SReturn(s) :: tl -> let xprs = List.map (rewrite_sexpr st ft) s in
         SReturn(xprs) :: (var_analysis st ft tl)
    | SFuncCall (lv, id, xprs) :: tl -> 
        SFuncCall(lv, id, xprs) :: (var_analysis st ft tl)
    | [] -> []

(*
Adds all var decls in a stmt list to the scope and returns the new scope
This does not do any type checking, and ignores the optional expression
*)
let rec add_to_scope st = function
    | SDecl(t, (id, xpr)) :: tl ->
       let st = add_sym t id st in
           add_to_scope st tl
    | _ :: tl -> add_to_scope st tl
    | [] -> st

let is_not_default = function
    | NullExpr -> true
    | _ -> false

(*Called when we see an arg with default val, all the rest must have defaults*)
let rec check_default_args = function
    | SDecl(t, (id, xpr)) :: tl -> if is_not_default xpr
            then raise InvalidArgOrder
        else
            SDecl(t, (id, xpr)) :: check_default_args tl
    | _ :: _ -> raise InvalidArgErr
    | [] -> []

(*Checks to make sure args with default vals come at the end fo the arg list*)
let rec check_arg_order = function
    | SDecl(t, (id, xpr)) :: tl -> if is_not_default xpr
            then SDecl(t, (id, xpr)) :: check_arg_order tl
        else
            SDecl(t, (id, xpr))  :: check_default_args tl
    | _ :: _ -> raise InvalidArgErr
    | [] -> []

let check_for_return body =
    let last_stmt = List.hd (List.rev body) in
    match last_stmt with
        | SReturn(s) -> ()
        | _ -> raise NoReturnErr

let rec check_funcs st ft = function
    | (fname, args, rets, body) :: tl ->
        let _ = check_arg_order args in
        let scoped_st = new_scope st in
        let targs = var_analysis scoped_st ft args in
        (*args are added to function scope*)
        let scoped_st = add_to_scope scoped_st args in
        (*typecheck the body and rewrite vars to have type*)
        let tbody = var_analysis scoped_st ft body in
        (*check the return type matches the return statement*)
        (*Once we add control flow I'm not sure how to make sure 
          all paths return something so ignoring for now.*)
        let _  = check_returns rets tbody in
        (*if no return types then don't worry, else find a return stmnt*)
        if rets = [] then 
            (fname, targs, rets, tbody) :: check_funcs st ft tl
        else
            let () = check_for_return tbody in
            (fname, targs, rets, tbody) :: check_funcs st ft tl
    | [] -> []

let rec build_function_table ft = function
    | (fname, args, rets, body) :: tl -> 
        let args_to_type = function
            | SDecl(t, (id, xpr)) -> t
            | _ -> raise InvalidArgErr
        in
        let arg_ts = List.map args_to_type args in
        let ft = (add_func ft fname arg_ts rets) in
        build_function_table ft tl
    | [] -> ft

(*The order of the checking and building of symbol tables may need to change
    to allow functions to be Hoisted*)
let gen_semantic_program stmts funcs =
    (* build an unsafe semantic AST *)
    let s_stmts = List.map translate_statement stmts in
    let s_funcs = List.map translate_function funcs in
    let ft = build_function_table empty_function_table s_funcs in
    (* typecheck and reclassify all variable usage *)
    let checked_stmts = var_analysis symbol_table_list ft s_stmts in
    (*Add all the var decls to the global scope*)
    let st = add_to_scope symbol_table_list s_stmts in
    (*typecheck all the functions (including args and returns)*)
    let checked_funcs = check_funcs st ft s_funcs in
    (checked_stmts, checked_funcs)


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, funcs) = ast in
    let stmts = List.rev stmts in
    gen_semantic_program stmts funcs

