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



(* Takes a symbol table and sexpr and rewrites variable references to be typed *)
let rec rewrite_sexpr st = function
    | SId id -> (
        match get_type id st with
        | Int -> SExprInt(SIntVar id)
        | String -> SExprString(SStringVar id)
        | Float -> SExprFloat(SFloatVar id)
        | Bool -> SExprBool(SBoolVar id)
        | _ -> raise UnsupportedDatatypeErr)
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
let rewrite_sexpr_to_t st xpr t =
    let typed_xpr = rewrite_sexpr st xpr in
    let () = check_t_sexpr t typed_xpr in
    typed_xpr


(* checks that an assignment has the proper types *)
let check_var_assign_use sym_tbl id xpr =
    let var_t = (get_type id sym_tbl) in
    let () = check_t_sexpr var_t xpr in
    sym_tbl


(* rewrites any sexprs in an SOutput statement *)
let check_s_output sym_tbl = function
    | SPrintf(s, xpr_l) ->
        let format_str = rewrite_sexpr_to_t sym_tbl s String in
        SPrintf(format_str, List.map (rewrite_sexpr sym_tbl) xpr_l)
    | SPrintln(xpr_l) -> SPrintln(List.map (rewrite_sexpr sym_tbl) xpr_l)


(* Processes an unsafe SAST and returns a type checked SAST *)
let rec var_analysis st ct = function
    | SDecl(t, (id, xpr)) :: tl ->
        let expr = rewrite_sexpr st xpr in
        let st = add_sym t id st in
        let () = check_t_sexpr t expr in
            SDecl(t, (id, expr)) :: var_analysis st ct tl
    | SAssign(id, xpr) :: tl ->
        let expr = rewrite_sexpr st xpr in
        let st = check_var_assign_use st id expr in
            SAssign(id, expr) :: (var_analysis st ct tl)
    | SOutput(so) :: tl ->
        let so = check_s_output st so in
            SOutput(so) :: (var_analysis st ct tl)
    | SUserDefDecl(cls, (id, xpr)) :: tl ->
        let expr = rewrite_sexpr st xpr in
        let t = UserDef cls in
        let st = add_sym t id st in
        let () = check_t_sexpr t expr in
            SUserDefDecl(cls, (id, xpr)) :: var_analysis st ct tl
    | [] -> []


(* Prcesses unchecked classes, adding them and their attributes to class_tbl *)
let rec class_analysis class_tbl = function
    | (class_id, attrs) :: tl ->
        let attr_tbl = add_attrs empty_attribute_table attrs in
        let class_tbl = new_class class_id attr_tbl class_tbl in
        class_analysis class_tbl tl
    | [] -> class_tbl


let gen_semantic_stmts stmts =
    (* build an unsafe semantic AST *)
    let s_stmts = List.map translate_statement stmts in
    (* typecheck and reclassify all variable usage *)
    let checked_stmts = var_analysis symbol_table_list class_table s_stmts in
    checked_stmts


let gen_class_stmts stmts =
    let sclasses = List.map translate_class stmts in
    let checked_sclasses = class_analysis class_table sclasses in
    checked_sclasses


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, _, classes) = ast in
    let stmts = List.rev stmts in
    let typechecked_statements = gen_semantic_stmts stmts in
    let typechecked_classes = gen_class_stmts classes in
    typechecked_statements, typechecked_classes

