open Sast;;
open Sast_helper;;
open Format;;

exception RepeatDeclarationErr of string
exception UncaughtCompareErr of string
exception UnsupportedStatementTypeErr of string
exception UndeclaredVarErr of string
exception InvalidTypeReassignErr of string
exception UnsupportedExpressionType of string

(* Searches for a match in a list and returns a corresponding option *)
let rec find f l = match l with
    | hd :: tl -> if f hd then Some hd
                  else find f tl
    | [] -> None


(* Takes a sorted list of objects and calls compare on each pair.
 * compare should throw exceptions when appropriate *)
let check_sorted compare sorted =
    let rec compare_last = fun last rest -> match rest with
        (* allow for an exception to be raised *)
        | hd :: tl -> let () = compare last hd in compare_last hd tl
        | [] -> ()
    in match sorted with
        | hd :: tl -> compare_last hd tl
        | [] -> ()


(* checks if any variables are redeclared *)
let check_repeat_var_decl sorted_stmts =
    let comp = fun (_, id1, _) (_, id2, _) ->
        if id1 = id2 then raise(RepeatDeclarationErr id1)
    in check_sorted comp sorted_stmts


(* Check for variables being reassigned to different types
 *
 * For every assign, compare the type of the expression to the
 * type in that variable's declaration
 *
 * @param sorted_decls: list of Ast.vdecl (var_type * string * expr option)
 * @param sorted_assigns: sorted list of Sast.svar_assign
 * *)
let check_invalid_var_reassign sorted_decls sorted_assigns =
    let mtch name = (fun (_, nm, _) -> name = nm) in
    List.iter (fun assign ->
        let id = (id_from_assign assign) in
        let decl_opt = find (mtch id) sorted_decls in
        let decl = match decl_opt with
            | Some d -> d
            | None -> raise(UndeclaredVarErr id) in
        match decl with
            | (Ast.Int, _, _) -> (match assign with
                | IntAssignDecl _ -> ()
                | IntAssign _ -> ()
                | _ -> raise(InvalidTypeReassignErr "non matching type"))
            | (t, _, _) -> raise(UnsupportedStatementTypeErr (Ast_helper.string_of_t t))
            | _ -> raise(UnsupportedStatementTypeErr "non matching type")
    ) sorted_assigns


(* takes in a list of ast variable declarations and outputs sast svar_assigns *)
let translate_statements var_decls =
    List.map (fun vd -> match vd with
        (* TODO: a ton more types here, also support expressions *)
        | Ast.VarDecl(Ast.Int, id, i) -> (match i with
            | Some Ast.IntLit i -> SAssign(IntAssignDecl(id, Some(SIntExprLit i)))
            | None              -> SAssign(IntAssignDecl(id, None))
            | _                 -> raise(UnsupportedExpressionType(sprintf "Var: %s" id)))
        | Ast.VarDecl(t, _, _) -> raise(UnsupportedStatementTypeErr (Ast_helper.string_of_t t))
        | Ast.Expr(Ast.Assign(id, expr)) -> (match expr with
            | Ast.IntLit i -> SAssign(IntAssign(id, SIntExprLit i))
            | _ -> raise(UnsupportedExpressionType(sprintf "Var: %s" id)))
        | _ ->         raise(UnsupportedStatementTypeErr "type unknown")
    ) var_decls


let gen_semantic_stmts stmts =
    (* reduce list to var ID's *)
    let decls = List.fold_left
        (fun lst st -> match st with
            | Ast.VarDecl(vd) -> vd :: lst
            | _ -> lst
        ) [] stmts in
    let comp = fun (_, id1, _) (_, id2, _) -> String.compare id1 id2 in
    let sorted_decls = List.sort comp decls in

    (* reduce list to var assigns *)
    let assigns = List.fold_left
        (fun lst stmt -> match stmt with
            | Ast.Expr(Ast.Assign(id, Ast.IntLit(i))) -> IntAssign(id, SIntExprLit(i)) :: lst
            | Ast.Expr(Ast.Assign(id, xpr)) -> (match xpr with
                | Ast.IntLit(i) -> IntAssign(id, SIntExprLit(i)) :: lst
                (* TODO: add all other assignments *)
                | _ -> raise(UnsupportedExpressionType(sprintf "Expression for %s not supported (%s)" id (Ast_helper.expr_s xpr))))
            | _ -> lst
    ) [] stmts in
    let comp_assigns = fun a1 a2 -> String.compare (id_from_assign a1) (id_from_assign a2) in
    let sorted_assigns = List.sort comp_assigns assigns in

    let () = check_repeat_var_decl sorted_decls in
    let () = check_invalid_var_reassign sorted_decls sorted_assigns in
    (* TODO: check order of reference *)
    translate_statements stmts


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, _) = ast in
    gen_semantic_stmts stmts


let string_of_sast sast =
    let strs = List.map semantic_stmt_s sast in
    String.concat "" strs

