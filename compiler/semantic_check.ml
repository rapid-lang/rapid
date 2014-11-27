open Sast

exception RepeatDeclarationErr of string
exception UncaughtCompareErr of string
exception UnsupportedStatementTypeErr of string
exception UndeclaredVarErr of string
exception InvalidTypeReassignErr of string

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
        else ()
    in check_sorted comp sorted_stmts


(* check for variables being reassigned to different types *)
(* TODO: refactor *)
let check_invalid_var_reassign sorted_decls sorted_assigns =
    let mtch name = (fun (_, nm, _) -> name = nm) in
    List.iter (fun (id, exp) ->
        let decl_opt = find (mtch id) sorted_decls in
        let decl = match decl_opt with
            | Some d -> d
            | None -> raise(UndeclaredVarErr id) in
        match decl with
            | (Ast.Int, _, _) -> (match exp with
                | IntAssignDecl _ -> ()
                | t -> raise(InvalidTypeReassignErr "non matching type"))
            | (t, _, _) -> raise(UnsupportedStatementTypeErr (Ast.string_of_t t))
    ) sorted_assigns


(* takes in a list of ast variable declarations and outputs sast svar_assigns *)
let translate_var_decls var_decls var_scp =
    List.map (fun (vd : svar_int_decl) -> match vd with
        | (Ast.Int, id, Some Ast.IntLit(i))  -> IntAssignDecl(id, Some(SIntExprLit(i)))
        | (Ast.Int, id, None)               -> IntAssignDecl(id, None)
        (* TODO: a ton more types here, also support expressions *)
        | (t, _, _) ->
            raise(UnsupportedStatementTypeErr (Ast.string_of_t t))
    ) var_decls


let gen_var_decls stmts =
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
            | _ -> lst
        ) [] stmts in
    let comp_asgns = fun (id1, _) (id2, _) -> String.compare id1 id2 in
    let sorted_assigns = List.sort comp_asgns assigns in

    let () = check_repeat_var_decl sorted_decls in
    let () = check_invalid_var_reassign sorted_decls sorted_assigns in
    let svar_decls = translate_var_decls sorted_decls in
    ()


let sast_from_ast ast =
    (* ignore functions for now *)
    let (stmts, _) = ast in
    let () = gen_var_decls stmts in
    stmts


let string_of_sast sast =
    "program goes here"

