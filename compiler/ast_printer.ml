open Ast
open Datatypes

(* alias print functions for cleaner code *)
let sprintf = Format.sprintf
let concat = String.concat
let str_concat l = concat "" l

let newline = Str.regexp "\n"
let indent_block s = Str.global_replace newline "\n\t" s

let rec string_of_t = function
    | Int -> "int"
    | Bool -> "bool"
    | String -> "string"
    | Float -> "float"
    | ListType(s) -> sprintf "list<%s>" (string_of_t s)
    | UserDef(s) -> sprintf "(USER_DEF %s)" s
    | Void -> "void"
    | Multi -> "multi return"
    | InfiniteArgs -> "InfiniteArgs"
    | AnyList -> "AnyList"

let bin_op_s = function
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Equal -> "=="
    | Neq -> "!="
    | Less -> "<"
    | Leq -> "<="
    | Greater -> ">"
    | Geq -> ">="
    | Qmark -> "?"
    | Or -> "||"
    | And -> "&&"
    | Mod -> "%"

(* Converts expressions to strings *)
let rec expr_s = function
    | IntLit l -> sprintf "(Int Literal (%d))" l
    | Id s -> sprintf "(Id %s)" s
    | Binop(e1, o, e2) -> sprintf "(Binop (%s) %s (%s))"
        (expr_s e1)
        (bin_op_s o)
        (expr_s e2)
    | Call f -> fcall_s f
    | BoolLit b -> sprintf "(Bool literal %b)" b
    | StringLit s -> sprintf "(String literal %s)" s
    | FloatLit f -> sprintf "(Float literal %f)" f
    | CastBool e -> sprintf "(Cast (%s) to boolean)" (expr_s e)
    | Cast(t, i) -> sprintf "(Cast (%s) to %s)" (expr_s i) (string_of_t t)
    | ListLit l -> sprintf "(List literal [%s])"
        (String.concat ", " (List.map expr_s l))
    | UserDefInst(id, actls) -> sprintf "(INSTANTIATE new UserDef %s(%s))"
        id
        ("\n\t" ^ (String.concat ",\n\t " (List.map actual_s actls)))
    | Access(e, mem) -> sprintf "(ACCESS %s.%s)" (expr_s e) mem
    | ListAccess(xpr_l, xpr_r) -> sprintf "(List access %s at index %s)"
        (expr_s xpr_l)
        (expr_s xpr_r)
    | Noexpr -> "( NOEXPR )"
    | Nullxpr -> "(Null)"

and fcall_s = function
    | (None, f, es) -> sprintf "(Call (%s) with (%s))"
        f
        (concat ", " (List.map (fun e -> sprintf "(%s)" (expr_s e)) es))
    | (Some(xpr), f, es) -> sprintf "(Call %s.(%s) with (%s))"
        (expr_s xpr)
        f
        (concat ", " (List.map (fun e -> sprintf "(%s)" (expr_s e)) es))

and actual_s = function
    | Actual(id, e) -> sprintf "(ACTUAL: %s=%s)" id (expr_s e)

and lhs_s = function
    | LhsId(id) -> id
    | LhsAcc(xpr, id) -> sprintf "(%s.%s)" (expr_s xpr) id

let string_of_vdecl (t, nm, e) = sprintf "%s %s %s"
    (string_of_t t)
    nm
    (match e with
        | Some xpr -> sprintf "= %s" (expr_s xpr)
        | None     -> "(Not assigned)")

let string_of_user_def_decl (cls, nm, e) = sprintf "%s %s %s"
    cls
    nm
    (match e with
        | Some xpr -> sprintf "= %s" (expr_s xpr)
        | None     -> "(Not assigned)")

(* Prettyprint statements *)
let func_lvalue_s = function
    | ID(i) -> i
    | VDecl(t, id, x) -> string_of_vdecl (t,id,x)

let rec stmt_s = function
    | Assign(lhs, e) -> sprintf "(Assign %s (%s))"
        (lhs_s lhs)
        (expr_s e)
    | If(e, s1, [] ) -> sprintf "(If (%s) -> (%s))"
        (expr_s e)
        (concat "\n" (List.map stmt_s s1))
    | If(e, s1, s2 ) -> sprintf "(If (%s) -> (%s))(Else -> (%s))"
        (expr_s e)
        (concat "\n" (List.map stmt_s s1))
        (concat "\n" (List.map stmt_s s2))
    | For(t, id, xpr, stmt_l) -> sprintf "(For (%s %s in %s)\n{(%s)})"
        (string_of_t t)
        id
        (expr_s xpr)
        (String.concat "\n" (List.map stmt_s stmt_l))
    | While(e, s) -> sprintf "(While (%s)\n{(%s))}"
        (expr_s e)
        (concat "\n" (List.map stmt_s s))
    | VarDecl vd -> sprintf "(VarDecl (%s))"
        (string_of_vdecl vd)
    | UserDefDecl ud -> sprintf "(UserDefDecl (%s))"
        (string_of_user_def_decl ud)
    | FuncCall(s,f) -> sprintf "(FuncCall(%s = %s))"
        (concat ", " (List.map func_lvalue_s s))
        (fcall_s f)

and fstmt_s = function
    | Return e -> sprintf "(Return (%s))\n"
        (concat ", " (List.map expr_s e))
    | FStmt s -> stmt_s s

let rec http_tree_s = function
    | Param(t, id, tree) -> sprintf "(param (%s %s)\n\t%s\n}"
        (string_of_t t)
        id
        (indent_block (String.concat "\n" (List.map http_tree_s tree)))
    | Namespace(id, tree) -> sprintf "(namespace (%s)\n\t%s\n}"
        id
        (indent_block (String.concat "\n"(List.map http_tree_s tree)))
    | Endpoint(id, args, ret_t, body) -> sprintf "(HTTP %s(%s)%s{\n\t%s\n}"
        id
        (String.concat "," (List.map string_of_vdecl args))
        (string_of_t ret_t)
        (indent_block(String.concat "\n" (List.map fstmt_s body)))

let func_decl_s f = sprintf "{\nfname = \"%s\"\nargs = [%s]\n\tbody = [%s]\n}"
    f.fname
    (concat ", " (List.map string_of_vdecl f.args))
    (concat ",\n" (List.map fstmt_s f.body))


let attr_s = function
    | NonOption(t, id, Some(xpr)) -> sprintf "(ATTR: %s of %s = %s)"
        id
        (string_of_t t)
        (expr_s xpr)
    | NonOption(t, id, None) -> sprintf "(ATTR: %s of %s)"
        id
        (string_of_t t)
    | Optional(t,id) -> sprintf "(ATTR: OPTIONAL %s of %s)"
        id
        (string_of_t t)

let member_s = function
    | Attr a      -> attr_s a
    | ClassFunc f -> func_decl_s f


let instance_block_s = function
    | Some(InstanceBlock(id, fns)) -> sprintf "(INSTANCE %s {\n\t%s}"
        id
        (concat "\n\t" (List.map func_decl_s fns))
    | None -> "(NO INSTANCE BLOCK)"


let class_s (name, members, instance_block) =
    sprintf "(CLASS %s:\n%s\n%s)"
        name
        (instance_block_s instance_block)
        (concat "\n" (List.map (fun a -> "\t" ^ a)
            (List.map member_s members)))


let program_s (stmts, classes, funcs, http_tree) = sprintf
    "classes:{\n%s\n}\nstatements:{\n%s\n}\nfunctions:\n%sHTTP tree:\n%s"
    (concat "\n" (List.rev (List.map class_s classes)))
    (concat "\n" (List.rev (List.map stmt_s stmts)))
    (concat "\n" (List.rev (List.map func_decl_s funcs)))
    (concat "\n" (List.rev (List.map http_tree_s http_tree)))
