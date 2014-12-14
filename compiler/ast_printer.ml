open Ast
open Datatypes

(* alias print functions for cleaner code *)
let sprintf = Format.sprintf
let concat = String.concat
let str_concat l = concat "" l


let rec string_of_t = function
    | Int -> "int"
    | Bool -> "bool"
    | String -> "string"
    | Float -> "float"
    | ListType(s) -> sprintf "list<%s>" (string_of_t s)
    | UserDef(s) -> sprintf "(USER_DEF %s)" s
    | Void -> "void"
    | Multi -> "multi return"
    | Var -> "var"

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
    | ListLit l -> sprintf "(List literal [%s])"
        (String.concat ", " (List.map expr_s l))
    | Noexpr -> "( NOEXPR )"
    | Nullxpr -> "(Null)"

and fcall_s = function
    | (f, es) -> sprintf "(Call (%s) with (%s))"
        f
        (concat ", " (List.map (fun e -> sprintf "(%s)" (expr_s e)) es))

let output_s = function
    | Printf el -> sprintf "(Printf(%s))"
        (String.concat ", " (List.map expr_s el))
    | Println el -> sprintf "(Println(%s))"
        (String.concat ", " (List.map expr_s el))

let string_of_vdecl (t, nm, e) = sprintf "%s %s %s"
    (string_of_t t)
    nm
    (match e with
        | Some exp -> sprintf "= %s" (expr_s exp)
        | None     -> "(Not assigned)")

(* Prettyprint statements *)
let func_lvalue_s = function
    | ID(i) -> i
    | VDecl(t, id, x) -> string_of_vdecl (t,id,x)

let rec stmt_s = function
    | Assign(v, e) -> sprintf "(Assign %s (%s))"
        v
        (expr_s e)
    | Block ss -> sprintf "(Block {\n%s\n})"
        (concat "\n" (List.map (fun s -> sprintf "(%s)" (stmt_s s)) ss))
    | If(e, s1, Ast.Block([])) -> sprintf "(If (%s) -> (%s))"
        (expr_s e)
        (stmt_s s1)
    | If(e, s1, s2) -> sprintf "(If (%s)\n(%s) else (%s))"
        (expr_s e)
        (stmt_s s1)
        (stmt_s s2)
    | For(e1, e2, e3, s) -> sprintf "(For ((%s); (%s); (%s))\n{(%s)})"
        (expr_s e1)
        (expr_s e2)
        (expr_s e3)
        (stmt_s s)
    | While(e, s) -> sprintf "(While (%s)\n{(%s))0"
        (expr_s e)
        (stmt_s s)
    | Output o -> sprintf "(Output (%s))"
        (output_s o)
    | VarDecl vd -> sprintf "(VarDecl (%s))"
        (string_of_vdecl vd)
    | FuncCall(s,f) -> sprintf "(FuncCall(%s = %s))" 
        (concat ", " (List.map func_lvalue_s s))
        (fcall_s f)

let fstmt_s = function
    | Return e -> sprintf "(Return (%s))"
        (concat ", " (List.map expr_s e))
    | FStmt s -> stmt_s s

let func_decl_s f = sprintf "{\nfname = \"%s\"\nargs = [%s]\n\tbody = [%s]\n}"
    f.fname
    (concat ", " (List.map string_of_vdecl f.args))
    (concat ",\n" (List.map fstmt_s f.body))

let program_s (stmts, funcs) = sprintf "statements:{\n%s\n}\nfunctions:\n%s"
    (concat "\n" (List.rev (List.map stmt_s stmts)))
    (concat "\n" (List.rev (List.map func_decl_s funcs)))


(*
 * Non-printing helpers
 *)

(* Converts a string to a datatype *)
let string_to_t = function
    | "boolean" -> Bool
    | "int" -> Int
    | "float" -> Float
    | "string" -> String
    | c -> UserDef(c)
