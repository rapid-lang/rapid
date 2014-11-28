open Ast

exception Error of string

(* alias print functions for cleaner code *)
let sprintf = Format.sprintf
let concat = String.concat
let str_concat l = concat "" l

(* Converts a string to a datatype *)
let string_to_t = function
    | "boolean" -> Bool
    | "int" -> Int
    | "float" -> Float
    | "string" -> String
    | "" -> raise(Error "No type")
    | _ -> User_def

(* Prettyprint expressions *)
let bin_op_s = function
    | Add -> "Add"
    | Sub -> "Sub"
    | Mult -> "Mult"
    | Div -> "Div"
    | Equal -> "Equal"
    | Neq -> "Neq"
    | Less -> "Less"
    | Leq -> "Leq"
    | Greater -> "Greater"
    | Geq -> "Geq"

(* Prettyprint expressions *)
let rec expr_s = function
    | IntLit(l) -> "Literal " ^ string_of_int l
    | Id(s) -> "Id " ^ s
    | Binop(e1, o, e2) -> sprintf "Binop (%s) %s (%s)"
        (expr_s e1)
        (bin_op_s o)
        (expr_s e2)
    | Assign(v, e) -> sprintf "Assign %s (%s)" v
        (expr_s e)
    | Call(f, es) -> sprintf "Call %s [%s]" f
        (concat ", " (List.map (fun e -> sprintf "(%s)" (expr_s e)) es))
    | BoolVal(b) -> string_of_bool b
    | StringLit(s) -> s
    | Noexpr -> "Noexpr"

let bin_op = function
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

let rec string_of_expr = function
    | IntLit(l) -> string_of_int l
    | Id(s) -> s
    | Binop(e1, o, e2) -> sprintf "%s %s %s"
        (string_of_expr e1)
        (bin_op o)
        (string_of_expr e2)
    | Assign(v, e) -> sprintf "%s = %s" v
        (string_of_expr e)
    | Call(f, el) -> sprintf "%s(%s)" f
        (concat ", " (List.map string_of_expr el))
    | BoolVal(b) -> string_of_bool b
    | StringLit(s) -> s
    | Noexpr -> ""

let string_of_t = function
    | Int -> "int"
    | Bool -> "bool"
    | String -> "string"
    | Float -> "float"
    | User_def -> "var" (*TODO: change user def to something else*)

let output_s = function
    | Printf(f, el) -> sprintf "Printf(%s, %s)"
        f
        (String.concat ", " (List.map expr_s el))
    | Println(el) -> sprintf "Println(%s)\n"
        (String.concat ", " (List.map expr_s el))

let string_of_vdecl (t, nm, e) = sprintf "%s %s %s\n"
    (string_of_t t)
    nm
    (match e with
        | Some exp -> sprintf "= %s" (expr_s exp)
        | None     -> "(Not assigned)")

(* Prettyprint statements *)
let rec stmt_s = function
    | Block(ss) -> sprintf "Block [%s]"
        (concat ",\n" (List.map (fun s -> sprintf "(%s)" (stmt_s s)) ss))
    | Expr(e) -> sprintf "Expr (%s)"
        (expr_s e)
    | If(e, s1, s2) -> sprintf "If (%s) (%s) (%s)"
        (expr_s e)
        (stmt_s s1)
        (stmt_s s2)
    | For(e1, e2, e3, s) -> sprintf "For (%s) (%s) (%s) (%s)"
        (expr_s e1)
        (expr_s e2)
        (expr_s e3)
        (stmt_s s)
    | While(e, s) -> sprintf "While (%s) (%s)"
        (expr_s e)
        (stmt_s s)
    | Output(o) -> output_s o
    | VarDecl(vd) -> string_of_vdecl vd

let fstmt_s = function
    | Return(e) -> sprintf "Return (%s)"
        (expr_s e)
    | FStmt(s) -> stmt_s s

let func_decl_s f = sprintf "{\nfname = \"%s\"\nformals = [%s]\n\tbody = [%s]\n}"
    f.fname
    (concat ", " f.formals)
    (concat ",\n" (List.map fstmt_s f.body))

let program_s (stmts, funcs) = sprintf "([%s],\n%s)"
    (concat ", " (List.map stmt_s stmts))
    (concat "\n" (List.map func_decl_s funcs))

let bin_op = function
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

let rec string_of_expr = function
    | IntLit(l) -> string_of_int l
    | Id(s) -> s
    | Binop(e1, o, e2) -> sprintf "%s %s %s"
        (string_of_expr e1)
        (bin_op o)
        (string_of_expr e2)
    | Assign(v, e) -> sprintf "%s = %s" v
        (string_of_expr e)
    | Call(f, el) -> sprintf "%s(%s)" f
        (concat ", " (List.map string_of_expr el))
    | BoolVal(b) -> string_of_bool b
    | StringLit(s) -> s
    | Noexpr -> ""

let rec string_of_stmt = function
    | Block(stmts) -> sprintf "{\n%s}\n"
        (str_concat (List.map string_of_stmt stmts))
    | Expr(expr) -> sprintf "%s\n"
        (string_of_expr expr)
    | If(e, s, Block([])) -> sprintf "if (%s)\n%s"
        (string_of_expr e)
        (string_of_stmt s)
    | If(e, s1, s2) -> sprintf "if (%s)\n%s else\n%s"
        (string_of_expr e)
        (string_of_stmt s1)
        (string_of_stmt s2)
    | For(e1, e2, e3, s) -> sprintf "for (%s; %s; %s) %s"
        (string_of_expr e1)
        (string_of_expr e2)
        (string_of_expr e3)
        (string_of_stmt s)
    | While(e, s) -> sprintf "while (%s) %s"
        (string_of_expr e)
        (string_of_stmt s)
    | Output(o) -> output_s o
    | VarDecl(vd) -> string_of_vdecl vd

let string_of_fstmt = function
    | Return(e) -> sprintf "Return (%s)"
        (expr_s e)
    | FStmt(s) -> string_of_stmt s

let string_of_fdecl fdecl =
    sprintf "func %s(%s)(%s)\n{\n%s}\n"
        (fdecl.fname)
        (concat ", " fdecl.formals)
        (concat ", " (List.map string_of_t fdecl.return))
        (*(str_concat (List.map string_of_vdecl fdecl.locals))*)
        (str_concat (List.map string_of_fstmt fdecl.body))

let string_of_program (stmts, funcs) =
    sprintf "%s\n%s"
        (str_concat (List.map string_of_stmt stmts))
        (concat "\n" (List.map string_of_fdecl funcs))

