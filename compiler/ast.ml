open Format

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq


exception Error of string


type expr =
    | Literal of int
    | BoolVal of bool
    | StringLit of string
    | Id of string
    | Binop of expr * op * expr
    | Assign of string * expr
    | Call of string * expr list
    | Noexpr


(* AST type for datatypes
 *
 * Primative types and a placeholder for userdefined types *)
type t =
    | Int
    | String
    | Bool
    | Float
    | User_def


type stmt =
    | Block of stmt list
    | Expr of expr
    | Return of expr
    | If of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt


type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
}


type program = string list * func_decl list


(* alias print functions for cleaner code *)
let sprintf = Format.sprintf
let concat = String.concat
let str_concat (l) = concat "" l


(* Converts a string to a datatype *)
let string_to_t = function
    | "boolean" -> Bool
    | "int" -> Int
    | "float" -> Float
    | "string" -> String
    | "" -> raise(Error("No type"))
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
    | Literal(l) -> "Literal " ^ string_of_int l
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



(* Prettyprint statements *)
let rec stmt_s = function
    | Block(ss) -> sprintf "Block [%s]"
        (concat ",\n" (List.map (fun s -> sprintf "(%s)" (stmt_s s)) ss))
    | Expr(e) -> sprintf "Expr (%s)"
        (expr_s e)
    | Return(e) -> sprintf "Return (%s)"
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


let func_decl_s f = sprintf "{
    fname = \"%s\"
    formats = [%s]
    locals = [%s]
    body = [%s]
    }" f.fname
        (concat ", " f.formals)
        (concat ", " f.locals)
        (concat ",\n" (List.map stmt_s f.body))


let program_s (vars, funcs) = sprintf "([%s],\n%s)"
    (concat ", " vars)
    (concat "\n" (List.map func_decl_s funcs))


(* "Pretty printed" version of the AST, meant to generate a MicroC program
    from the AST.  These functions are only for pretty-printing (the -a flag)
    the AST and can be removed. *)

let rec string_of_expr = function
    | Literal(l) -> string_of_int l
    | Id(s) -> s
    | Binop(e1, o, e2) ->
        sprintf "%s %s %s" (string_of_expr e1)
        (match o with
            | Add -> "+"
            | Sub -> "-"
            | Mult -> "*"
            | Div -> "/"
            | Equal -> "=="
            | Neq -> "!="
            | Less -> "<"
            | Leq -> "<="
            | Greater -> ">"
            | Geq -> ">=")
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
    | Return(expr) -> sprintf "return %s\n"
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


let string_of_vdecl id = sprintf "int %s\n" id


let string_of_fdecl fdecl =
    sprintf "%s(%s)\n{\n%s%s}\n"
        (fdecl.fname)
        (concat ", " fdecl.formals)
        (str_concat (List.map string_of_vdecl fdecl.locals))
        (str_concat (List.map string_of_stmt fdecl.body))


let string_of_program (vars, funcs) =
    sprintf "%s\n%s"
        (concat "" (List.map string_of_vdecl vars))
        (concat "\n" (List.map string_of_fdecl funcs))

