open Datatypes



type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    | Id of string
    | IntLit of int
    | BoolLit of bool
    | StringLit of string
    | FloatLit of float
    | Binop of expr * op * expr
    | Call of fcall
    | ListLit of expr list
    | Noexpr
and fcall =
    | FCall of string * expr list

type vdecl = var_type * string * expr option

type print =
    | Printf of expr list
    | Println of expr list

type stmt =
    | Assign of string * expr
    | Block of stmt list
    | If of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt
    | Output of print
    | VarDecl of vdecl
    | FuncCall of fcall

type func_stmt =
    | FStmt of stmt
    | Return of expr

type func_decl = {
    fname : string;
    formals : string list;
    return : var_type list;
    body : func_stmt list;
}

type attr =
    | NonOption of var_type * string * expr option
    | Optional of var_type * string

type class_decl = string * attr list

(*
type class_decl = string * attr list * func_decl list * route_decl list
*)

type program = stmt list * func_decl list * class_decl list
(*
type program = stmt list * func_decl list * class_decl list * route_decl list
*)

