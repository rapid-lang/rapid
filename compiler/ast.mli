open Datatypes


type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Qmark | Or | And 

type expr =
    | Id of string
    | IntLit of int
    | BoolLit of bool
    | StringLit of string
    | FloatLit of float
    | Binop of expr * op * expr
    | Call of fcall
    | CastBool of expr
    | ListLit of expr list
    | Noexpr
    | Nullxpr
and fcall = string * expr list

type vdecl = var_type * string * expr option

type print =
    | Printf of expr list
    | Println of expr list

(*Used for function calling*)
type vars = 
    | ID of string
    | VDecl of vdecl

type stmt =
    | Assign of string * expr
    | Block of stmt list
    | If of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt
    | Output of print
    | VarDecl of vdecl
    | FuncCall of vars list * fcall

type func_stmt =
    | FStmt of stmt
    | Return of expr list

type func_decl = {
    fname : string;
    args : vdecl list;
    return : var_type list;
    body : func_stmt list;
}

type program = stmt list * func_decl list

