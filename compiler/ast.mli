open Datatypes



type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Qmark  

type expr =
    | Id of string
    | IntLit of int
    | BoolVal of bool
    | StringLit of string
    | Binop of expr * op * expr
    | Call of fcall
    | CastBool of expr * op                         
    | Noexpr
and fcall =
    | FCall of string * expr list

type vdecl = var_type * string * expr option

type print =
    | Printf of expr * expr list
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

type program = stmt list * func_decl list

