
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    | Id of string
    | IntLit of int
    | BoolVal of bool
    | StringLit of string
    | Binop of expr * op * expr
    | Assign of string * expr
    | Call of string * expr list
    | Noexpr

(* AST type for datatypes
 *
 * Primative types and a placeholder for userdefined types *)
type var_type =
    | Int
    | String
    | Bool
    | Float
    | User_def

type vdecl = var_type * string * expr option

type print =
    | Printf of string * expr list
    | Println of expr list

type stmt =
    | Block of stmt list
    | Expr of expr
    | If of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt
    | Output of print
    | VarDecl of vdecl

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

