open Datatypes

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Qmark | Or | And | Mod

type expr =
    | Id of string
    | IntLit of int
    | BoolLit of bool
    | StringLit of string
    | FloatLit of float
    | Binop of expr * op * expr
    | Call of fcall
    | Cast of var_type * expr
    | CastBool of expr
    | ListLit of expr list
    | UserDefInst of string * actual list
    | Access of expr * string
    | Noexpr
    | Nullxpr
and fcall = expr option * string * expr list
and actual =
    | Actual of string * expr

type vdecl = var_type * string * expr option

type user_def_decl = string * string * expr option

type print =
    | Printf of expr list
    | Println of expr list

(*Used for function calling*)
type vars =
    | ID of string
    | VDecl of vdecl

type lhs =
    | LhsId of string
    | LhsAcc of expr * string

type stmt =
    | Assign of lhs * expr
    | Block of stmt list
    | If of expr * stmt * stmt
    | For of expr * expr * expr * stmt
    | While of expr * stmt
    | Output of print
    | VarDecl of vdecl
    | UserDefDecl of user_def_decl
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

type attr =
    | NonOption of var_type * string * expr option
    | Optional of var_type * string

type member =
    | Attr of attr
    | ClassFunc of func_decl

type instance_block =
    | InstanceBlock of string * func_decl list

type class_decl = string * member list * instance_block option

(*
type class_decl = string * attr list * func_decl list * route_decl list
*)

type program = stmt list * class_decl list * func_decl list
(*
type program = stmt list * func_decl list * class_decl list * route_decl list
*)

