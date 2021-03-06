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
    | ListAccess of expr * expr
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

(* Used for function calling *)
type vars =
    | ID of string
    | VDecl of vdecl

type lhs =
    | LhsId of string
    | LhsAcc of expr * string

type stmt =
    | Assign of lhs * expr
    | For of var_type * string * expr * stmt list
    | If of expr * stmt list * stmt list
    | While of expr * stmt list
    | Output of print
    | VarDecl of vdecl
    | UserDefDecl of user_def_decl
    | FuncCall of vars list * fcall
    | HttpTree of http_tree

and http_tree =
    (* typed route param, rest of tree *)
    | Param of var_type * string * http_tree list
    (* /route, rest of tree *)
    | Namespace of string * http_tree list
    (* /route, argument list, return type, function body *)
    | Endpoint of string * vdecl list * var_type * func_stmt list

and func_stmt =
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

type program = stmt list  * class_decl list * func_decl list * http_tree list

