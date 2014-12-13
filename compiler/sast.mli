open Ast
open Datatypes


type bool_expr =
    | SBoolExprLit of bool
    | SBoolVar of string
    | SBoolNull

type int_expr =
    | SIntExprLit of int
    | SIntVar of string
    | SIntNull

type float_expr =
    | SFloatExprLit of float
    | SFloatVar of string
    | SFloatNull

type string_expr =
    | SStringExprLit of string
    | SStringVar of string
    | SStringNull

type sexpr =
    | SExprInt of int_expr
    | SExprString of string_expr
    | SExprFloat of float_expr
    | SExprBool of bool_expr
    | SExprUserDef of user_def_expr
    | SId of string
    | NullExpr
and user_def_expr =
    | SUserDefInst of string * sactual list
    | SUserDefVar of string
    | SUserDefNull
and sactual =
    | SActual of string * sexpr

type soutput =
    | SPrintf of sexpr * sexpr list
    | SPrintln of sexpr list

type svar_assign = string * sexpr

type semantic_stmt =
    | SAssign of svar_assign
    | SDecl of var_type * svar_assign
    | SOutput of soutput
    | SUserDefDecl of string * svar_assign


type sattr =
    | SNonOption of var_type * string * sexpr option
    | SOptional of var_type * string

type sclass = string * sattr list
(* TODO: Eventually it will look like this: ?
type sclass = string * sattr list * sfunc_decl list * sroute_decl list
*)

(* TODO: Add HTTP routes or something similar in the future *)
(* TODO: add functions so we allow more than just scripts *)
type semantic_program = semantic_stmt list * sclass list

