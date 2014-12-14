open Ast
open Datatypes


type bool_expr =
    | SBoolExprLit of bool
    | SBoolVar of string
    | SBoolAcc of string * string
    | SBoolNull

type int_expr =
    | SIntExprLit of int
    | SIntVar of string
    | SIntAcc of string * string
    | SIntNull

type float_expr =
    | SFloatExprLit of float
    | SFloatVar of string
    | SFloatAcc of string * string
    | SFloatNull

type string_expr =
    | SStringExprLit of string
    | SStringVar of string
    | SStringAcc of string * string
    | SStringNull

type sexpr =
    | SExprInt of int_expr
    | SExprString of string_expr
    | SExprFloat of float_expr
    | SExprBool of bool_expr
    | SExprUserDef of user_def_expr
    | SExprAccess of sexpr * string
    | SId of string
    | NullExpr
and user_def_expr =
    | SUserDefInst of var_type * sactual list (* class * actuals *)
    | SUserDefVar of var_type * string (* class * variablename *)
    | SUserDefAcc of var_type * string * string (* class * var_id * member *)
    | SUserDefNull of var_type
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

