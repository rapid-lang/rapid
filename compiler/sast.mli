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
    | SId of string
    | NullExpr

type soutput =
    | SPrintf of sexpr * sexpr list
    | SPrintln of sexpr list

type svar_assign = string * sexpr

type semantic_stmt =
    | SAssign of svar_assign
    | SDecl of var_type * svar_assign
    | SOutput of soutput

(* TODO: Add HTTP routes or something similar in the future *)
(* TODO: add functions so we allow more than just scripts *)
type semantic_program = semantic_stmt list

