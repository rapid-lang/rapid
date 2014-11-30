open Ast
open Datatypes

(*
 * This file declares all the types for the semantic AST
 *)

(* Will need to figure out how we want to do nested scopes *)
type scope =
    | Global
    | Local

type int_expr =
    | SIntExprLit of int
    | SIntVar of string

type string_expr =
    | SStringExprLit of string
    | SStringVar of string

type sexpr =
    | SExprInt of int_expr
    | SExprString of string_expr
    | SId of string
    | NullExpr

type soutput =
    | SPrintf of string * sexpr list
    | SPrintln of sexpr list

type svar_assign = string * sexpr

type semantic_stmt =
    | SAssign of svar_assign
    | SDecl of var_type * svar_assign
    | SOutput of soutput

(* TODO: Add HTTP routes or something similar in the future *)
(* TODO: add functions so we allow more than just scripts *)
type semantic_program = semantic_stmt list

