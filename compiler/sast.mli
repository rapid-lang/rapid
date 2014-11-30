open Ast

(*
 * This file declares all the types for the semantic AST
 *)

type null

(* Will need to figure out how we want to do nested scopes *)
type scope =
    | Global
    | Local

type int_expr =
    | SIntExprLit of int

type string_expr =
    | SStringExprLit of string

type sexpr =
    | SExprInt of int_expr
    | SExprString of string_expr

type soutput =
    | SPrintf of string * expr list
    | SPrintln of expr list

type svar_decl =
    | IntAssignDecl of string * int_expr option
    | StringAssignDecl of string * string_expr option

type svar_assign =
    | IntAssign of string * int_expr
    | StringAssign of string * string_expr

type print =
    | SPrintln of sexpr list
    | SPrintf of string * sexpr list

type semantic_stmt =
    | SAssign of svar_assign
    | SDecl of svar_decl
    | SOutput of print

(* TODO: Add HTTP routes or something similar in the future *)
(* TODO: add functions so we allow more than just scripts *)
type semantic_program = semantic_stmt list


