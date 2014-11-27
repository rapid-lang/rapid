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


type sexpr =
    | SExprInt of int_expr


type soutput =
    | SPrintf of string * expr list
    | SPrintln of expr list


type 'svar_int_decl (string * int_expr option) =
    | IntDecl of (string * int_expr option)


type svar_assign =
    | IntAssignDecl of string * int_expr option
    | IntAssign of string * int_expr


type semantic_stmt =
    | SAssign of svar_assign
    (* TODO:  | SOutput of print *)


(* TODO: Add HTTP routes or something similar in the future *)
(* TODO: add functions so we allow more than just scripts *)
type semantic_program = semantic_stmt list


