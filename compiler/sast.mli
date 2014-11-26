open Ast

(*
 * This file declares all the types for the semantic AST
 *)


(* Will need to figure out how we want to do nested scopes *)
type scope =
    | Global
    | Local


type IntExpr =
    | SIntExprLit of scope * SValIntLit


type sexpr =
    | SExprInt of IntExpr


type soutput =
    | SPrintf of string * expr list
    | SPrintln of expr list


type svar_decl =
    | IntDecl of scope * Int * string * int_expr option


type svar_assign
    | IntAssign of scope * string * int_expr


type semantic_stmt =
    | SDecl of svar_decl
    | SAssign of svar_assign
    (* TODO:  | SOutput of print *)


(* TODO: Add HTTP routes or something similar in the future *)
(* TODO: add functions so we allow more than just scripts *)
type semantic_program = semantic_stmt list


