open Ast
open Datatypes

type cast_side = | Left | Right | None

type string_expr =
    | SStringExprLit of string
    | SStringVar of string
    | SStringCast of sexpr
    | SStringAcc of string * string
    | SStringNull
and int_expr =
    | SIntExprLit of int
    | SIntVar of string
    | SIntBinOp of sexpr * op * sexpr
    | SIntCast of sexpr
    | SIntAcc of string * string
    | SIntNull
and float_expr =
    | SFloatExprLit of float
    | SFloatVar of string
    | SFloatBinOp of sexpr * op * sexpr 
    | SFloatCast of sexpr
    | SFloatAcc of string * string
    | SFloatNull
and bool_expr =
    | SBoolExprLit of bool
    | SBoolVar of string
    | SBoolCast of sexpr
    | SBoolBinOp of sexpr * op * sexpr 
    | SBoolAcc of string * string
    | SBoolNull
and func_call_expr = string * sexpr list
and bin_expr = sexpr * op * sexpr
and sexpr =
    | SExprInt of int_expr
    | SExprString of string_expr
    | SExprFloat of float_expr
    | SExprBool of bool_expr
    | SExprUserDef of user_def_expr
    | SExprAccess of sexpr * string
    | SId of string
    | SCall of func_call_expr
    | SCallTyped of var_type * func_call_expr (*return type, id, arg expressions*)
    | SBinop of bin_expr
    | NullExpr (*this is for implied  Null expr*)
    | UntypedNullExpr (*This is for when you type out null in rapid code.*)
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

type sfunc_lval =
    | SFuncDecl of var_type * svar_assign (*always a vdecl*)
    | SFuncId of string (*after translate before second pass*)
    | SFuncTypedId of var_type * string (*After second pass*)

type semantic_stmt =
    | SAssign of svar_assign
    | SDecl of var_type * svar_assign
    | SOutput of soutput
    | SReturn of sexpr list
    | SFuncCall of sfunc_lval list * string * sexpr list (*left hand of assing, fname, args*)
    | SUserDefDecl of string * svar_assign

type sattr =
    | SNonOption of var_type * string * sexpr option
    | SOptional of var_type * string

type sclass =
    | SClass of string * sattr list

(*this is the id, args, return types, body*)
type semantic_function = string * semantic_stmt list * var_type list * semantic_stmt list
(* TODO: Add HTTP routes or something similar in the future *)
(* TODO: add functions so we allow more than just scripts *)
type semantic_program = semantic_stmt list * sclass list * semantic_function list

