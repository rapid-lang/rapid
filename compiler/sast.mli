open Ast
open Datatypes

type cast_side = | Left | Right | None

type string_expr =
    | SStringExprLit of string
    | SStringVar of string
    | SStringCast of sexpr
    | SStringAcc of user_def_expr * string
    | SStringNull
and int_expr =
    | SIntExprLit of int
    | SIntVar of string
    | SIntBinOp of sexpr * op * sexpr
    | SIntCast of sexpr
    | SIntAcc of user_def_expr * string
    | SIntNull
and float_expr =
    | SFloatExprLit of float
    | SFloatVar of string
    | SFloatBinOp of sexpr * op * sexpr
    | SFloatCast of sexpr
    | SFloatAcc of user_def_expr * string
    | SFloatNull
and bool_expr =
    | SBoolExprLit of bool
    | SBoolVar of string
    | SBoolCast of sexpr
    | SBoolBinOp of sexpr * op * sexpr
    | SBoolAcc of user_def_expr * string
    | SBoolNull
and func_call_expr = string * sexpr list
and bin_expr = sexpr * op * sexpr
and list_expr =
    | SListExprLit of var_type option * sexpr list
    | SListVar of var_type * string
    | SListAccess of sexpr * sexpr
    | SListNull
and sexpr =
    | SExprInt of int_expr
    | SExprString of string_expr
    | SExprFloat of float_expr
    | SExprBool of bool_expr
    | SExprUserDef of user_def_expr
    | SExprAccess of sexpr * string
    | SExprList of list_expr
    | SId of string
    | SCall of func_call_expr
    | SCallTyped of var_type * func_call_expr (*return type, id, arg expressions*)
    | SBinop of bin_expr
    | NullExpr (*this is for implied  Null expr*)
    | UntypedNullExpr (*This is for when you type out null in rapid code.*)
and user_def_expr =
    | SUserDefInst of var_type * sactual list (* class * actuals *)
    | SUserDefVar of var_type * string (* class * variablename *)
    | SUserDefNull of var_type
    | SUserDefAcc of var_type * user_def_expr * string (* class * var / instance * member *)
and sactual = string * sexpr

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
    | SFuncCall of sfunc_lval list * string * sexpr list (* left hand of assing, fname, args *)
    | SUserDefDecl of string * svar_assign (* class_id, (id, expr) *)
    | SIfElse of sexpr * semantic_stmt list * semantic_stmt list
    | SIf of sexpr * semantic_stmt list
    | SWhile of sexpr * semantic_stmt list 
    | SFor of var_type * sexpr * sexpr * semantic_stmt list

type sattr =
    | SNonOption of var_type * string * sexpr option
    | SOptional of var_type * string

type sclass = string * sattr list

(*this is the id, args, return types, body*)
type semantic_function = string * semantic_stmt list * var_type list * semantic_stmt list
(* TODO: Add HTTP routes or something similar in the future *)
(* TODO: add functions so we allow more than just scripts *)
type semantic_program = semantic_stmt list * sclass list * semantic_function list

