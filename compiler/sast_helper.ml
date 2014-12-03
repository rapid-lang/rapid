open Sast
open Datatypes

exception UnsupportedSexprTypeClassification
exception UnsupportedAssignExpr
exception UnsupportedDeclStmt
exception ExistingSymbolErr
exception MissingSymbolTablesErr
exception VariatbleNotDefinedErr of string


let expr_option_map func = function
    | Some o -> func o
    | _ -> NullExpr


module StringMap = Map.Make(String)

let empty_symbol_table = StringMap.empty

let symbol_table_list = StringMap.empty :: []


(* inserts a (symbol -> type) into the top level scope *)
let add_sym t id = function
    | current_scope :: scope_list ->
        if StringMap.mem id current_scope
            then raise ExistingSymbolErr
            else (StringMap.add id t current_scope) :: scope_list
    | _ -> raise MissingSymbolTablesErr


(* retrieves a type in the top level scope that it is found *)
let rec get_type id = function
    | current_scope :: scope_list ->
        if StringMap.mem id current_scope
            then StringMap.find id current_scope
            else get_type id scope_list
    | _ -> raise(VariatbleNotDefinedErr(Format.sprintf "%s is not defined" id))


(* adds a new empty symbol table for use in the new scope *)
let new_scope sym_tbl = empty_symbol_table :: sym_tbl


(* pops the last added scope removing all scoped variables. *)
let pop_scope = function
    | current_scope :: scope_list -> scope_list
    | [] -> raise MissingSymbolTablesErr


let sexpr_to_t = function
    | SExprInt _ -> Int
    | SExprFloat _ -> Float
    | SExprString _ -> String
    | SId _ | _ -> raise UnsupportedSexprTypeClassification

