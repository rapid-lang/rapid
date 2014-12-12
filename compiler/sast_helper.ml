open Sast
open Datatypes

exception UnsupportedSexprTypeClassification
exception UnsupportedAssignExpr
exception UnsupportedDeclStmt
exception ExistingSymbolErr
exception MissingSymbolTablesErr
exception VariatbleNotDefinedErr of string
exception ExistingFuncErr



(* Maps a function to a expr option if it is defined, otherwise return NullExpr *)
let expr_option_map func = function
    | Some o -> func o
    | _ -> NullExpr


module StringMap = Map.Make(String)


let empty_function_table = StringMap.empty

(*add a (id -> typelist*typelist into the funciton table*)
let add_func ft id arg_ts ret_ts = 
    if StringMap.mem id ft
        then raise ExistingFuncErr
    else 
        let v = (arg_ts, ret_ts) in 
        StringMap.add id v ft

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

let get_return_type id ft = 
    let (_, ret_t) = StringMap.find id ft in
    match ret_t with
    | [] -> Void
    | t :: [] -> t
    | _ :: _ -> Multi

let get_return_type_list id ft =
    let (_, retl) = StringMap.find id ft in retl

(* adds a new empty symbol table for use in the new scope *)
let new_scope sym_tbl = empty_symbol_table :: sym_tbl


(* pops the last added scope removing all scoped variables. *)
let pop_scope = function
    | current_scope :: scope_list -> scope_list
    | [] -> raise MissingSymbolTablesErr


(* returns the type of a typed sexpr *)
let sexpr_to_t expected_t = function
    | SExprInt _ -> Int
    | SExprFloat _ -> Float
    | SExprBool _ -> Bool
    | SExprString _ -> String
    | NullExpr -> expected_t
    | SCallTyped(t, _) -> t
    | UntypedNullExpr -> expected_t
    | SId _ | _ -> raise UnsupportedSexprTypeClassification

