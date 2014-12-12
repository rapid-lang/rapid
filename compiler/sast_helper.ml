open Sast
open Datatypes

exception UnsupportedSexprTypeClassification
exception UnsupportedAssignExpr
exception UnsupportedDeclStmt
exception UnsupportedSattr
exception ExistingSymbolErr
exception ExistingClassErr
exception ExistingAttributeErr
exception MissingSymbolTablesErr
exception VariatbleNotDefinedErr of string
exception ClassNotDefinedErr of string
exception AttributeNotDefinedErr of string



(* Maps a function to a expr option if it is defined, otherwise return NullExpr *)
let expr_option_map func = function
    | Some o -> func o
    | _ -> NullExpr


module StringMap = Map.Make(String)


let empty_symbol_table = StringMap.empty


let empty_attribute_table = StringMap.empty


let symbol_table_list = StringMap.empty :: []


let class_table = StringMap.empty


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


(* adds a new entry to the class table *)
let new_class class_id class_tbl =
    if StringMap.mem class_id class_tbl
        then raise ExistingClassErr
        else (StringMap.add class_id empty_attribute_table class_tbl)


(* adds a new attribute on the class called class_id *)
let add_attr class_id class_tbl = function
    | SNonOption(t, id, _) | SOptional(t, id)->
        if StringMap.mem class_id class_tbl
            then let attr_tbl = StringMap.remove class_id class_tbl in
                if StringMap.mem id attr_tbl
                    then raise ExistingAttributeErr
                    else let attr_tbl = StringMap.add id t attr_tbl in
                        StringMap.add class_id attr_tbl class_tbl
            else let message = Format.sprintf "%s is not a class" class_id in
                raise (ClassNotDefinedErr message)
    | _ -> raise UnsupportedSattr


(* gets the type of the attribute called id on the class called class_id *)
let get_attr_type class_id class_tbl id =
    if StringMap.mem class_id class_tbl
        then let attr_tbl = StringMap.find class_id class_tbl in
            if StringMap.mem id attr_tbl
                then StringMap.find id attr_tbl
                else let format = "%s is not an attribute on the class %s" in
                    let message = Format.sprintf format id class_id in
                    raise (AttributeNotDefinedErr message)
        else let message = Format.sprintf "%s is not a class" class_id in
            raise (ClassNotDefinedErr message)


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
    | SId _ | _ -> raise UnsupportedSexprTypeClassification

