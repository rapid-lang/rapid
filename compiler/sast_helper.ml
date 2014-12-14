open Sast
open Datatypes

exception UnsupportedSexprTypeClassification
exception UnsupportedAssignExpr
exception UnsupportedDeclStmt
exception UnsupportedSattr
exception UnsupportedSactual
exception ExistingSymbolErr
exception ExistingClassErr
exception ExistingActualErr
exception ExistingAttributeErr of string
exception MissingSymbolTablesErr
exception VariatbleNotDefinedErr of string
exception ClassNotDefinedErr of string
exception AttributeNotDefinedErr of string
exception MissingActualErr of string


(* Maps a function to a expr option if it is defined, otherwise return NullExpr *)
let expr_option_map func = function
    | Some o -> func o
    | _ -> NullExpr


module StringMap = Map.Make(String)


let empty_symbol_table = StringMap.empty
let symbol_table_list = StringMap.empty :: []

let empty_actuals_table = StringMap.empty
let empty_attribute_table = StringMap.empty
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


(* pops the last added scope removing all scoped variables. *)
let pop_scope = function
    | current_scope :: scope_list -> scope_list
    | [] -> raise MissingSymbolTablesErr


(* adds a new entry to the class table *)
let new_class class_id attr_tbl class_tbl =
    if StringMap.mem class_id class_tbl
        then raise ExistingClassErr
        else (StringMap.add class_id attr_tbl class_tbl)


(* adds a new attribute on the class called class_id *)
let rec add_attrs attr_tbl = function
    | SNonOption(t, attr_id, Some(default)) :: tl ->
        insert_attr attr_id attr_tbl (t, false, default) tl
    | SNonOption(t, attr_id, None) :: tl ->
        insert_attr attr_id attr_tbl (t, true, NullExpr) tl
    | SOptional(t, attr_id) :: tl ->
        insert_attr attr_id attr_tbl (t, false, NullExpr) tl
    | [] -> attr_tbl
    | _ -> raise UnsupportedSattr
and insert_attr attr_id attr_tbl triple tl =
    if StringMap.mem attr_id attr_tbl
        then raise(ExistingAttributeErr(attr_id))
        else add_attrs (StringMap.add attr_id triple attr_tbl) tl

(* Get the table of attributes for a specific class *)
let get_attr_table class_id class_tbl =
    if StringMap.mem class_id class_tbl
        then StringMap.find class_id class_tbl
        else raise (ClassNotDefinedErr
                (Format.sprintf "%s is not a class" class_id))

(* Get attribute, as (type, required, default) *)
let get_attr class_id class_tbl id =
    let attr_tbl = get_attr_table class_id class_tbl in
    if StringMap.mem id attr_tbl
        then StringMap.find id attr_tbl
        else raise(AttributeNotDefinedErr(Format.sprintf
                "%s is not an attribute on the class %s"
                id class_id))

(* gets the type of the attribute called id on the class called class_id *)
let get_attr_type class_id class_tbl id =
    let (t, _, _) = get_attr class_id class_tbl id in
    t

(* Add the actuals into actl_tbl from an actuals list, verifying uniqueness *)
let rec add_actls actl_tbl = function
    | Sast.SActual(key, xpr) :: tl ->
        if StringMap.mem key actl_tbl
            then raise ExistingActualErr
            else add_actls (StringMap.add key xpr actl_tbl) tl
    | [] -> actl_tbl
    | _ -> raise UnsupportedSactual


let get_actl_type actl_tbl key =
    if StringMap.mem key actl_tbl
        then StringMap.find key actl_tbl
        else raise(MissingActualErr(
            Format.sprintf "Argument %s is missing" key))


(* returns the type of a typed sexpr *)
let sexpr_to_t expected_t = function
    | SExprInt _ -> Int
    | SExprFloat _ -> Float
    | SExprBool _ -> Bool
    | SExprString _ -> String
    | SExprUserDef(SUserDefInst(s, _) | SUserDefVar(s, _) | SUserDefNull(s)) ->
        s
    | SExprAccess _ -> expected_t
    | NullExpr -> expected_t
    | SId _ | _ -> raise UnsupportedSexprTypeClassification

