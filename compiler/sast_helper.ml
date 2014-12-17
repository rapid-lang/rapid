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
exception VariableNotDefinedErr of string
exception ClassNotDefinedErr of string
exception AttributeNotDefinedErr of string
exception MissingActualErr of string
exception ExistingFuncErr
exception BadFunctionId
exception CannotFindFunctionIDForArgTypes
exception CannotFindFunctionIDForReturnType
exception CannotFindFunctionIDForReturnTypeList


(* Maps a function to a expr option if it is defined, otherwise return NullExpr *)
let expr_option_map func = function
    | Some o -> func o
    | _ -> NullExpr


(* returns the the possible types a binary op can have*)
let get_op_types = function
    | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Less | Ast.Greater | Ast.Leq | Ast.Geq -> [Int; Float]
    | Ast.Equal | Ast.Neq -> [Bool; Int; Float; String]
    | Ast.And | Ast.Or -> [(Bool)]
    | Ast.Mod -> [(Int)]

module StringMap = Map.Make(String)


let empty_function_table = StringMap.empty

(*add a (id -> typelist*typelist into the function table*)
let add_func ft id arg_ts ret_ts =
    if StringMap.mem id ft
        then raise ExistingFuncErr
    else
        let v = (arg_ts, ret_ts) in
        StringMap.add id v ft

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
    | _ -> raise(VariableNotDefinedErr(Format.sprintf "%s is not defined" id))

let get_return_type id ft =
    if StringMap.mem id ft
        then let (_, ret_t) = StringMap.find id ft in
            match ret_t with
                | [] -> Void
                | t :: [] -> t
                | t_list -> Multi
        else raise CannotFindFunctionIDForReturnType

let get_return_type_list id ft =
    if StringMap.mem id ft
        then let (_, retl) = StringMap.find id ft in retl
        else raise CannotFindFunctionIDForReturnTypeList


let get_arg_types id ft =
    if StringMap.mem id ft
        then let (arg_ts, _) = StringMap.find id ft in arg_ts
        else raise CannotFindFunctionIDForArgTypes


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
        add_attrs (insert_attr attr_id attr_tbl (t, false, default)) tl
    | SNonOption(t, attr_id, None) :: tl ->
        add_attrs (insert_attr attr_id attr_tbl (t, true, NullExpr)) tl
    | SOptional(t, attr_id) :: tl ->
        add_attrs (insert_attr attr_id attr_tbl (t, false, NullExpr)) tl
    | [] -> attr_tbl
    | _ -> raise UnsupportedSattr
and insert_attr attr_id attr_tbl triple =
    if StringMap.mem attr_id attr_tbl
        then raise(ExistingAttributeErr(attr_id))
        else StringMap.add attr_id triple attr_tbl

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
    | (key, xpr) :: tl ->
        if StringMap.mem key actl_tbl
            then raise ExistingActualErr
            else add_actls (StringMap.add key xpr actl_tbl) tl
    | [] -> actl_tbl
    | _ -> raise UnsupportedSactual

(* Get the type of an actual given *)
let get_actl_type actl_tbl key =
    if StringMap.mem key actl_tbl
        then StringMap.find key actl_tbl
        else raise(MissingActualErr(
            Format.sprintf "Argument %s is missing" key))


(* returns the type of a typed sexpr *)
let rec sexpr_to_t expected_t = function
    | SExprInt _ -> Int
    | SExprFloat _ -> Float
    | SExprBool _ -> Bool
    | SExprString _ -> String
    | SExprUserDef(SUserDefInst(s, _) | SUserDefVar(s, _) | SUserDefNull(s)) -> s
    | SExprAccess _ -> expected_t
    | NullExpr -> expected_t
    | SCallTyped(t, _) -> t
    | UntypedNullExpr -> expected_t
    | SExprList l -> (
        match l with
        | SListExprLit(Some(t), _) -> t
        | SListExprLit(None, _) -> expected_t
        | SListVar(t, _) -> ListType t
        | SListAccess(xpr_l, xpr_r) -> sexpr_to_t expected_t xpr_l
        | _ -> raise UnsupportedSexprTypeClassification
    )
    | SId _ | _ -> raise UnsupportedSexprTypeClassification
