open Sast
open Datatypes

exception UnsupportedAssignExpr
exception UnsupportedDeclStmt
exception ExistingSymbolErr
exception VariatbleNotDefinedErr of string


let expr_option_map func = function
    | Some o -> func o
    | _ -> NullExpr



module StringMap = Map.Make(String)
type symbol_table = sexpr StringMap.t
let empty_symbol_table = StringMap.empty


let add_sym st t id =
    if StringMap.mem id st
        then raise ExistingSymbolErr
        else StringMap.add id t st


let get_type st id =
    if StringMap.mem id st
        then StringMap.find id st
        else raise(VariatbleNotDefinedErr(Format.sprintf "%s is not defined" id))

