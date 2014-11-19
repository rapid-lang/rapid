
open Ast;;


exception Error of string


type symbol_table = {
    (* list of tuples of ID, type and an optional value *)
    variables: (ident * datatype * value option)
}



let semantic_check ast =
    ""


