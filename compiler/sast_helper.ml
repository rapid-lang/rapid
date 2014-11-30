open Sast
open Datatypes

exception UnsupportedAssignExpr
exception UnsupportedDeclStmt
exception ExistingSymbolErr
exception VariatbleNotDefinedErr of string



(* Searches for a match in a list and returns a corresponding option *)
let rec find f l = match l with
    | hd :: tl -> if f hd then Some hd
                  else find f tl
    | [] -> None


(* Takes a sorted list of objects and calls compare on each pair.
 * compare should throw exceptions when appropriate *)
let check_sorted compare sorted =
    let rec compare_last = fun last rest -> match rest with
        (* allow for an exception to be raised *)
        | hd :: tl -> let () = compare last hd in compare_last hd tl
        | [] -> ()
    in match sorted with
        | hd :: tl -> compare_last hd tl
        | [] -> ()


let option_map func = function
    | Some o -> Some(func o)
    | _ -> None


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


let print_sym st =
    let print_sym = (fun id t -> print_endline(Format.sprintf "%s of %s" id (Ast_helper.string_of_t t))) in
    StringMap.iter print_sym st

