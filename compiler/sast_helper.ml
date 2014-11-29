open Sast

exception UnsupportedAssignExpr
exception ExistingSymbolErr


let id_from_assign = function
    | IntAssign(id, _) -> id
    | StringAssign(id, _) -> id
    | _ -> raise UnsupportedAssignExpr


let id_from_decl = function
    | IntAssignDecl(id, _) -> id
    | StringAssignDecl(id, _) -> id


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


let translate_if_exists tran o = match o with
    | Some o -> Some (tran o)
    | None -> None





module StringMap = Map.Make(String)
type symbol_table = sexpr StringMap.t
let empty_symbol_table = StringMap.empty



let add_sym st vd =
    let (t, id, _) = vd in
    if StringMap.mem id st
        then raise ExistingSymbolErr
        else StringMap.add id t st

let print_sym st =
    let print_sym = (fun id t -> print_endline(Format.sprintf "%s of %s" id (Ast_helper.string_of_t t))) in
    StringMap.iter print_sym st

