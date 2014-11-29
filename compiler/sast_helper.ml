open Sast

exception UnsupportedAssignExpr


let id_from_assign = function
    | IntAssign(id, _) -> id
    | StringAssign(id, _) -> id
    | _ -> raise UnsupportedAssignExpr


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

