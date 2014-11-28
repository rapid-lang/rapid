open Ast;;

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
}

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    | [] -> []
    | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
    List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

(* TODO *)
let translate ast =
    let sast = Semantic_check.sast_from_ast ast in
    let sast = List.rev sast in
    let code = Generate.build_prog sast in
    code


