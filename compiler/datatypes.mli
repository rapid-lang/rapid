
(* AST type for datatypes
 * Primative types and a placeholder for userdefined types *)
type var_type =
    | Int
    | String
    | Bool
    | Float
    | UserDef of string
    | ListType of var_type
    | Var
    | Void (*Used for funcions with no args or no rets*)
    | Multi of ret_type(*Used for funcs with multiple rets*)
and ret_type = var_type list
