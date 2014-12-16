
(* AST type for datatypes
 * Primative types and a placeholder for userdefined types *)
type var_type =
    | Int
    | String
    | Bool
    | Float
    | Error
    | UserDef of string
    | ListType of var_type
    | Void (*Used for funcions with no args or no rets*)
    | Multi(*Used for funcs with multiple rets*)
