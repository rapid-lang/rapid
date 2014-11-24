{
    open Parser;;
    let string_to_bool = function
        | "false" -> false
        | "true" -> true
        | _ -> false
}

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }              (* Whitespace *)
| "/*" { comment lexbuf } | "//" { comment2 lexbuf } (* Comments *)


(* blocks *)
| "(" { LPAREN }
| ")" { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }

(*
TODO: add when working on lists
| "[" { LBRACKET }
| "]" { RBRACKET }
*)

| ";" { SEMI }
| "," { COMMA }
(* | ":" { COLON }  *)

(* operators *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }

(* comparisons *)
| "==" { EQ }
| "!=" { NEQ }
| "<"  { LT }
| "<=" { LEQ }
| ">"  { GT }
| ">=" { GEQ }

(* control structures *)
| "if"   { IF }
| "else" { ELSE }
| "for"  { FOR }
| "in"   { IN }

(*
| "while" { WHILE }
*)

(* primatives *)
| '=' { ASSIGN }

(* | '?' { QUESTION } *)

| "true"  | "false" as bool_val { BOOL_VAL( string_to_bool(bool_val) ) }
| "boolean" | "int"  | "float"| "string" as prim { TYPE(prim) }

(*
| "dict" { DICT }
| "list" { LIST }
*)


| "printf"  { PRINTF }
| "println" { PRINTLN }
(* functions *)
| "func"   { FUNC }
| "return" { RETURN }
(*
| "unsafe" { UNSAFE }
*)

(* classes *)
(*
| "class"    { CLASS }
| "optional" { OPTIONAL }
| "instance" { INSTANCE }
*)

(* http related *)
(*
| "http"      { HTTP }
| "param"     { PARAM }
| "namespace" { NAMESPACE }
*)

(* switch statements *)
(*
| "switch"      { SWITCH }
| "case"        { CASE }
| "default"     { DEFAULT }
| "fallthrough" { FALLTHROUGH }
*)

(* literals *)
| ['0'-'9']+ as lxm { INT_VAL(int_of_string(lxm) ) }
| '"' ([^'"']* as str) '"' { STRING_LIT(str) }

(* ID's *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and comment2 = parse
  "\n" { token lexbuf }
| _    { comment2 lexbuf }

