{
    open Parser;;
    let string_to_bool = function
        | "false" -> false
        | "true" -> true
        | _ -> false
}

let decdigit = ['0'-'9']

let floating = '.' decdigit+
    | decdigit+ '.' decdigit*
    | decdigit+ ('.' decdigit*)? 'e' '-'? decdigit+
    | '.' decdigit+ 'e' '-'? decdigit+



rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }              (* Whitespace *)
| "/*" { comment lexbuf } | "//" { comment2 lexbuf } (* Comments *)


(* blocks *)
| "(" { LPAREN }
| ")" { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }

| "[" { LBRACKET }
| "]" { RBRACKET }

| ";" { SEMI }
| "," { COMMA }
(* | ":" { COLON }  *)

(* generic type declaration *)
| "<" { LTGEN }
| ">" { GTGEN }

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

(* Casting operators *)
| '?'      { CASTBOOL }  
| "int("   { CASTINT  }
| "float(" { CASTFLOAT }                         

| "true"  | "false" as bool_val { BOOL_LIT( string_to_bool bool_val ) }
| "boolean" | "int"  | "float" | "string" as prim { TYPE prim }

(*
| "dict" { DICT }
*)
| "list" { LIST }


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
| ['0'-'9']+ as lxm         { INT_VAL( int_of_string lxm ) }
| '"' ([^'"']* as str) '"'  { STRING_LIT str }
| floating as lit           { FLOAT_LIT(float_of_string lit) }
| "null" { NULL }

(* ID's *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID lxm }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and comment2 = parse
  "\n" { token lexbuf }
| _    { comment2 lexbuf }

