{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }              (* Whitespace *)
| "/*" { comment lexbuf } | "//" { comment2 lexbuf } (* Comments *)

(* blocks *)
| "(" { LPAREN }   | ")" { RPAREN }
| "{" { LBRACE }   | "}" { RBRACE }
| "[" { LBRACKET } | "]" { RBRACKET }

| ";" { SEMI } | ":" { COLON } | "," { COMMA }

(* operators *)
| '+' { PLUS }  | '-' { MINUS }
| '*' { TIMES } | '/' { DIVIDE }

(* comparisons *)
| "==" { EQ } | "!=" { NEQ }
| "<"  { LT } | "<=" { LEQ }
| ">"  { GT } | ">=" { GEQ }

(* control structures *)
| "if"  { IF }  | "else" { ELSE }
| "for" { FOR } | "in"   { IN }
| "while" { WHILE }

(* functions *)
| "unsafe" { UNSAFE }
| "func" { FUNC } | "return" { RETURN }

(* primatives *)
| '=' { ASSIGN }
| "boolean" { BOOL } | '?' { QUESTION }
| "true" { TRUE } | "false" { FALSE }
| "int"  { INT }  | "float" { FLOAT } | "string" { STRING }
| "dict" { DICT } | "list"  { LIST }

(* classes *)
| "class"  { CLASS }
| "optional" { OPTIONAL }
| "instance" { INSTANCE }

(* http related *)
| "http"   { HTTP }
| "param" { PARAM } | "namespace" { NAMESPACE }

(* switch statements *)
| "switch" { SWITCH }
| "case"{ DEFAULT } | "default"{ DEFAULT }
| "fallthrough"{ FALLTHROUGH }

(* literals *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }

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

