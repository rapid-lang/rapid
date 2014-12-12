%{
    open Ast
    open Datatypes
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token LBRACKET RBRACKET LTGEN GTGEN LIST
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE FUNC IN
%token PRINTLN PRINTF // LOG
%token CLASS NEW
// %token INT BOOL FLOAT STRING

%token <string> ID TYPE STRING_LIT
%token <int> INT_VAL
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program


%% /* Parser Rules */


primtype:
    | TYPE { Ast_printer.string_to_t $1 }
    | LIST LTGEN primtype GTGEN { ListType $3 }
    /* todo: add arrays, dicts to primtype */


/* Base level expressions of a program:
 * TODO: Classes */
program:
    | /* nothing */ { [], [], [] }
    | program stmt SEMI {
        let (statements, functions, classes) = $1 in
            ($2 :: statements), functions, classes }
    | program func_decl {
        let (statements, functions, classes) = $1 in
            statements, ($2 :: functions), classes }
    | program class_decl {
        let (statements, functions, classes) = $1 in
            statements, functions, ($2 :: classes) }


/* TODO: allow user defined types */
datatype_list:
    | datatype_list COMMA primtype { $3 :: $1 }
    | primtype                 { [$1] }
    | /* nothing */            { [] }


return_type:
    /* TODO: allow user defined types */
    | primtype                    { [$1] }
    | LPAREN datatype_list RPAREN { $2 }

/*var declarations can now be done inline*/
func_decl:
    // func w/ return types
    | FUNC ID LPAREN arguments RPAREN return_type LBRACE fstmt_list RBRACE
    {{
        fname = $2;
        formals = $4;
        return = $6;
        body = List.rev $8
    }}
    // func w/o return types
    | FUNC ID LPAREN arguments RPAREN LBRACE fstmt_list RBRACE
    {{
        fname = $2;
        formals = $4;
        return = [];
        body = List.rev $7
    }}
    /* TODO: unsafe functions */


arguments:
    | /* nothing */ { [] }
    | formal_list   { List.rev $1 }


formal_list:
    /* TODO: allow user defined types */
    | primtype ID                   { [$2] }
    | formal_list COMMA primtype ID { $4 :: $1 }


/* a tuple here of (primtype, ID) */
var_decl:
    | primtype ID             { ($1 , $2, None) }
    | primtype ID ASSIGN expr { ($1 , $2, Some($4)) }


user_def_decl:
    | ID ID             { ($1, $2, None) }
    | ID ID ASSIGN expr { ($1, $2, Some($4)) }


fstmt_list:
    | /* nothing */         { [] }
    | fstmt_list func_stmt { $2 :: $1 }


func_stmt:
    | RETURN expr SEMI { Return($2) }
    | stmt SEMI        { FStmt($1) }


stmt:
    | print          { Output $1 }
    | var_decl       { VarDecl $1 }
    | user_def_decl  { UserDefDecl $1 }
    | ID ASSIGN expr { Assign($1, $3) }
    | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
    | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
    | WHILE LPAREN expr RPAREN stmt { While($3, $5) }


print:
    | PRINTLN LPAREN expression_list RPAREN { Println $3 }
    | PRINTF LPAREN expression_list RPAREN { Printf $3 }


expr_opt:
    | /* nothing */ { Noexpr }
    | expr          { $1 }


lit:
    | INT_VAL    { IntLit $1 }
    | BOOL_LIT   { BoolLit $1 }
    | STRING_LIT { StringLit $1 }
    | FLOAT_LIT  { FloatLit $1 }


fcall:
    | ID LPAREN expression_list_opt RPAREN { FCall($1, $3) }


expr:
    | lit              { $1 }
    /* TODO add float handling */
    | ID               { Id $1 }
    | expr PLUS   expr { Binop($1, Add,   $3) }
    | expr MINUS  expr { Binop($1, Sub,   $3) }
    | expr TIMES  expr { Binop($1, Mult,  $3) }
    | expr DIVIDE expr { Binop($1, Div,   $3) }
    | expr EQ     expr { Binop($1, Equal, $3) }
    | expr NEQ    expr { Binop($1, Neq,   $3) }
    | expr LT     expr { Binop($1, Less,  $3) }
    | expr LEQ    expr { Binop($1, Leq,   $3) }
    | expr GT     expr { Binop($1, Greater,  $3) }
    | expr GEQ    expr { Binop($1, Geq,   $3) }
    | NEW ID LPAREN actuals_list_opt RPAREN { UserDefInst($2, $4)}
    | fcall            { Call $1 }
    | LPAREN expr RPAREN { $2 }
    | LBRACKET expression_list_opt RBRACKET { ListLit $2 }


expression_list:
    | expression_list_internal    { List.rev $1 }


expression_list_opt:
    | /* nothing */    { [] }
    | expression_list  { $1 }


expression_list_internal:
    | expr                               { [$1] }
    | expression_list_internal COMMA expr { $3 :: $1 }


actuals_list:
    | /* nothing */ { [] }
    | actuals_list_internal  { List.rev $1 }


actuals_list_opt:
    | /* nothing */ { [] }
    | actuals_list  { $1 }


actuals_list_internal:
    /* TODO: allow user defined types */
    | ID ASSIGN expr                    { [Actual($1, $3)] }
    | actuals_list COMMA ID ASSIGN expr { Actual($3, $5) :: $1 }


attr_decl:
    | primtype ID             { NonOption($1 , $2, None) }
    /* we limit the default values to literals */
    | primtype ID ASSIGN lit { NonOption($1 , $2, Some($4)) }
    /* TODO: add optional attributes here */


attribute_list:
    | /* nothing */                 { [] }
    | attribute_list attr_decl SEMI { $2 :: $1 }


class_decl:
    | CLASS ID LBRACE attribute_list RBRACE { $2, $4 }

%%
