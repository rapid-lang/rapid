%{
    open Ast
    open Datatypes
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token LBRACKET RBRACKET LIST
%token PLUS MINUS TIMES DIVIDE ASSIGN CASTBOOL
%token EQ NEQ LT LEQ GT GEQ AND OR MOD
%token RETURN IF ELSE FOR WHILE FUNC IN
%token PRINTLN PRINTF // LOG
%token CLASS NEW ACCESS OPTIONAL
// %token INT BOOL FLOAT STRING

%token <string> ID TYPE STRING_LIT
%token <int> INT_VAL
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token NULL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN
%left LT GT LEQ GEQ EQ NEQ AND OR
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left ACCESS
%left CASTBOOL


%start program
%type <Ast.program> program


%% /* Parser Rules */


primtype:
    | TYPE { Ast_printer.string_to_t $1 }
    | LIST LT primtype GT { ListType $3 }
    /* todo: add arrays and dicts to primtype */


/* Base level expressions of a program:
 * TODO: Classes */
program:
    | /* nothing */ { [], [], [] }
    | program stmt {
        let (statements, classes, functions) = $1 in
            ($2 :: statements), classes, functions }
    | program func_decl {
        let (statements, classes, functions) = $1 in
            statements, classes, ($2 :: functions) }
    | program class_decl {
        let (statements, classes, functions) = $1 in
            statements, ($2 :: classes), functions }


/* TODO: allow user defined types */
datatype_list:
    | datatype_list COMMA primtype { $3 :: $1 }
    | primtype                 { [$1] }
    | /* nothing */            { [] }


return_type:
    /* TODO: allow user defined types */
    | datatype_list { List.rev $1 }

/*var declarations can now be done inline*/
func_decl:
    // func w/ return types
    | FUNC ID LPAREN arguments RPAREN return_type LBRACE fstmt_list RBRACE
    {{
        fname = $2;
        args = $4;
        return = $6;
        body = List.rev $8
    }}
    /* TODO: unsafe functions */


arguments:
    | /* nothing */ { [] }
    | formal_list   { List.rev $1 }


formal_list:
    /* TODO: allow user defined types */
    | primtype ID { [($1, $2, None)] }
    | primtype ID ASSIGN lit {[($1, $2, Some($4))]}
    | formal_list COMMA primtype ID { ($3, $4, None) :: $1 }
    | formal_list COMMA primtype ID ASSIGN lit {($3, $4, Some($6)) :: $1}


/* a tuple here of (primtype, ID, optional expr) expr is the optional assign */
var_decl:
    | primtype ID             { ($1 , $2, None) }
    | primtype ID ASSIGN expr { ($1 , $2, Some($4)) }


user_def_decl:
    | ID ID             { ($1, $2, None) }
    | ID ID ASSIGN expr { ($1, $2, Some($4)) }


fstmt_list:
    | /* nothing */         { [] }
    | fstmt_list func_stmt { $2 :: $1 }

ret_expr_list:
    | expr {[$1]}
    | ret_expr_list COMMA expr {$3 :: $1}
    | { [] }

func_stmt:
    | RETURN ret_expr_list SEMI { Return( List.rev $2) }
    | stmt       { FStmt($1) }

id_list:
    | id_list COMMA primtype ID { VDecl($3, $4, None) :: $1 }
    | id_list COMMA ID          { ID($3) :: $1 }
    | ID {[ID($1)]}
    | primtype ID {[VDecl($1, $2, None)]}

fcall:
    | ID LPAREN expression_list_opt RPAREN { ($1, $3) }

func_call:
    | fcall                {FuncCall([], $1)}
    | LPAREN id_list RPAREN ASSIGN fcall { FuncCall(List.rev $2, $5) }

stmt_list:
    | {[]}
    | stmt            { [$1] }
    | stmt_list stmt { $2 :: $1 }

stmt:
    | print SEMI        { Output $1 }
    | var_decl SEMI     { VarDecl $1 }
    | user_def_decl SEMI { UserDefDecl $1 }
    | func_call SEMI     { $1 }
    | ID ASSIGN expr SEMI { Assign($1, $3) }
    | FOR LPAREN TYPE ID IN expr RPAREN LBRACE stmt_list RBRACE
        { For(Ast_printer.string_to_t $3, $4, $6, List.rev $9) }
    | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE %prec NOELSE { If($3, List.rev $6, []) }
    | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE { If($3, List.rev $6, List.rev $10) }
    | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, List.rev $6) }

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
    | NULL       { Nullxpr }

expr:
    | lit              { $1 }
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
    | expr AND    expr { Binop($1, And, $3) }
    | expr OR    expr  { Binop($1, Or, $3 )}
    | expr MOD expr    { Binop($1, Mod, $3 )}
    | expr CASTBOOL    { CastBool $1 }
    | primtype LPAREN expr RPAREN { Cast($1, $3) }
    | fcall            { Call $1 }
    | LPAREN expr RPAREN { $2 }
    | NEW ID LPAREN actuals_list_opt RPAREN { UserDefInst($2, $4)}
    | expr ACCESS ID                        { Access($1, $3) }
    | LBRACKET expression_list_opt RBRACKET { ListLit $2 }
    | expr LBRACKET expr RBRACKET { ListAccess($1, $3) }


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
    | primtype ID ASSIGN lit  { NonOption($1 , $2, Some($4)) }
    | OPTIONAL primtype ID    { Optional($2, $3) }


attribute_list:
    | /* nothing */                 { [] }
    | attribute_list attr_decl SEMI { $2 :: $1 }


class_decl:
    | CLASS ID LBRACE attribute_list RBRACE { $2, $4 }

%%
