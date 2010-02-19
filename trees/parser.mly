/*** parser.mly - ocamlyacc parser for Ast ***/

%{
  let mkapp (e:Ast.expression) (el:Ast.expression list): Ast.expression =
    let rec mkapp_helper el =
      match el with
          [] -> e
        | e :: el -> Ast.Exp_app(mkapp_helper el, e)
    in mkapp_helper el
%}

%token <int> INT
%token <bool> BOOL
%token <Ast.identifier> ID
%token <int> PROJ
%token PLUS MINUS TIMES
%token LSS GRT LEQ GEQ EQ
%token LPAREN RPAREN
%token LET REC IN
%token IF THEN ELSE
%token LAMBDA DOT COMMA
%token EOF

%start main
%type <Ast.operator> op
%type <Ast.constant> const
%type <Ast.expression> value
%type <Ast.expression list> value_comma_list
%type <Ast.expression> expr
%type <Ast.expression list> expr_comma_list
%type <Ast.expression> simple_expr
%type <Ast.expression> simple_app_expr
%type <Ast.expression> main

%%

op:
  PLUS  { Ast.Op_add }
| MINUS { Ast.Op_sub }
| TIMES { Ast.Op_mul }
| LSS   { Ast.Op_lss }
| GRT   { Ast.Op_grt }
| LEQ   { Ast.Op_leq }
| GEQ   { Ast.Op_geq }
| EQ    { Ast.Op_eq  }
;

const:
  INT  { Ast.Const_int $1  }
| BOOL { Ast.Const_bool $1 }
| op   { Ast.Const_operator $1 }
| PROJ { Ast.Const_proj $1 }
;

value:
  const                                      { Ast.Exp_const $1 }
| LAMBDA ID DOT expr                         { Ast.Exp_abstr ($2, $4) }
| LPAREN value COMMA value_comma_list RPAREN { Ast.Exp_tuple ($2 :: $4) }
| LPAREN value RPAREN                        { $2 }
;

value_comma_list:
  value                        { [$1] }
| value COMMA value_comma_list { $1 :: $3 }
;

expr:
  simple_app_expr             { $1 }
| LAMBDA ID DOT expr          { Ast.Exp_abstr ($2, $4) }
| IF expr THEN expr ELSE expr { Ast.Exp_if ($2, $4, $6) }
| LET ID EQ expr IN expr      { Ast.Exp_let ($2, $4, $6) }
| LET REC ID EQ value IN expr { Ast.Exp_letrec ($3, $5, $7) }
;

expr_comma_list:
  expr                       { [$1] }
| expr COMMA expr_comma_list { $1 :: $3 }
;

simple_expr:
  const                                    { Ast.Exp_const $1 }
| ID                                       { Ast.Exp_ident $1 }
| LPAREN expr RPAREN                       { $2 }
| LPAREN expr COMMA expr_comma_list RPAREN { Ast.Exp_tuple ($2 :: $4) }
;

simple_app_expr:
  simple_expr                 { $1 }
| simple_app_expr simple_expr { Ast.Exp_app($1, $2) }
;

main:
  expr EOF { $1 }
;
