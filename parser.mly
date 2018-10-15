/* Authors:
	Benjamin Lewinter bsl2121
	Irina Mateescu    im2441
	Harry Smith       hs3061
	Yasunari Watanabe yw3239
*/

%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE SEQUENCE ASSIGN EOF
%token LBRACE RBRACE DOT COMMA LPAREN RPAREN LANGLE RANGLE LBRAK RBRAK SQUOTE DQUOTE COLON
%token EQ NEQ LEQ GEQ AND OR NOT
%token INTTYPE BOOLTYPE CHARTYPE FLOATTYPE STRINGTYPE FUNTYPE GRAPHTYPE NODETYPE EDGETYPE VOIDTYPE
%token LUEDGE RUEDGE LDEDGE RDEDGE
%token IF ELSE NOELSE WHILE FOR FORNODE FOREDGE IN NULL RETURN BREAK CONTINUE
%token <int> INTLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token <bool> BOOLLIT
%token <string> VARIABLE

%left SEQUENCE
%left DOT
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE
%left AND OR
%nonassoc EQ NEQ 
%nonassoc LEQ GEQ LT GT
%right NOT
%nonassoc LPAREN RPAREN
%nonassoc NOELSE
%nonassoc ELSE

%start program
%type <Ast.program> program

%%

program: decls EOF { $1 }

decls: 
  { [], [] }
| program vdecl { ($2 :: fst $1), snd $1 }
| program fdecl { fst $1, ($2 :: snd $1) }

vdecl:
  typ VARIABLE SEQUENCE { ($1, $2) }

fdecl:
  typ VARIABLE LPAREN args_opt RPAREN LBRACE stmt_list RBRACE 
    { { typ = $1; fname = $2; args = $4; body = List.rev $7 } }

args_opt:
  { [] }
| args_list { List.rev $1 }

args_list:
  typ VARIABLE                    { [($1, $2)] }
| args_list SEQUENCE typ VARIABLE { ($3, $4) :: $1 }



actuals_opt:
  { [] }
| actuals_list { List.rev $1 }

actuals_list:
  expr                       { [$1] }
| actuals_list SEQUENCE expr { $3 :: $1 }

typ:
  VOIDTYPE   { Void }
| INTTYPE    { Int }
| BOOLTYPE   { Bool }
| CHARTYPE   { Char }
| FLOATTYPE  { Float}
| STRINGTYPE { String }
| FUNTYPE    { Fun }
| GRAPHTYPE LANGLE typ COLON typ COMMA typ RANGLE  { Graph($3, $5, $7) }
| NODETYPE  LANGLE typ COLON typ RANGLE { Node($3, $5) }
| EDGETYPE  LANGLE typ RANGLE { Edge($3) }

stmt_list:
  { [] }
| stmt_list stmt { $2 :: $1 }

stmt:
  FOR LPAREN expr SEQUENCE expr SEQUENCE expr RPAREN stmt  { For($3, $5, $7, $9) }
| FORNODE LPAREN VARIABLE COLON VARIABLE RPAREN stmt       { ForNode($3, $5, $7) }
| FOREDGE LPAREN VARIABLE COLON VARIABLE RPAREN stmt       { ForEdge($3, $5, $7) }
| WHILE LPAREN expr RPAREN stmt                            { While($3, $5) }
| IF LPAREN expr RPAREN stmt %prec NOELSE                  { If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt                     { If($3, $5, $7) }
| LBRACE stmt_list RBRACE                                  { Block(List.rev $2) }
| typ VARIABLE SEQUENCE                                    { Vdecl($1, $2, Noexpr) }
| typ VARIABLE ASSIGN expr SEQUENCE                        { Vdecl($1, $2, Asn($2, $4)) }
| RETURN SEQUENCE                                          { Return Noexpr }
| RETURN expr SEQUENCE                                     { Return $2 }
| BREAK SEQUENCE                                           { Break }
| CONTINUE SEQUENCE                                        { Continue }

/*

x = lambda (n1, n2): n1 + n2

void main() {
  fun x =  int (int n1; int n2) (n1 + n2);

  int res = x(5,4);

}

*/

expr:
  INTLIT                { Intlit($1) }
| CHARLIT               { Charlit($1) }
| STRINGLIT             { Stringlit($1) }
| BOOLLIT               { Boollit($1) }
| typ LPAREN args_opt RPAREN LPAREN expr RPAREN { Funsig($1, $3, $6) }
| NULL                  { Null }
| VARIABLE              { Var($1) }
| expr PLUS   expr      { Binop($1, Add, $3) }
| expr MINUS  expr      { Binop($1, Sub, $3) }
| expr TIMES  expr      { Binop($1, Mul, $3) }
| expr DIVIDE expr      { Binop($1, Div, $3) }
| expr AND expr         { Binop($1, And, $3) }
| expr OR expr          { Binop($1, Or, $3) }
| expr EQ expr          { Binop($1, Eq, $3) }
| expr NEQ expr         { Binop($1, Neq, $3) }
| expr LEQ expr         { Binop($1, Leq, $3) }
| expr GEQ expr         { Binop($1, Geq, $3) }
| expr LT expr          { Binop($1, Lt, $3) }
| expr GT expr          { Binop($1, Gt, $3) }
| NOT expr              { Unop(Not, $2) }
| VARIABLE ASSIGN expr  { Asn($1, $3) }
| VARIABLE LPAREN actuals_opt RPAREN { FCall($1, $3) }
| expr DOT VARIABLE LPAREN actuals_opt RPAREN { MCall($1, $3, $5) }
| LBRAK graph_item_opt RBRAK                  { GraphExpr($2) }

graph_item_opt:
  { [] }
| graph_item_list { List.rev $1 }

graph_item_list:
  node_edge_list                          { [$1] }
| graph_item_list SEQUENCE                { [$1] }
| graph_item_list SEQUENCE node_edge_list { $3 :: $1 }

node_edge_list:
  node_expr                          { [$1] }
| node_edge_list edge_expr node_expr { $3 :: $2 :: $1 }

edge_expr:
  LUEDGE expr RUEDGE { Edge($2) }
| LDEDGE expr RDEDGE { Edge($2) }
| LUEDGE expr RDEDGE { Redge($2)}
| LDEDGE expr RUEDGE { Ledge($2) }

node_expr:
| expr COLON expr { Node($1, $3) }
| expr            { Node($1, Noexpr)}
