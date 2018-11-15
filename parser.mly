/* Authors:
	Benjamin Lewinter bsl2121
	Irina Mateescu    im2441
	Harry Smith       hs3061
	Yasunari Watanabe yw3239
*/

%{
  open Ast
  open Parseraux  
%}

%token PLUS MINUS TIMES DIVIDE SEQUENCE ASSIGN EOF
%token LBRACE RBRACE DOT COMMA LPAREN RPAREN LANGLE RANGLE LBRAK RBRAK SQUOTE DQUOTE COLON
%token EQ NEQ LEQ GEQ AND OR NOT
%token INTTYPE BOOLTYPE CHARTYPE FLOATTYPE STRINGTYPE FUNTYPE GRAPHTYPE NODETYPE EDGETYPE VOIDTYPE
%token LUEDGE RUEDGE LDEDGE RDEDGE
%token IF ELSE NOELSE WHILE FOR FORNODE FOREDGE IN NULL RETURN BREAK CONTINUE
%token <int> INTLIT
%token <string> FLOATLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token <bool> BOOLLIT
%token <string> VARIABLE

%left SEQUENCE
%left DOT
%right ASSIGN
%left AND OR
%nonassoc EQ NEQ 
%nonassoc LEQ GEQ LT GT
%left PLUS MINUS
%left TIMES DIVIDE
%right NEG NOT
%nonassoc LPAREN RPAREN
%nonassoc NOELSE
%nonassoc ELSE

%start program
%type <Ast.program> program

%%

program: decls EOF { $1 }

decls: 
  { [], [] }
| decls vdecl { ($2 :: fst $1), snd $1 }
| decls fdecl { fst $1, ($2 :: snd $1) }

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
  expr SEQUENCE                                            { Expr $1 }
| FOR LPAREN expr SEQUENCE expr SEQUENCE expr RPAREN stmt  { For($3, $5, $7, $9) }
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

expr:
  INTLIT                { Intlit($1) }
| FLOATLIT              { Floatlit($1) }
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
| MINUS expr %prec NEG  { Unop(Neg, $2) }
| NOT expr              { Unop(Not, $2) }
| VARIABLE ASSIGN expr  { Asn($1, $3) }
| VARIABLE LPAREN actuals_opt RPAREN { FCall($1, $3) }
| expr DOT VARIABLE LPAREN actuals_opt RPAREN { MCall($1, $3, $5) }
| LBRAK graph_item_opt RBRAK                  { match $2 with (node_list, edge_list) ->
                                                  GraphExpr(node_list, edge_list) }

graph_item_opt:
  { [], [] }
| graph_item_list { match $1 with (node_list, edge_list) -> 
                      List.rev node_list, List.rev edge_list }

graph_item_list:
  node_edge_list                          { $1 }
| graph_item_list SEQUENCE                { $1 }
| graph_item_list SEQUENCE node_edge_list { merge_node_edge_lists $1 $3 }

node_edge_list:
  node_expr                                   { [$1], [] }
| node_edge_list LUEDGE expr RUEDGE node_expr { update_node_edge_list_with_edge $1 $3 $5  }
| node_edge_list LDEDGE expr RDEDGE node_expr { update_node_edge_list_with_edge $1 $3 $5  }
| node_edge_list LUEDGE expr RDEDGE node_expr { update_node_edge_list_with_redge $1 $3 $5  }
| node_edge_list LDEDGE expr RUEDGE node_expr { update_node_edge_list_with_ledge $1 $3 $5  }

node_expr:
| expr COLON expr { ($1, $3) }
| expr            { ($1, Noexpr) }
