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
%token INTTYPE BOOLTYPE STRINGTYPE FUNTYPE GRAPHTYPE NODETYPE VOIDTYPE
%token LUEDGE RUEDGE LDEDGE RDEDGE
%token IF ELSE NOELSE WHILE FOR FORNODE FOREDGE IN NULL RETURN
%token <int> INTLIT
%token <string> STRINGLIT
%token <bool> BOOLLIT
%token <string> VARIABLE

%left SEQUENCE
%left DOT
%right ASSIGN
%left AND OR
%right COLON
%nonassoc EQ NEQ 
%nonassoc LEQ GEQ LANGLE RANGLE
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
| STRINGTYPE { String }
| FUNTYPE LANGLE typ COLON typ_list_opt RANGLE     { Fun($3, $5) }
| GRAPHTYPE LANGLE typ COLON typ COMMA typ RANGLE  { Graph($3, $5, $7) }
| GRAPHTYPE LANGLE typ COMMA typ RANGLE            { Graph($3, Bool, $5) }
| GRAPHTYPE LANGLE typ COLON typ RANGLE            { Graph($3, $5, Bool) }
| GRAPHTYPE LANGLE typ RANGLE                      { Graph($3, Bool, Bool) }
| NODETYPE  LANGLE typ COLON typ RANGLE            { Node($3, $5) }
| NODETYPE  LANGLE typ RANGLE                      { Node($3, Bool) }

typ_list_opt:
  { [] }
| typ_list { List.rev $1 }

typ_list:
  typ                       { [$1] }
| typ_list COMMA typ { $3 :: $1 }

stmt_list:
  { [] }
| stmt_list stmt { $2 :: $1 }

stmt:
  expr SEQUENCE                                            { Expr $1 }
| FOR LPAREN expr SEQUENCE expr SEQUENCE expr RPAREN stmt  { For($3, $5, $7, $9) }
| FORNODE LPAREN VARIABLE COLON VARIABLE RPAREN stmt       { ForNode($3, Var($5), $7) }
| FOREDGE LPAREN VARIABLE COMMA VARIABLE COMMA VARIABLE COLON VARIABLE RPAREN stmt       { ForEdge($3, $5, $7, Var($9), $11) }
| WHILE LPAREN expr RPAREN stmt                            { While($3, $5) }
| IF LPAREN expr RPAREN stmt %prec NOELSE                  { If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt                     { If($3, $5, $7) }
| LBRACE stmt_list RBRACE                                  { Block(List.rev $2) }
| typ VARIABLE SEQUENCE                                    { Vdecl($1, $2, Noexpr) }
| typ VARIABLE ASSIGN expr SEQUENCE                        { Vdecl($1, $2, Asn($2, $4)) }
| RETURN SEQUENCE                                          { Return Null }
| RETURN expr SEQUENCE                                     { Return $2 }

expr:
  INTLIT                { Intlit($1) }
| STRINGLIT             { Stringlit($1) }
| BOOLLIT               { Boollit($1) }
| typ LPAREN args_opt RPAREN LPAREN expr RPAREN { Funsig($1, $3, $6) }
| NULL                  { Null }
| VARIABLE              { Var($1) }
| LPAREN expr RPAREN    { $2 }
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
| expr LANGLE expr          { Binop($1, Lt, $3) }
| expr RANGLE expr          { Binop($1, Gt, $3) }
| MINUS expr %prec NEG  { Unop(Neg, $2) }
| NOT expr              { Unop(Not, $2) }
| VARIABLE ASSIGN expr  { Asn($1, $3) }
| VARIABLE LPAREN actuals_opt RPAREN { FCall($1, $3) }
| expr DOT VARIABLE LPAREN actuals_opt RPAREN { MCall($1, $3, $5) }
| expr COLON  expr      { NodeExpr($1, $3) }
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
  expr                                   { [construct_node_expr $1], [] }
| node_edge_list LUEDGE expr RUEDGE expr { update_node_edge_list_with_edge $1 $3 (construct_node_expr $5)  }
| node_edge_list LDEDGE expr RDEDGE expr { update_node_edge_list_with_edge $1 $3 (construct_node_expr $5)  }
| node_edge_list LUEDGE expr RDEDGE expr { update_node_edge_list_with_redge $1 $3 (construct_node_expr $5)  }
| node_edge_list LDEDGE expr RUEDGE expr { update_node_edge_list_with_ledge $1 $3 (construct_node_expr $5)  }
| node_edge_list LUEDGE RUEDGE expr      { update_node_edge_list_with_edge $1 Null (construct_node_expr $4)  }
| node_edge_list LDEDGE RDEDGE expr      { update_node_edge_list_with_edge $1 Null (construct_node_expr $4)  }
| node_edge_list LUEDGE RDEDGE expr      { update_node_edge_list_with_redge $1 Null (construct_node_expr $4)  }
| node_edge_list LDEDGE RUEDGE expr      { update_node_edge_list_with_ledge $1 Null (construct_node_expr $4)  }
