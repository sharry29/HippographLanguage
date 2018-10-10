/* Authors:
	Benjamin Lewinter bsl2121
	Irina Mateescu    im2441
	Harry Smith       hs3061
	Yasunari Watanabe yw3239
*/

/*%{ open Ast %}*/

%token PLUS MINUS TIMES DIVIDE SEQUENCE ASSIGN EOF
%token LBRACE RBRACE DOT LPAREN RPAREN LANGLE RANGLE LBRAK RBRAK SQUOTE DQUOTE COLON
%token EQ NEQ LEQ GEQ AND OR NOT
%token INTTYPE BOOLTYPE CHARTYPE FLOATTYPE STRINGTYPE GRAPHTYPE NODETYPE EDGETYPE
%token LUEDGE RUEDGE LDEDGE RDEDGE
%token IF ELSE WHILE FOR IN FUN NULL RETURN VOID
%token <int> INTLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token <bool> BOOLLIT
%token <string> VARIABLE

%left SEQUENCE
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE
%left AND OR
%nonassoc EQ NEQ 
%nonassoc LEQ GEQ LT GT
%right NOT

%start expr
%type <Ast.expr> expr

%%

expr:
  expr PLUS   expr      { Binop($1, Add, $3) }
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
| expr SEQUENCE expr    { Seq($1, $3) }
| INTLIT                { Intlit($1) }
| CHARLIT               { Charlit($1) }
| STRINGLIT             { Stringlit($1) }
| BOOLLIT               { Boollit($1) }
| VARIABLE              { Var($1) }
