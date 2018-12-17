(* Authors:
	Benjamin Lewinter bsl2121
	Irina Mateescu    im2441
	Harry Smith       hs3061
	Yasunari Watanabe yw3239
*)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { TIMES }
| '/'  { DIVIDE }
| ';'  { SEQUENCE }
| '='  { ASSIGN }
| '{'  { LBRACE }
| '}'  { RBRACE }
| '.'  { DOT }
| ','  { COMMA }
| '('  { LPAREN }
| ')'  { RPAREN }
| '<'  { LANGLE }
| '>'  { RANGLE }
| '['  { LBRAK }
| ']'  { RBRAK }
| '\'' { SQUOTE }
| '\"' { DQUOTE }
| ':'  { COLON }
| "=="     { EQ }
| "!="     { NEQ }
| "<="     { LEQ }
| ">="     { GEQ }
| "and"    { AND }
| "or"     { OR }
| "not"    { NOT }
| "int"    { INTTYPE }
| "bool"   { BOOLTYPE }
| "char"   { CHARTYPE }
| "float"  { FLOATTYPE }
| "string" { STRINGTYPE }
| "fun"    { FUNTYPE }
| "void"   { VOIDTYPE }
| "graph"  { GRAPHTYPE }
| "node"   { NODETYPE }
| "-("     { LUEDGE }
| ")-"     { RUEDGE }
| "<("     { LDEDGE }
| ")>"     { RDEDGE }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "for"    { FOR }
| "for_node"    { FORNODE }
| "for_edge"    { FOREDGE }
| "in"     { IN }
| "NULL"   { NULL }
| "return" { RETURN }
| ['0'-'9']+ as int_lit                 { INTLIT(int_of_string int_lit) }
| '\'' ([^'\''] as char_lit) '\''       { CHARLIT(char_lit) }
| ['0'-'9']*'.'['0'-'9']+ as float_lit  { FLOATLIT(float_lit)}
| '\"' ([^'\"']* as string_lit) '\"'    { STRINGLIT(string_lit) }
| ("true" | "false") as bool_lit        { BOOLLIT(bool_of_string bool_lit) }
| ['a'-'z' 'A'-'Z']['0'-'9' 'a'-'z' 'A'-'Z' '_']* as id { VARIABLE(id) }
| eof { EOF }
| "(*" { comment lexbuf }
and comment = parse 
   "*)" { token lexbuf }
	| _ { comment lexbuf }
