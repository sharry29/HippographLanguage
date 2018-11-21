type typ =
    Int
  | Float
  | Char
  | Fun
  | String
  | Bool
  | Void
  | Graph of typ * typ * typ
  | Node of typ * typ
  | Edge of typ

type binding = typ * string

type binop = 
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Eq
  | Neq
  | Leq
  | Geq
  | Lt
  | Gt

type unop = 
  | Not
  | Neg

type expr = 
  | Intlit of int
  | Charlit of char
  | Stringlit of string
  | Floatlit of string
  | Boollit of bool
  | Funsig of typ * binding list * expr
  | Null
  | Var of string
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Asn of string * expr
  | FCall of string * expr list
  | MCall of expr * string * expr list
  | NodeExpr of expr * expr
  | EdgeExpr of expr * expr * expr
  | GraphExpr of node_list * edge_list
  | Noexpr
and node_list = expr list
and edge_list = expr list

type stmt = 
  | Expr of expr
  | For of expr * expr * expr * stmt
  | ForNode of string * string * stmt
  | ForEdge of string * string * stmt
  | While of expr * stmt
  | If of expr * stmt * stmt
  | Block of stmt list
  | Vdecl of typ * string * expr
  | Return of expr
  | Break
  | Continue

type fdecl = {typ: typ; fname:string; args:binding list; body:stmt list}

type program = binding list * fdecl list

exception Unsupported_constructor;;

let unwrap_node_expr n_expr =
  match n_expr with
  | NodeExpr(label, data) -> (label, data)
  | _ -> raise Unsupported_constructor
;;

let unwrap_edge_expr e_expr =
  match e_expr with
  | EdgeExpr(src, dst, w) -> (src, dst, w)
  | _ -> raise Unsupported_constructor
;;

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Not -> "!"
  | Neg -> "-"

let rec string_of_expr = function
    Intlit(l) -> string_of_int l
  | Floatlit(l) -> l
  | Boollit(true) -> "true"
  | Boollit(false) -> "false"
  | Var(s) -> s
  | Stringlit(l) -> l
  | Null -> "null"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e 
  | Asn(v, e) -> v ^ " = " ^ string_of_expr e
  | FCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")" 
  | MCall(caller, f, el) ->
      string_of_expr caller ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | NodeExpr(e1, e2) -> string_of_expr e1 ^ ": " ^ string_of_expr e2
  | EdgeExpr(src, dst, w) -> "(" ^ (string_of_expr src) ^ ", " ^ (string_of_expr dst) ^ ", " ^ (string_of_expr w) ^ ")"
  | GraphExpr(node_list, edge_list) ->
      "[nodes: [" ^ String.concat ", " (List.map string_of_expr node_list) ^
      "], edges: [" ^ String.concat ", " (List.map string_of_expr edge_list) ^ "]]"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec string_of_typ = function
    Int    -> "int"
  | Bool   -> "bool"
  | Float  -> "float"
  | Void   -> "void"
  | Char   -> "char"
  | Fun    -> "fun"
  | String -> "string"
  | Void   -> "void"
  | Node(nl, nd)  -> "node<" ^ string_of_typ nl ^ ", " ^ string_of_typ nd ^ ">"
  | Edge(vl)      -> "edge<" ^ string_of_typ vl ^ ">"
  | Graph(nl, nd, ew) -> "graph<" ^ string_of_typ nl ^ ", " ^ string_of_typ nd ^ ", " ^ string_of_typ ew ^ ">"

let string_of_vdecl (t, var) = string_of_typ t ^ " " ^ var ^ ";\n"

let string_of_fdecl fdecl = 
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.args) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^ 
  "}\n"

let string_of_program (vars, funcs) = 
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
