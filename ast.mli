type typ =
    Int
  | Float
  | Char
  | Fun
  | String
  | Bool
  | Void

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
  | Noexpr

type stmt = 
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


(* type stmt =
  For of expr * expr * expr * stmt
  ForNode of string * string * stmt
  ForEdge of string * string * stmt
  While of expr * stmt
  *)