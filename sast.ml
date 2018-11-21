(*Semantically checked abstract syntax tree*)

open Ast

type sexpr = typ * sx
and sx = 
	  SIntlit of int
  | SCharlit of char
  | SStringlit of string
  | SFloatlit of string
  | SBoollit of bool
  | SFunsig of typ * binding list * sexpr
  | SNull
  | SVar of string
  | SBinop of sexpr * binop * sexpr
  | SUnop of unop * sexpr
  | SAsn of string * sexpr
  | SFCall of string * sexpr list
  | SMCall of sexpr * string * sexpr list
  | SNodeExpr of sexpr * sexpr
  | SEdgeExpr of sexpr * sexpr * sexpr
  | SGraphExpr of node_list * edge_list
  | SNoexpr
and node_list = sexpr list
and edge_list = sexpr list

type sstmt =
	  SExpr of sexpr
  | SFor of sexpr * sexpr * sexpr * sstmt
	| SForNode of string * string * sstmt
	| SForEdge of string * string * sstmt
	| SWhile of sexpr * sstmt
	| SIf of sexpr * sstmt * sstmt
	| SBlock of sstmt list
	| SVdecl of typ * string * sexpr
	| SReturn of sexpr
	| SBreak
	| SContinue

type sfdecl = {
	styp: typ;
	sfname: string;
	sargs: binding list;
	sbody: sstmt list;
}

type sprogram = binding list * sfdecl list

let rec string_of_sexpr (_, e) =
  match e with
    SIntlit(l) -> string_of_int l
  | SBoollit(true) -> "true"
  | SBoollit(false) -> "false"
  | SFloatlit(l) -> l
  | SVar(s) -> s
  | SStringlit(l) -> l
  | SNull -> "null"
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e 
  | SAsn(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SFCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  (* | SMCall(f, el, t) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")" *)
  | SNodeExpr(e1, e2) -> string_of_sexpr e1 ^ ": " ^ string_of_sexpr e2
  | SEdgeExpr(src, dst, w) -> "(" ^ (string_of_sexpr src) ^ ", " ^ (string_of_sexpr dst) ^ ", " ^ (string_of_sexpr w) ^ ")"
  | SGraphExpr(node_list, edge_list) ->
      "[nodes: [" ^ String.concat ", " (List.map string_of_sexpr node_list) ^
      "], edges: [" ^ String.concat ", " (List.map string_of_sexpr edge_list) ^ "]]"
  | SNoexpr -> ""

let string_of_svdecl (t, var) = string_of_typ t ^ " " ^ var ^ ";\n"

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SVdecl(t, var, expr) -> 
      match expr with
      | (_, SNoexpr) ->
          string_of_svdecl (t, var)
      | _ ->
          string_of_typ t ^ " " ^ string_of_sexpr expr ^ ";\n"

let string_of_sfdecl fdecl = string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sargs) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"


let string_of_sprogram (vars, funcs) =
  (match vars with
   | [] -> ""
   | _ -> String.concat "" (List.map string_of_svdecl vars) ^ "\n") ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
