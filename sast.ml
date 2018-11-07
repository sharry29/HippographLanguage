(*Semantically checked abstract syntax tree*)

open Ast

type sexpr = typ * sx
and sx = 
	SIntlit of int
	| SCharlit of char
	| SStringlit of string
	| SFloatlit of string
	| SBoolLit of bool
	| SFunsig of typ * binding list * sexpr
	| SVar of string
	| SBinop of sexpr * binop * sexpr
	| SUnop of unop * sexpr
	| SAssign of string * sexpr
	| SFCall of string * sexpr list
	| SMCall of sexpr * string * sexpr list
	| SNoexpr

type sstmt =
	SFor of sexpr * sexpr * sexpr * sstmt
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
	typ: typ;
	fname: string;
	args: binding list;
	body: sstmt list;
}

type sprogram = binding list * sfdecl list