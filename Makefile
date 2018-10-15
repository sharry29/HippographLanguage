hpg:
	ocamllex scanner.mll
	ocamlyacc -v parser.mly
	ocamlc -c ast.mli
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c hippograph.ml
	ocamlc -o hippograph parser.cmo scanner.cmo hippograph.cmo