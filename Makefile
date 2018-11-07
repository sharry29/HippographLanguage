hpg:
	ocamllex scanner.mll
	ocamlyacc -v parser.mly
	ocamlc -c ast.mli
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parseraux.ml
	ocamlc -c parser.ml
	ocamlc -c hippograph.ml
	ocamlc -o hippograph parseraux.cmo parser.cmo scanner.cmo hippograph.cmo

test:
	ocamllex scanner.mll
	ocamlyacc -v parser.mly
	ocamlc -c ast.mli
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parseraux.ml
	ocamlc -c parser.ml
	ocamlc -c hippograph.ml
	ocamlc -o hippograph parseraux.cmo parser.cmo scanner.cmo hippograph.cmo
	./hippograph < hello.hpg