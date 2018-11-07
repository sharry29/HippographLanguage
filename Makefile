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

# "make microc.native" compiles the compiler

.PRECIOUS : hippograph.native
hippograph.native :
	opam config exec -- \
	ocamlbuild -no-hygiene -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		hippograph.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff