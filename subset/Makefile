# makefile for do interpreter

exec = interpreter
CFLAGS = -c
OBJ = scanner.cmo parser.cmo interpreter.cmo

$(exec) : $(OBJ)
	ocamlc -o $@ $(OBJ)

interpreter.cmo : interpreter.ml ast.cmi
	ocamlc $(CFLAGS) interpreter.ml

scanner.cmo : scanner.ml parser.cmi
	ocamlc $(CFLAGS) scanner.ml

parser.cmo : parser.ml parser.cmi
	ocamlc $(CFLAGS) parser.ml

parser.cmi : parser.mli ast.cmi
	ocamlc $(CFLAGS) parser.mli

ast.cmi : ast.ml
	ocamlc $(CFLAGS) $^

parser.ml parser.mli: parser.mly
	ocamlyacc $^

scanner.ml : scanner.mll
	ocamllex $^

.PHONY : clean
clean:
	rm $(exec) *.cmi *.cmo scanner.ml parser.ml parser.mli


