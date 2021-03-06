# a simple "make" command builds the compiler
# "make printer" builds the AST printer

CFLAGS = -c
YACCFLAGS = -v

OBJ = ast.cmo indent.cmo scanner.cmo parser.cmo cache.cmo sast.cmo\
	  stdlib.cmo semantic.cmo

compile : $(OBJ) compile.cmo
	ocamlc -o $@ str.cma $(OBJ) compile.cmo

compile.cmo : compile.ml ast.cmi
	ocamlc $(CFLAGS) $<

indent.cmo : indent.ml
	ocamlc $(CFLAGS) indent.ml

cache.cmo : cache.ml scanner.ml parser.ml
	ocamlc $(CFLAGS) cache.ml

semantic.cmo : semantic.ml
	ocamlc $(CFLAGS) semantic.ml

scanner.cmo : scanner.ml parser.cmi indent.cmi
	ocamlc $(CFLAGS) scanner.ml

parser.cmo : parser.ml parser.cmi
	ocamlc $(CFLAGS) parser.ml

parser.cmi : parser.mli ast.cmi
	ocamlc $(CFLAGS) parser.mli

stdlib.cmo : stdlib.ml
	ocamlc $(CFLAGS) stdlib.ml

sast.cmi : sast.ml
	ocamlc $(CFLAGS) $^

sast.cmo: sast.ml
	ocamlc $(CFLAGS) $^

ast.cmi : ast.ml
	ocamlc $(CFLAGS) $^

ast.cmo : ast.ml
	ocamlc $(CFLAGS) $^

parser.ml parser.mli: parser.mly
	ocamlyacc $(YACCFLAGS) $^

scanner.ml : scanner.mll
	ocamllex $^

.PHONY : printer
printer : $(OBJ) printer.cmo
	ocamlc -o $@ str.cma $(OBJ) printer.cmo

printer.cmo : printer.ml ast.cmo cache.cmo
	ocamlc $(CFLAGS) printer.ml

.PHONY : clean
clean:
	rm compile printer *.cmi *.cmo scanner.ml parser.ml parser.mli *.output *.java
