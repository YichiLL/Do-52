# a simple "make" command builds the interpreter
# "make printer" builds the printer

CFLAGS = -c
YACCFLAGS = -v
OBJ = indent.cmo scanner.cmo parser.cmo

interpreter : $(OBJ) interpreter.cmo
	ocamlc -o $@ $(OBJ) interpreter.cmo

interpreter.cmo : interpreter.ml ast.cmi
	ocamlc $(CFLAGS) interpreter.ml

indent.cmo : indent.ml
	ocamlc $(CFLAGS) indent.ml

scanner.cmo : scanner.ml parser.cmi indent.cmi
	ocamlc $(CFLAGS) scanner.ml

parser.cmo : parser.ml parser.cmi
	ocamlc $(CFLAGS) parser.ml

parser.cmi : parser.mli ast.cmi
	ocamlc $(CFLAGS) parser.mli

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
	ocamlc -o $@ ast.cmo $(OBJ) printer.cmo

printer.cmo : printer.ml ast.cmo
	ocamlc $(CFLAGS) printer.ml

.PHONY : clean
clean:
	rm interpreter printer *.cmi *.cmo scanner.ml parser.ml parser.mli


