SOURCES=		\
	ast.ml		\
	ast.mli		\
	lexer.ml	\
	lexer.mli	\
	parser.ml	\
	parser.mli	\
	path.ml		\
	path.mli	\
	syntax.ml	\
	syntax.mli	\
	tree.ml		\
	tree.mli

OBJECTS=$(patsubst %.ml,%.cmo,$(patsubst %.mli,%.cmi,$(SOURCES)))

all: $(OBJECTS)

.SUFFIXES: .ml .mli .mll .mly .cmi .cmo .cmx

.ml.cmo:
	ocamlc -c -o $@ $<

.mli.cmi:
	ocamlc -c -o $@ $<

.mll.ml:
	ocamllex -o $@ $<

%.ml %.mli: %.mly
	ocamlyacc $<

clean::
	rm -f *.cmi
	rm -f *.cmo
	rm -f .depend
	rm -f parser.ml parser.mli
	rm -f lexer.ml

.depend: Makefile $(SOURCES)
	ocamldep $(SOURCES) > $@

include .depend

.PHONY: clean
