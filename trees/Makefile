SOURCES=		\
	path.ml		\
	path.mli	\
	tree.ml		\
	tree.mli

OBJECTS=$(patsubst %.ml,%.cmo,$(patsubst %.mli,%.cmi,$(SOURCES)))

all: $(OBJECTS)

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	ocamlc -c -o $@ $<

.ml.cmo:
	ocamlc -c -o $@ $<

clean:
	rm -f *.cmi
	rm -f *.cmo
	rm -f .depend

.depend: Makefile $(SOURCES)
	ocamldep $(SOURCES) > $@

include .depend

.PHONY: clean
