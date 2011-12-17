RUNS=5

CC=cc
CPPFLAGS=-D_GNU_SOURCE
CFLAGS=$(CPPFLAGS) -O3 -Wall -Werror

OCAMLC=ocamlc
OCAMLCFLAGS=
OCAMLOPT=ocamlopt
OCAMLOPTFLAGS=-nodynlink

SOURCES=$(shell ls *.ml | sort)

all: benchmark

benchmark: benchmark.o
	$(CC) $(CFLAGS) -o $@ $<

byte: $(SOURCES:.ml=.byteresult)

opt: $(SOURCES:.ml=.optresult)

clean:
	rm -f *.a *.cm[aiox] *.o
	rm -f benchmark
	rm -f *.byte *.byteresult 
	rm -f *.opt *.optresult

.SUFFIXES: .byte .byteresult .c .cmo .cmx .ml .o .opt .optresult

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

.cmo.byte:
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $<

.cmx.opt:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $<

.byte.byteresult:
	./benchmark -r $(RUNS) $< > $@

.opt.optresult:
	./benchmark -r $(RUNS) $< > $@

$(SOURCES:.ml=.byteresult) $(SOURCES:.ml=.optresult): benchmark

.PHONY: all byte opt clean
