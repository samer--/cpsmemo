OCAMLC=ocamlfind ocamlc -g -package batteries -linkpkg
OCAMLOPT=ocamlfind ocamlopt -g -package batteries -linkpkg

OBJECTS=cpsmemo.cmo
SOURCES=cpsmemo.cmo

.SUFFIXES: .ml .mli .cmi .cmo .cmx

%.cmi: %.ml
	$(OCAMLC) -c $(INCLUDES) $<

%.cmo: %.ml 
	$(OCAMLC) -c $(INCLUDES) $<

%.cmx: %.ml 
	$(OCAMLOPT) -c $(INCLUDES) $<

testopt: cpsmemo.ml
	$(OCAMLOPT) -o testopt cpsmemo.ml

test: cpsmemo.ml
	$(OCAMLC) -o test cpsmemo.ml

clean::
	  rm -f *.cm[iox] *.[oa] *~ test



