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

test: cpsmemo.cmx test.ml
	$(OCAMLOPT) -o test cpsmemo.cmx test.ml

cpsmemo.cmo: cpsmemo.ml
cpsmemo.cmx: cpsmemo.ml

clean::
	  rm -f *.cm[iox] *.[oa] *~ test

# test: cpsmemo.cmo test.ml
# 	$(OCAMLC) -o test cpsmemo.cmo test.ml


