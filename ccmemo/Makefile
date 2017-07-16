OCAMLC=ocamlfind ocamlc -g -package batteries,delimcc -linkpkg
OCAMLOPT=ocamlfind ocamlopt -verbose -g -package batteries,delimcc -linkpkg

.SUFFIXES: .ml .mli .cmi .cmo .cmx

%.cmi: %.ml
	$(OCAMLC) -c $(INCLUDES) $<

%.cmo: %.ml 
	$(OCAMLC) -c $(INCLUDES) $<

%.cmx: %.ml 
	$(OCAMLOPT) -c $(INCLUDES) $<

test: ccmemo.ml
	$(OCAMLC) -o test ccmemo.ml

testopt: ccmemo.ml
	$(OCAMLOPT) -o testopt ccmemo.ml

clean::
	  rm -f *.cm[iox] *.[oa] *~ test

