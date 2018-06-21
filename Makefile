OCAML = ocamlfind ocamlopt -package earley,earley.str

all: matching

matching: ast.cmx parser.cmx matching.ml
	$(OCAML) -linkpkg -o $@ $^

ast.cmx: ast.ml
	$(OCAML) -c $<

parser.cmx: parser.ml ast.cmx
	$(OCAML) -pp pa_ocaml -c $<

clean:
	rm -f *~ *.cmx *.cmi *.o

distclean: clean
	rm -f matching
