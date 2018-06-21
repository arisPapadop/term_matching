all: matching

matching: ast.cmx matching.ml
	ocamlopt -o $@ $^

ast.cmx: ast.ml
	ocamlopt -c $<

clean:
	rm -f *.cmx *.cmi *.o matching
