all:
	ocamlc -c ast.ml
	ocamlyacc -v parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c main.ml
	ocamlc -o test ast.cmo lexer.cmo parser.cmo main.cmo
	./test > output.txt

.PHONY: clean
clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli output.txt parser.output test