all:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o test ast.cmo lexer.cmo parser.cmo main.cmo
	./test > output.txt

.PHONY: clean
clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli output.txt parser.output test