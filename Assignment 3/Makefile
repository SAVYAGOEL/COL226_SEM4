all: lexer.ml test.ml input.txt
	@ocamlc -o test lexer.ml test.ml
	@./test < input.txt > output.txt

lexer.ml: lexer.mll
	@ocamllex lexer.mll

input.txt : 
	@touch input.txt

.PHONY: clean
clean:
	@rm -f test lexer.ml *.cmo *.cmi output.txt
