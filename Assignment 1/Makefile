all: vector.cmo test_cases.cmo
	@ocamlc vector.cmo test_cases.cmo -o program
	@./program

vector.cmo: vector.ml
	@ocamlc -c vector.ml

test_cases.cmo: test_cases.ml
	@ocamlc -c test_cases.ml

.PHONY: clean
clean:
	@rm -f *.cmo *.cmi program