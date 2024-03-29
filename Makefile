all: calc

calc: re.cmo lexer.cmo parser.cmo calc.cmo
	ocamlc -o $@ lexer.cmo parser.cmo re.cmo calc.cmo

%.cmo: %.ml
	ocamlc -c $<

parser.cmi: parser.mli
	ocamlc -c $<

parser.ml parser.mli: parser.mly
	ocamlyacc $<

lexer.ml: lexer.mll parser.cmi
	ocamllex lexer.mll

clean:
	rm *.cmo *.cmi lexer.ml parser.mli parser.ml calc
