nomfic = ""

binary:
	dune build main.exe
	
parsing:
	ocamlfind ocamlopt -o parsing str.cmxa -linkpkg parsing.ml

byte:
	dune build main.bc

clean:
	dune clean
	
parse:$(nomfic)
	./parsing $(nomfic)

