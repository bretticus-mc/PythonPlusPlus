all: pythonpp.native

# Type "make" to compile all files
# opam config exec -- 
# ocamlbuild -use-ocamlfind -pkgs str pythonpp.native 

pythonpp.native:
	ocamlbuild -pkgs llvm pythonpp.native
	ocamlbuild test1.native
	ocamlbuild test2.native

repl: parser.cmo scanner.cmo ast.cmo sast.cmo semant.cmo repl.cmo
	ocamlc str.cma -w A -o repl $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

%.cmi : %.ml
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

pythonpp.out : pythonpp pythonpp.tb
	./pythonpp < pythonpp.tb > pythonpp.out

# Dependencies from ocamldep
repl.cmo : scanner.cmo parser.cmi ast.cmi sast.cmi semant.cmi
repl.cmx : scanner.cmx parser.cmx ast.cmi sast.cmi semant.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -rf \
	*.cmi *.cmo parser.ml parser.mli parser.output scanner.ml \
		repl.out repl *.out __pycache__ _build pythonpp.native \
	*.ll *.exe *.s *.o testall.log