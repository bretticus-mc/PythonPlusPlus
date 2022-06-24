SOURCES = scanner.mll \
          parser.mly \
          sast.ml \
          ast.ml \
          semant.ml \
          parser.ml \
          scanner.ml \
          test2.ml 


OCAMLBUILD = ocamlbuild test2.native

all: 
	$(OCAMLBUILD) test2.native parser semant

clean:
	$(OCAMLBUILD) -clean
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml 




