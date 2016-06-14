OCB_FLAGS = -use-ocamlfind -I CertReader -I JVMReader -I Translation -I TypeInference
OCB = 		ocamlbuild $(OCB_FLAGS)

all:	byte

byte:	sanity
	$(OCB) main.byte

clean:
	$(OCB) -clean

lib:
	$(OCB) CertReader.cma
	$(OCB) JVMReader.cma
	$(OCB) Translation.cma
	$(OCB) TypeInference.cma

.PHONY: 	all clean byte native profile debug lib sanity test
