export OCAMLMAKEFILE = OCamlMakefile 

OCAMLFIND=ocamlfind -toolchain metaocaml
OCAMLC=ocamlc -short-paths
PACKS=camlimages.png

SOURCES=raytracer_types.ml \
        raytracer_engine.mli raytracer_engine.ml \
        raytracer_engine_staged.mli raytracer_engine_staged.ml \
        raytracer.ml
RESULT=raytracer

all: byte-code

include $(OCAMLMAKEFILE)
