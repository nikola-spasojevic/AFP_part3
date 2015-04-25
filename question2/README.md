# Advanced Functional Programming: Exercise 3, Question 2

## Task: 

Implement staged versions of the operations in `raytracer_types.ml`
and `raytracer_engine.ml`.  See the handout for details.

## Usage:

The ray tracer depends on the `camlimages` library, which you can install using OPAM: 

    opam install camlimages

Build the ray tracer using `make`, and run it with `-staged` or `-unstaged` to select an implementation:

    make
    ./raytracer -unstaged

Running the ray tracer generates an image file `fig.png`.

## Files:

* `raytracer_types.ml`: Basic types and functions.  (You do not need
  to modify this file.)

* `raytracer_engine.ml`, `raytracer_engine.mli`: the unstaged ray
  tracer implementation.  (You do not need to modify this file.)

* `raytracer_engine_staged.ml`, `raytracer_engine_staged.mli`:
  placeholder for the staged ray tracer implementation.  Fill in this
  file to complete the exercise.

* `raytracer.ml`: the entry module that calls the ray tracer to
  generate a particular scene.  (You do not need to modify this file,
  but you might enjoy seeing the effects of changing the scene
  definition and other parameters.)

* `Makefile`, `OCamlMakefile`: build system.  You need the `make`
  program (standard on Unix systems) to execute these files.

* `README.md`: this file.

## Acknowledgements

This ray tracer implementation is based on Python code by Cyrille Rossant:

    https://gist.github.com/rossant/6046463
