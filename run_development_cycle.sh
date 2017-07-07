#!/bin/bash

set -o errexit

cd src
ocamlbuild \
    -use-ocamlfind -no-links \
    -plugin-tag "package(js_of_ocaml.ocamlbuild)" \
    -syntax camlp4o -pkgs General,js_of_ocaml,js_of_ocaml.syntax,cairo2 \
    -cflags -w,@a-33-44,-strict-sequence \
    drawing_tests.byte
cd ..

src/_build/drawing_tests.byte
