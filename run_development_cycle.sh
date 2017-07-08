#!/bin/bash

set -o errexit

cd src
ocamlbuild \
    -use-ocamlfind -no-links \
    -plugin-tag "package(js_of_ocaml.ocamlbuild)" \
    -pkgs General,JsOfOCairo,js_of_ocaml,js_of_ocaml.ppx,cairo2 \
    -cflags -w,@a-33-44,-strict-sequence \
    drawing_tests_js.js draw_grammar_js.js draw_grammar.byte
cd ..

echo "Have a look at $(pwd)/drawing_tests.html"

src/_build/draw_grammar.byte *.ebnf

echo "Have a look at $(pwd)/*.png"

echo "Have a look at $(pwd)/demo.html"
