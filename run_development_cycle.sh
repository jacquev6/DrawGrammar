#!/bin/bash

set -o errexit

cd src
ocamlbuild \
    -use-ocamlfind -no-links \
    -plugin-tag "package(js_of_ocaml.ocamlbuild)" \
    -syntax camlp4o -pkgs General,js_of_ocaml,js_of_ocaml.syntax,cairo2 \
    -cflags -w,@a-33-44,-strict-sequence \
    unit_tests.js drawing_tests.byte draw_grammar.byte draw_grammar_js.js
cd ..

src/_build/unit_tests.byte
nodejs src/_build/unit_tests.js
src/_build/drawing_tests.byte

src/_build/draw_grammar.byte *.ebnf

echo "Have a look at $(pwd)/demo.html"
