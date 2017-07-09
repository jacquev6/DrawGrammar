#!/bin/bash

set -o errexit

function build {
  ocamlbuild -use-ocamlfind -no-links -tag debug -plugin-tag "package(js_of_ocaml.ocamlbuild)" $@
}

cd src
build unit_tests.byte
cd ..
src/_build/unit_tests.byte

cd src
build draw_grammar.byte
cd ..
src/_build/draw_grammar.byte *.ebnf
echo
echo "Have a look at $(pwd)/*.png"
echo

cd src
build draw_grammar_js.js drawing_tests.js
cd ..
echo
echo "Have a look at $(pwd)/drawing_tests.html"
echo
echo "Have a look at $(pwd)/demo.html"
echo
