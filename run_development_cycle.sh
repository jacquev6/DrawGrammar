#!/bin/bash

set -o errexit

eval `opam config env`
opam install --yes JsOfOCairo General menhir jbuilder bisect_ppx bisect-summary
clear

for p in src/*_Parser.mly
do
  m=${p%_Parser.mly}_Messages.messages
  c=${p%_Parser.mly}_Messages.ml
  menhir --list-errors $p > $m.dummies
  if [ -f $m ]
  then
    menhir --compare-errors $m.dummies --compare-errors $m $p
    rm $m.dummies
  else
    mv $m.dummies $m
  fi
  menhir --compile-errors $m $p > $c
done

function switch_flavor {
  mkdir -p _builds/$1
  rm -rf _build
  ln -sf _builds/$1 _build
}

switch_flavor coverage

# https://github.com/aantron/bisect_ppx/blob/master/doc/advanced.md#Jbuilder suggests
# modifying the jbuild file for release. Let's modify it for tests instead.
sed -i "s/^;\(.*bisect_ppx.*\)$/\1/" src/jbuild
jbuilder runtest --dev
sed -i "s/^\(.*bisect_ppx.*\)$/;\1/" src/jbuild
if [ -f _builds/coverage/default/src/bisect????.out ]
then
  echo
  bisect-summary _builds/coverage/default/src/bisect????.out
  echo
  bisect-ppx-report -I _builds/coverage/default -html _builds/coverage/bisect _builds/coverage/default/src/bisect????.out
  echo "See coverage report in $(pwd)/_builds/coverage/bisect/index.html"
else
  echo "Coverage report from previous test run: $(pwd)/_builds/coverage/bisect/index.html"
fi

echo
switch_flavor debug
jbuilder build --dev src/draw_grammar.bc src/draw_grammar_js.bc.js

echo
_builds/debug/default/src/draw_grammar.bc --help

echo
cd docs
[ -e python2.7.python-ebnf ] || wget https://raw.githubusercontent.com/python/cpython/2.7/Grammar/Grammar --output-document python2.7.python-ebnf
[ -e python3.6.python-ebnf ] || wget https://raw.githubusercontent.com/python/cpython/3.6/Grammar/Grammar --output-document python3.6.python-ebnf
[ -e lex.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/lex.etex --output-document lex.ocaml-etex-ebnf
[ -e names.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/names.etex --output-document names.ocaml-etex-ebnf
[ -e types.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/types.etex --output-document types.ocaml-etex-ebnf
[ -e const.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/const.etex --output-document const.ocaml-etex-ebnf
[ -e patterns.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/patterns.etex --output-document patterns.ocaml-etex-ebnf
[ -e expr.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/expr.etex --output-document expr.ocaml-etex-ebnf
[ -e typedecl.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/typedecl.etex --output-document typedecl.ocaml-etex-ebnf
[ -e classes.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/classes.etex --output-document classes.ocaml-etex-ebnf
[ -e modtypes.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/modtypes.etex --output-document modtypes.ocaml-etex-ebnf
[ -e modules.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/modules.etex --output-document modules.ocaml-etex-ebnf
[ -e compunit.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/compunit.etex --output-document compunit.ocaml-etex-ebnf
[ -e exten.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/exten.etex --output-document exten.ocaml-etex-ebnf
../_builds/debug/default/src/draw_grammar.bc arithmetics.iso-ebnf --inline digit --inline factor --inline not_a_rule
mv arithmetics.iso-ebnf.png arithmetics-inlined-digit-factor.iso-ebnf.png
../_builds/debug/default/src/draw_grammar.bc arithmetics.iso-ebnf --inline digit,factor,term --inline-keep integer
mv arithmetics.iso-ebnf.png arithmetics-inlined-all.iso-ebnf.png
../_builds/debug/default/src/draw_grammar.bc *.*-ebnf
mv *.*-ebnf.png ..
cd ..
echo
echo "Have a look at $(pwd)/*.png"
echo

echo
echo "Have a look at $(pwd)/drawing_tests.html"
echo

switch_flavor release
jbuilder build src/draw_grammar_js.bc.js
cp _builds/release/default/src/draw_grammar_js.bc.js docs
echo
echo "Have a look at $(pwd)/docs/index.html"

switch_flavor none

echo
echo "Development cycle OK"
