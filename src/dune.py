#!/usr/bin/env python3

# Copyright 2018 Vincent Jacques <vincent@vincent-jacques.net>

import sys


def gen(flavor):
    yield '(ocamllex IsoEbnf_Lexer OCamlETexEbnf_Lexer PythonEbnf_Lexer)'
    yield ''
    yield '(menhir'
    yield '  (infer false)'
    yield '  (modules IsoEbnf_Parser OCamlETexEbnf_Parser PythonEbnf_Parser)'
    yield '  (flags --table)'
    yield ')'
    yield ''
    yield '(library'
    yield '  (name DrawGrammar)'
    yield '  (modules (:standard \ unit_tests draw_grammar draw_grammar_js DrawingTests))'
    yield '  (libraries General JsOfOCairo menhirLib)'
    if flavor == "coverage":
        yield "  (preprocess (pps bisect_ppx))"
    yield ')'
    yield ''
    yield '(executable'
    yield '  (name unit_tests)'
    yield '  (modules unit_tests)'
    yield '  (libraries DrawGrammar)'
    yield '  (js_of_ocaml (flags "+nat.js"))'
    if flavor == "coverage":
        yield "  (preprocess (pps bisect_ppx))"
    yield ')'
    yield ''
    yield '(rule'
    yield '  (targets unit_tests.sentinel)'
    yield '  (deps unit_tests.bc unit_tests.bc.js)'
    yield '  (action (progn'
    yield '    (run %{exe:unit_tests.bc})'
    yield '    (run %{bin:node} unit_tests.bc.js)'
    yield '    (write-file %{targets} sentinel)'
    yield '  ))'
    yield ')'
    yield ''
    yield '(alias'
    yield '  (name runtest)'
    yield '  (deps unit_tests.sentinel)'
    yield ')'
    yield ''
    yield '(executable'
    yield '  (name draw_grammar)'
    yield '  (public_name draw_grammar)'
    yield '  (modules draw_grammar)'
    yield '  (libraries DrawGrammar cairo2)'
    yield ')'
    yield ''
    yield '(executable'
    yield '  (name draw_grammar_js)'
    yield '  (modules draw_grammar_js DrawingTests)'
    yield '  (libraries DrawGrammar)'
    yield '  (preprocess (pps "js_of_ocaml-ppx"))'
    yield '  (js_of_ocaml (flags "+nat.js"))'
    yield ')'

for line in gen(sys.argv[1]):
    print(line)
