%{
  open General.Abbr
  module Lexing = OCamlStandard.Lexing
  module Printf = OCamlStandard.Printf
  module Pervasives = OCamlStandard.Pervasives

  open Grammar
%}

%token <string> IDENTIFIER
%token <string> RULE
%token <string> TERMINAL
%token <string> TOKEN
%token ALTERNATIVE
%token EOF
%token REPEAT_ONE
%token REPEAT_ZERO
%token START_GROUP, END_GROUP
%token START_OPTION, END_OPTION

%start grammar

%type <Grammar.t> grammar

%%

grammar:
  | rules=nonempty_list(rule) EOF
    { grammar rules }

rule:
  | name=RULE definition=definition
    { rule name definition }

definition:
  | alternative=alternative
    { alternative }

alternative:
  | elements=separated_nonempty_list(ALTERNATIVE, sequence)
    { alternative elements }

sequence:
  | elements=nonempty_list(repetition)
    { sequence elements }

repetition:
  | definition=single_definition REPEAT_ZERO
    { repetition null definition }
  | definition=single_definition REPEAT_ONE
    { repetition definition null }
  | definition=single_definition
    { definition }

single_definition:
  | value=TERMINAL
    { terminal value }
  | name=TOKEN
    { token name }
  | name=IDENTIFIER
    { non_terminal name }
  | definition=delimited(START_GROUP, definition, END_GROUP)
    { definition }
  | definition=delimited(START_OPTION, definition, END_OPTION)
    { alternative [null; definition] }
