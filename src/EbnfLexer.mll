{
  open General.Abbr
  module Lexing = OCamlStandard.Lexing
  module Array = OCamlStandard.Array

  open EbnfParser
}

let white = [' ' '\t' '\n' '\r']

rule token = parse
  | white  { token lexbuf }
  | eof   { EOF }

  | ['0'-'9']+ as value { INTEGER (Int.of_string value) }
  | ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as name { META_IDENTIFIER name }
  | '\'' ([^'\'']+ as value) '\'' { TERMINAL_STRING value }
  | '"' ([^'"']+ as value) '"' { TERMINAL_STRING value }
  | '?' white* (([^'?' ' ' '\t' '\n' '\r']+ (white+ [^'?' ' ' '\t' '\n' '\r']+)*) as value) white* '?' { SPECIAL_SEQUENCE value }

  | ',' { CONCATENATE_SYMBOL }
  | '=' { DEFINING_SYMBOL }
  | '|'| '/'| '!' { DEFINITION_SEPARATOR_SYMBOL } (* @todo Why? *)
  | '-' { EXCEPT_SYMBOL }
  | '*' { REPETITION_SYMBOL }
  | '(' { START_GROUP_SYMBOL }
  | ')' { END_GROUP_SYMBOL }
  | '[' | "(/" { START_OPTION_SYMBOL }
  | ']' | "/)" { END_OPTION_SYMBOL }
  | '{' | "(:" { START_REPEAT_SYMBOL }
  | '}' | ":)" { END_REPEAT_SYMBOL }
  | ';' { TERMINATOR_SYMBOL }
