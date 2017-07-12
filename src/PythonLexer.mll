{
  open General.Abbr
  module Lexing = OCamlStandard.Lexing
  module Array = OCamlStandard.Array

  open PythonParser
}

let identifier = ['a'-'z'] ['a'-'z' '_' '0'-'9']*

rule token = parse
  | [' ' '\n' '\r']+  { token lexbuf }
  | '#' [^'\n']* '\n' { token lexbuf }
  | eof { EOF }

  | (identifier as name) ':' { RULE name }
  | identifier as name { IDENTIFIER name }
  | '\'' ([^'\'']+ as value) '\'' { TERMINAL value }
  | (['A'-'Z']+ as value) { TERMINAL value }

  | '|' { ALTERNATIVE }
  | '(' { START_GROUP }
  | ')' { END_GROUP }
  | '[' { START_OPTION }
  | ']' { END_OPTION }
  | '*' { REPEAT_ZERO }
  | '+' { REPEAT_ONE }
