{
  open General.Abbr
  module Array = OCamlStandard.Array
  module Lexing = OCamlStandard.Lexing

  open IsoEbnf_Parser

  exception Error of string

  let error format =
    Frmt.with_result ~f:(fun message -> Exn.raise (Error message)) format
}

let identifier_first_char = ['a'-'z' 'A'-'Z' '_'] | "\\ " | "\\-"

let identifier = identifier_first_char (identifier_first_char | ['0'-'9'])*

let white = [' ' '\t' '\n' '\r']

rule token = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | white { token lexbuf }
  | eof { EOF }

  | ['0'-'9']+ as value { INTEGER (Int.of_string value) }
  | identifier as name { META_IDENTIFIER (Lex.unescape name) }
  | '\'' ([^'\'']+ as value) '\'' { TERMINAL_STRING value }
  | '\'' [^'\'']* eof { error "unexpected end of file in string" }
  | '"' ([^'"']+ as value) '"' { TERMINAL_STRING value }
  | '"' [^'"']* eof { error "unexpected end of file in string" }
  | '?' white* (([^'?' ' ' '\t' '\n' '\r']+ (white+ [^'?' ' ' '\t' '\n' '\r']+)*) as value) white* '?' { SPECIAL_SEQUENCE value }
  | '?' [^'?']* eof { error "unexpected end of file in special sequence" }

  | "(*" { skip_comment 0 lexbuf; token lexbuf }

  | ',' { CONCATENATE_SYMBOL }
  | '=' { DEFINING_SYMBOL }
  | '|'| '/'| '!' { DEFINITION_SEPARATOR_SYMBOL }
  | '-' { EXCEPT_SYMBOL }
  | '*' { REPETITION_SYMBOL }
  | '(' { START_GROUP_SYMBOL }
  | ')' { END_GROUP_SYMBOL }
  | '[' | "(/" { START_OPTION_SYMBOL }
  | ']' | "/)" { END_OPTION_SYMBOL }
  | '{' | "(:" { START_REPEAT_SYMBOL }
  | '}' | ":)" { END_REPEAT_SYMBOL }
  | ';' { TERMINATOR_SYMBOL }
  | _ as c { error "unexpected character %C" c }

and skip_comment i = parse
  | "(*" { skip_comment (i + 1) lexbuf }
  | "*)" { if i > 0 then skip_comment (i - 1) lexbuf }
  | eof { error "unexpected end of file in comment" }
  | '\n' { Lexing.new_line lexbuf; skip_comment i lexbuf }
  | _ { skip_comment i lexbuf}
