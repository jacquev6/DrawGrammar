{
  open General.Abbr
  module Array = OCamlStandard.Array
  module Lexing = OCamlStandard.Lexing

  open OCamlETexEbnf_Parser

  exception Error of string

  let error format =
    Frmt.with_result ~f:(fun message -> Exn.raise (Error message)) format
}

let identifier = ['a'-'z'] ['a'-'z' '-' '0'-'9']*

rule token = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\r']+ { token lexbuf }
  | "\\end{syntax}" { skip_text lexbuf }
  | eof { EOF }

  | '%' [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | '%' [^'\n']* eof { error "unexpected end of file in LaTeX comment" }
  | (identifier as name) ':' { RULE name }
  | identifier as name { IDENTIFIER name }
  | '"' ([^'"']+ as value) '"' { TERMINAL value }
  | '"' [^'"']* eof { error "unexpected end of file in literal terminal" }
  | '\'' ([^'\'']+ as value) '\'' { TERMINAL value }
  | '\'' [^'\'']* eof { error "unexpected end of file in literal terminal" }

  | "\\\\" { token lexbuf }

  | "...." '.'* { ANYTHING }
  | "||" { ALTERNATIVE }
  | '|' { ALTERNATIVE }
  | '(' { BEGIN_GROUP }
  | ')' { END_GROUP }
  | "{{" { BEGIN_REPEAT_ONE }
  | "}}" { END_REPEAT_ONE }
  | '{' { BEGIN_REPEAT_ZERO }
  | '}' { END_REPEAT_ZERO }
  | '[' { BEGIN_OPTION }
  | ']' { END_OPTION }
  | ';' { token lexbuf }
  | "\\ldots" { RANGE }
  | "..." { RANGE }
  | _ as c { error "unexpected character %C" c }

and skip_text = parse
  | '\n' { Lexing.new_line lexbuf; skip_text lexbuf }
  | "\\begin{syntax}" { token lexbuf }
  | eof { EOF }
  | _ { skip_text lexbuf }

{
  let token ({Lexing.lex_curr_pos; _} as lexbuf) =
    if lex_curr_pos = 0 then
      skip_text lexbuf
    else
      token lexbuf
}
