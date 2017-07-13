open General.Abbr

module Lexing = OCamlStandard.Lexing
module Printf = OCamlStandard.Printf

let set_file_name lexbuf = Lexing.(function
  | None -> ()
  | Some file_name ->
    lexbuf.lex_start_p <- {lexbuf.lex_start_p with pos_fname=file_name};
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname=file_name};
)

module Errors = struct
  exception Lexing of string
  exception Parsing of string

  let position_to_string {Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum; _} =
    let file = match pos_fname with
      | "" -> ""
      | _ -> Printf.sprintf "file %S, " pos_fname
    in
    Printf.sprintf "%sline %n, character %n" file pos_lnum (pos_cnum - pos_bol + 1)

  let lexing position message =
    raise (Lexing (Printf.sprintf "%s: lexing error: %s" (position_to_string position) message))

  let parsing position message =
    raise (Parsing (Printf.sprintf "%s: parsing error: %s" (position_to_string position) message))
end

module Make(Parser: sig
  type token
  val syntax: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Grammar.t
  exception Error

  module MenhirInterpreter: sig
    include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE with type token = token
  end

  module Incremental: sig
    val syntax: Lexing.position -> Grammar.t MenhirInterpreter.checkpoint
  end
end)(Messages: sig
  val message: int -> string
end)(Lexer: sig
  val token: Lexing.lexbuf -> Parser.token
  exception Error of string
end) = struct
  let parse_lexbuf ?file_name lexbuf =
    set_file_name lexbuf file_name;
    try
      Parser.MenhirInterpreter.loop_handle
        identity
        (function
          | Parser.MenhirInterpreter.HandlingError env ->
            env
            |> Parser.MenhirInterpreter.current_state_number
            |> Messages.message
            |> Errors.parsing (Lexing.lexeme_start_p lexbuf)
          | _ -> Errors.parsing (Lexing.lexeme_start_p lexbuf) "unknown"
        )
        (Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.token lexbuf)
        (Parser.Incremental.syntax lexbuf.Lexing.lex_curr_p)
    with
      | Lexer.Error message -> Errors.lexing (Lexing.lexeme_start_p lexbuf) message

  let parse_chan ?file_name chan =
    chan
    |> Lexing.from_channel
    |> parse_lexbuf ?file_name

  let parse_file file_name =
    let chan = open_in file_name in
    let g = parse_chan ~file_name chan in
    close_in chan;
    g

  let parse_string ?file_name code =
    code
    |> Lexing.from_string
    |> parse_lexbuf ?file_name
end

module IsoEbnf = Make(IsoEbnfParser)(IsoEbnfParser_messages)(IsoEbnfLexer)

module PythonEbnf = Make(PythonEbnfParser)(PythonEbnfParser_messages)(PythonEbnfLexer)

(* @todo Parse Mehnir/ocamlyacc's .mly files *)

module Syntax = struct
  type t =
    | IsoEbnf
    | PythonEbnf

  let of_string = function
    | "iso-ebnf" -> IsoEbnf
    | "python-ebnf" -> PythonEbnf
    | syntax -> failwith (Printf.sprintf "Unknown grammar syntax %s" syntax)
end

let parse_string ~syntax s =
  match syntax with
    | Syntax.IsoEbnf -> IsoEbnf.parse_string s
    | Syntax.PythonEbnf -> PythonEbnf.parse_string s

let parse_file name =
  let syntax =
    name
    |> Str.split ~sep:"."
    |> Li.reverse
    |> Li.head
    |> Syntax.of_string
  in
  match syntax with
    | Syntax.IsoEbnf -> IsoEbnf.parse_file name
    | Syntax.PythonEbnf -> PythonEbnf.parse_file name

module IsoEbnfUnitTests = struct
  open Tst

  let make s expected =
    s >:: (fun _ ->
      check_poly ~to_string:Grammar.to_string Grammar.(grammar [rule "r" expected]) (parse_string ~syntax:Syntax.IsoEbnf (Printf.sprintf "r = %s;" s))
    )

  let t = Grammar.terminal "t"
  let v1 = Grammar.non_terminal "v1"
  let v2 = Grammar.non_terminal "v2"
  let v3 = Grammar.non_terminal "v3"
  let v4 = Grammar.non_terminal "v4"
  let s = Grammar.sequence
  let a = Grammar.alternative
  let r = Grammar.repetition
  let n = Grammar.null
  let sp = Grammar.special
  let ex = Grammar.except

  let test = "IsoEbnf" >::: [
    make "'t'" t;
    make "'t' (* foobar *)" t;
    make "\"t\"" t;
    make "v1" v1;
    make "v1, v2, v3, v4" (s [v1; v2; v3; v4]);
    make "v1, (v2, v3), v4" (s [v1; v2; v3; v4]);
    make "v1 | v2 ! v3 / v4" (a [v1; v2; v3; v4]);
    make "v1 | (v2 | v3) | v4" (a [v1; v2; v3; v4]);
    make "{v1}" (r n v1);
    make "(:v1:)" (r n v1);
    make "5 * v1" (r v1 n);
    make "[v1]" (a [n; v1]);
    make "(/v1/)" (a [n; v1]);
    make "" n;
    make "v1 - v2" (ex v1 v2);
    make "? foo bar baz ?" (sp "foo bar baz");
  ]
end

module PythonEbnfUnitTests = struct
  open Tst

  let make s expected =
    s >:: (fun _ ->
      check_poly ~to_string:Grammar.to_string Grammar.(grammar [rule "r" expected]) (parse_string ~syntax:Syntax.PythonEbnf (Printf.sprintf "r: %s" s))
    )

  let g = Grammar.grammar
  let nt = Grammar.non_terminal
  let t = Grammar.token
  let s = Grammar.sequence
  let a = Grammar.alternative
  let r = Grammar.repetition
  let ru = Grammar.rule
  let n = Grammar.null
  let sp = Grammar.special
  let ex = Grammar.except

  let test = "PythonEbnf" >::: [
    make "FOO" (t "FOO");
    make "FOO # bar baz\n" (t "FOO");
    make "foo" (nt "foo");
    make "FOO | BAR" (a [t "FOO"; t "BAR"]);
    make "FOO BAR" (s [t "FOO"; t "BAR"]);
    make "FOO BAR | BAZ BIM" (a [s [t "FOO"; t "BAR"]; s [t "BAZ"; t "BIM"]]);
    make "FOO (BAR | BAZ) BIM" (s [t "FOO"; a [t "BAR"; t "BAZ"]; t "BIM"]);
    make "[FOO]" (a [n; t "FOO"]);
    make "FOO*" (r n (t "FOO"));
    make "FOO+" (r (t "FOO") n);
    "several rules" >:: (fun _ ->
      check_poly ~to_string:Grammar.to_string
        (g [ru "a" (t "FOO"); ru "b"(t "BAR")])
        (parse_string ~syntax:Syntax.PythonEbnf "a: FOO\nb: BAR\n")
    )
  ]
end

module UnitTests = struct
  open Tst

  let test = "Parse" >::: [
    IsoEbnfUnitTests.test;
    PythonEbnfUnitTests.test;
  ]
end
