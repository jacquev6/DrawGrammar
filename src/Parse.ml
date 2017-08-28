open General.Abbr

module Lexing = OCamlStandard.Lexing

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
      | _ -> Frmt.apply "file %S, " pos_fname
    in
    Frmt.apply "%sline %n, character %n" file pos_lnum (pos_cnum - pos_bol + 1)

  let lexing position message =
    Exn.raise (Lexing (Frmt.apply "%s: lexing error: %s" (position_to_string position) message))

  let parsing position message =
    Exn.raise (Parsing (Frmt.apply "%s: parsing error: %s" (position_to_string position) message))
end

module Make(Parser: sig
  module MenhirInterpreter: sig
    include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
  end

  module Incremental: sig
    val grammar: Lexing.position -> Grammar.t MenhirInterpreter.checkpoint
  end
end)(Messages: sig
  val message: int -> string
end)(Lexer: sig
  val token: Lexing.lexbuf -> Parser.MenhirInterpreter.token
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
            |> Str.drop_suffix ~suf:"\n"
            |> Errors.parsing (Lexing.lexeme_start_p lexbuf)
          | _ -> Errors.parsing (Lexing.lexeme_start_p lexbuf) "unknown"
        )
        (Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.token lexbuf)
        (Parser.Incremental.grammar lexbuf.Lexing.lex_curr_p)
    with
      | Lexer.Error message -> Errors.lexing (Lexing.lexeme_start_p lexbuf) message

  let parse_chan ?file_name chan =
    chan
    |> Lexing.from_channel
    |> parse_lexbuf ?file_name

  let parse_file file_name =
    InFile.with_channel file_name ~f:(parse_chan ~file_name)

  let parse_string ?file_name code =
    code
    |> Lexing.from_string
    |> parse_lexbuf ?file_name
end

module IsoEbnf = Make(IsoEbnf_Parser)(IsoEbnf_Messages)(IsoEbnf_Lexer)

module PythonEbnf = Make(PythonEbnf_Parser)(PythonEbnf_Messages)(PythonEbnf_Lexer)

module OCamlETexEbnf = Make(OCamlETexEbnf_Parser)(OCamlETexEbnf_Messages)(OCamlETexEbnf_Lexer)

(* @todo Parse Menhir/ocamlyacc's .mly files *)

module Syntax = struct
  type t =
    | IsoEbnf
    | PythonEbnf
    | OCamlETexEbnf

  let all = [
    IsoEbnf;
    PythonEbnf;
    OCamlETexEbnf;
  ]

  let to_string = function
    | IsoEbnf -> "iso-ebnf"
    | PythonEbnf -> "python-ebnf"
    | OCamlETexEbnf -> "ocaml-etex-ebnf"

  let description = function
    | IsoEbnf -> "ISO-14977 EBNF"
    | PythonEbnf -> "syntax used in Python grammar file"
    | OCamlETexEbnf -> "syntax used in OCaml manual .etex sources"

  let online_reference = function
    | IsoEbnf -> "http://www.cl.cam.ac.uk/~mgk25/iso-14977.pdf"
    | PythonEbnf -> "https://github.com/python/cpython/blob/master/Grammar/Grammar"
    | OCamlETexEbnf -> "https://github.com/ocaml/ocaml/tree/trunk/manual/manual/refman"

  let of_string = function
    | "iso-ebnf" -> IsoEbnf
    | "python-ebnf" -> PythonEbnf
    | "ocaml-etex-ebnf" -> OCamlETexEbnf
    | syntax -> Exn.failure "Unknown grammar syntax %s" syntax
end

let parse_string ~syntax s =
  match syntax with
    | Syntax.IsoEbnf -> IsoEbnf.parse_string s
    | Syntax.PythonEbnf -> PythonEbnf.parse_string s
    | Syntax.OCamlETexEbnf -> OCamlETexEbnf.parse_string s

let parse_file ?syntax name =
  let syntax =
    match syntax with
      | Some syntax -> syntax
      | None ->
        name
        |> Str.split ~sep:"."
        |> Li.reverse
        |> Li.head
        |> Syntax.of_string
  in
  match syntax with
    | Syntax.IsoEbnf -> IsoEbnf.parse_file name
    | Syntax.PythonEbnf -> PythonEbnf.parse_file name
    | Syntax.OCamlETexEbnf -> OCamlETexEbnf.parse_file name

let check_grammar =
  General.Testing.check_poly ~repr:Grammar.to_string

module IsoEbnfUnitTests = struct
  open Tst

  let base_success s expected =
    s >: (lazy (check_grammar ~expected (parse_string ~syntax:Syntax.IsoEbnf s)))

  let success s expected =
    base_success (Frmt.apply "r = %s;" s) Grammar.(grammar [rule "r" expected])

  let fail_lexing s message =
    s >: (lazy (
      expect_exception
        ~expected:(Errors.Lexing message)
        (lazy (parse_string ~syntax:Syntax.IsoEbnf s))
    ))

  let fail_parsing s message =
    s >: (lazy (
      expect_exception
        ~expected:(Errors.Parsing message)
        (lazy (parse_string ~syntax:Syntax.IsoEbnf s))
    ))

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

  let test = "IsoEbnf" >:: [
    success "'t'" t;
    success "'t' (* foobar *)" t;
    success "\"t\"" t;
    success "v1" v1;
    success "v1, v2, v3, v4" (s [v1; v2; v3; v4]);
    success "v1, (v2, v3), v4" (s [v1; v2; v3; v4]);
    success "v1 | v2 ! v3 / v4" (a [v1; v2; v3; v4]);
    success "v1 | (v2 | v3) | v4" (a [v1; v2; v3; v4]);
    success "{v1}" (r n v1);
    success "(:v1:)" (r n v1);
    success "5 * v1" (r v1 n);
    success "[v1]" (a [n; v1]);
    success "(/v1/)" (a [n; v1]);
    success "" n;
    success "v1 - v2" (ex v1 v2);
    success "? foo bar baz ?" (sp "foo bar baz");

    base_success "ABCDEFGHIJKLMNOPQRSTUVWXYZ = foo;" Grammar.(grammar [rule "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (non_terminal "foo")]);
    success "ABCDEFGHIJKLMNOPQRSTUVWXYZ" Grammar.(non_terminal "ABCDEFGHIJKLMNOPQRSTUVWXYZ");

    base_success "abcdefghijklmnopqrstuvwxyz = foo;" Grammar.(grammar [rule "abcdefghijklmnopqrstuvwxyz" (non_terminal "foo")]);
    success "abcdefghijklmnopqrstuvwxyz" Grammar.(non_terminal "abcdefghijklmnopqrstuvwxyz");

    base_success "_0123456789 = foo;" Grammar.(grammar [rule "_0123456789" (non_terminal "foo")]);
    success "_0123456789" Grammar.(non_terminal "_0123456789");

    base_success "_with_underscores_ = foo;" Grammar.(grammar [rule "_with_underscores_" (non_terminal "foo")]);
    success "_with_underscores_" Grammar.(non_terminal "_with_underscores_");

    base_success "\\-with\\-dashes\\- = foo;" Grammar.(grammar [rule "-with-dashes-" (non_terminal "foo")]);
    success "\\-with\\-dashes\\-" Grammar.(non_terminal "-with-dashes-");

    base_success "\\ with\\ spaces\\  = foo;" Grammar.(grammar [rule " with spaces " (non_terminal "foo")]);
    success "\\ with\\ spaces\\ " Grammar.(non_terminal " with spaces ");

    fail_lexing "#" "line 1, character 1: lexing error: unexpected character '#'";
    fail_lexing "(*" "line 1, character 3: lexing error: unexpected end of file in comment";
    fail_lexing "'" "line 1, character 1: lexing error: unexpected end of file in string";
    fail_lexing "\"" "line 1, character 1: lexing error: unexpected end of file in string";
    fail_lexing "?" "line 1, character 1: lexing error: unexpected end of file in special sequence";

    fail_parsing "a = , *" "line 1, character 7: parsing error: A definition is expected after ','. (iso-ebnf 1)";
    fail_parsing "a = | *" "line 1, character 7: parsing error: A definition is expected after '|'. (iso-ebnf 2)";
    fail_parsing "a = - b *" "line 1, character 9: parsing error: A ';' or a ',' is expected after a definition. (iso-ebnf 3)";
    fail_parsing "a = -" "line 1, character 6: parsing error: A definition is expected after '-'. (iso-ebnf 4)";
    fail_parsing "a = - *" "line 1, character 7: parsing error: A definition is expected after '-'. (iso-ebnf 4)";
    fail_parsing "a = 1 * =" "line 1, character 9: parsing error: A definition is expected after '*'. (iso-ebnf 5)";
    fail_parsing "a = 1;" "line 1, character 6: parsing error: A '*' is expected after an integer. (iso-ebnf 6)";
    fail_parsing "a = *" "line 1, character 5: parsing error: A definition is expected after '='. (iso-ebnf 7)";
    fail_parsing "a = ( b ;" "line 1, character 9: parsing error: '(' not closed. (iso-ebnf 8)";
    fail_parsing "a = ( ;" "line 1, character 7: parsing error: A definition is expected after '('. (iso-ebnf 9)";
    fail_parsing "a = [ b ;" "line 1, character 9: parsing error: '[' not closed. (iso-ebnf 10)";
    fail_parsing "a = [ ;" "line 1, character 7: parsing error: A definition is expected after '['. (iso-ebnf 11)";
    fail_parsing "a = { b ;" "line 1, character 9: parsing error: '{' not closed. (iso-ebnf 12)";
    fail_parsing "a = { ;" "line 1, character 7: parsing error: A definition is expected after '{'. (iso-ebnf 13)";
    fail_parsing "a = b } ;" "line 1, character 7: parsing error: ';' is expected after a definition. (iso-ebnf 14)";
    fail_parsing "a = b *" "line 1, character 7: parsing error: ';' or '-' is expected after a definition. (iso-ebnf 15)";
    fail_parsing "a = ; ;" "line 1, character 7: parsing error: Another rule (or end of file) is expected after a rule. (iso-ebnf 16)";
    fail_parsing "a ;" "line 1, character 3: parsing error: '=' is expected after rule name. (iso-ebnf 17)";
    fail_parsing ";" "line 1, character 1: parsing error: A rule name is expected. (iso-ebnf 18)";
  ]
end

module PythonEbnfUnitTests = struct
  open Tst

  let base_success s expected =
    s >: (lazy (check_grammar ~expected (parse_string ~syntax:Syntax.PythonEbnf s)))

  let success s expected =
    base_success
      (Frmt.apply "r: %s" s)
      Grammar.(grammar [rule "r" expected])

  let fail_lexing s message =
    s >: (lazy (
      expect_exception
        ~expected:(Errors.Lexing message)
        (lazy (parse_string ~syntax:Syntax.PythonEbnf s))
    ))

  let fail_parsing s message =
    s >: (lazy (
      expect_exception
        ~expected:(Errors.Parsing message)
        (lazy (parse_string ~syntax:Syntax.PythonEbnf s))
    ))

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

  let test = "PythonEbnf" >:: [
    success "FOO" (t "FOO");
    success "FOO # bar baz\n" (t "FOO");
    success "foo" (nt "foo");
    success "FOO | BAR" (a [t "FOO"; t "BAR"]);
    success "FOO BAR" (s [t "FOO"; t "BAR"]);
    success "FOO BAR | BAZ BIM" (a [s [t "FOO"; t "BAR"]; s [t "BAZ"; t "BIM"]]);
    success "FOO (BAR | BAZ) BIM" (s [t "FOO"; a [t "BAR"; t "BAZ"]; t "BIM"]);
    success "[FOO]" (a [n; t "FOO"]);
    success "FOO*" (r n (t "FOO"));
    success "FOO+" (r (t "FOO") n);
    "several rules" >: (lazy (
      check_grammar
        ~expected:(g [ru "a" (t "FOO"); ru "b"(t "BAR")])
        (parse_string ~syntax:Syntax.PythonEbnf "a: FOO\nb: BAR\n")
    ));

    (* No upper-case letters in non-terminals because they are used for tokens *)
    (* base_success "ABCDEFGHIJKLMNOPQRSTUVWXYZ: foo" Grammar.(grammar [rule "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (non_terminal "foo")]); *)
    (* success "ABCDEFGHIJKLMNOPQRSTUVWXYZ" Grammar.(non_terminal "ABCDEFGHIJKLMNOPQRSTUVWXYZ"); *)

    base_success "abcdefghijklmnopqrstuvwxyz: foo" Grammar.(grammar [rule "abcdefghijklmnopqrstuvwxyz" (non_terminal "foo")]);
    success "abcdefghijklmnopqrstuvwxyz" Grammar.(non_terminal "abcdefghijklmnopqrstuvwxyz");

    base_success "_0123456789: foo" Grammar.(grammar [rule "_0123456789" (non_terminal "foo")]);
    success "_0123456789" Grammar.(non_terminal "_0123456789");

    base_success "_with_underscores_: foo" Grammar.(grammar [rule "_with_underscores_" (non_terminal "foo")]);
    success "_with_underscores_" Grammar.(non_terminal "_with_underscores_");

    base_success "-with-dashes-: foo" Grammar.(grammar [rule "-with-dashes-" (non_terminal "foo")]);
    success "-with-dashes-" Grammar.(non_terminal "-with-dashes-");

    base_success "\\-with\\-dashes\\-: foo" Grammar.(grammar [rule "-with-dashes-" (non_terminal "foo")]);
    success "\\-with\\-dashes\\-" Grammar.(non_terminal "-with-dashes-");

    base_success "\\ with\\ spaces\\ : foo" Grammar.(grammar [rule " with spaces " (non_terminal "foo")]);
    success "\\ with\\ spaces\\ " Grammar.(non_terminal " with spaces ");

    fail_lexing "{" "line 1, character 1: lexing error: unexpected character '{'";
    fail_lexing "'" "line 1, character 1: lexing error: unexpected end of file in literal terminal";

    fail_parsing "a: )" "line 1, character 4: parsing error: We are working on better error messages. (python-ebnf 1)";
  ]
end

module OCamlETexEbnfUnitTests = struct
  open Tst

  let base_success s expected =
    s >: (lazy (check_grammar ~expected (parse_string ~syntax:Syntax.OCamlETexEbnf s)))

  let success s expected =
    base_success
      (Frmt.apply "{lkqjsd|\\begin{syntax}r: %s\\end{syntax}x{{xx\\begin{syntax}s: 't'\\end{syntax}flkdjf" s)
      Grammar.(grammar [rule "r" expected; rule "s" (terminal "t")])

  let fail_parsing s message =
    s >: (lazy (
      let s = Frmt.apply "{lkqjsd|\\begin{syntax}%s\\end{syntax}" s in
      expect_exception
        ~expected:(Errors.Parsing message)
        (lazy (parse_string ~syntax:Syntax.OCamlETexEbnf s))
    ))

  let g = Grammar.grammar
  let nt = Grammar.non_terminal
  let t = Grammar.terminal
  let s = Grammar.sequence
  let a = Grammar.alternative
  let r = Grammar.repetition
  let ra = Grammar.range
  let ru = Grammar.rule
  let n = Grammar.null
  let sp = Grammar.special
  let ex = Grammar.except

  let test = "OCamlETexEbnf" >:: [
    success "foo" (nt "foo");
    success "\"bar\"" (t "bar");
    success "foo | bar" (a [nt "foo"; nt "bar"]);
    success "foo || bar" (a [nt "foo"; nt "bar"]);
    success "foo \\ldots bar" (ra (nt "foo") (nt "bar"));
    success "foo bar" (s [nt "foo"; nt "bar"]);

    base_success "\\begin{syntax}ABCDEFGHIJKLMNOPQRSTUVWXYZ: foo\\end{syntax}" Grammar.(grammar [rule "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (non_terminal "foo")]);
    success "ABCDEFGHIJKLMNOPQRSTUVWXYZ" Grammar.(non_terminal "ABCDEFGHIJKLMNOPQRSTUVWXYZ");

    base_success "\\begin{syntax}abcdefghijklmnopqrstuvwxyz: foo\\end{syntax}" Grammar.(grammar [rule "abcdefghijklmnopqrstuvwxyz" (non_terminal "foo")]);
    success "abcdefghijklmnopqrstuvwxyz" Grammar.(non_terminal "abcdefghijklmnopqrstuvwxyz");

    base_success "\\begin{syntax}_0123456789: foo\\end{syntax}" Grammar.(grammar [rule "_0123456789" (non_terminal "foo")]);
    success "_0123456789" Grammar.(non_terminal "_0123456789");

    base_success "\\begin{syntax}_with_underscores_: foo\\end{syntax}" Grammar.(grammar [rule "_with_underscores_" (non_terminal "foo")]);
    success "_with_underscores_" Grammar.(non_terminal "_with_underscores_");

    base_success "\\begin{syntax}-with-dashes-: foo\\end{syntax}" Grammar.(grammar [rule "-with-dashes-" (non_terminal "foo")]);
    success "-with-dashes-" Grammar.(non_terminal "-with-dashes-");

    (* This is probably not be LaTeX-compliant. We add it anyway, and we'll add the actual LaTeX syntax when it's identified. *)
    base_success "\\begin{syntax}\\-with\\-dashes\\-: foo\\end{syntax}" Grammar.(grammar [rule "-with-dashes-" (non_terminal "foo")]);
    success "\\-with\\-dashes\\-" Grammar.(non_terminal "-with-dashes-");

    base_success "\\begin{syntax}\\ with\\ spaces\\ : foo\\end{syntax}" Grammar.(grammar [rule " with spaces " (non_terminal "foo")]);
    success "\\ with\\ spaces\\ " Grammar.(non_terminal " with spaces ");

    fail_parsing "a: (;" "line 1, character 40: parsing error: A definition is expected after '('. (ocaml-etex-ebnf 1)";
    fail_parsing "a: (b;" "line 1, character 41: parsing error: '(' not closed. (ocaml-etex-ebnf 2)";
    fail_parsing "a: [;" "line 1, character 40: parsing error: A definition is expected after '['. (ocaml-etex-ebnf 3)";
    fail_parsing "a: [b;" "line 1, character 41: parsing error: ']' not closed. (ocaml-etex-ebnf 4)";
    fail_parsing "a: {{;" "line 1, character 41: parsing error: A definition is expected after '{{'. (ocaml-etex-ebnf 5)";
    fail_parsing "a: {{b;" "line 1, character 42: parsing error: '{{' not closed. (ocaml-etex-ebnf 6)";
    fail_parsing "a: {;" "line 1, character 40: parsing error: A definition is expected after '{'. (ocaml-etex-ebnf 7)";
    fail_parsing "a: {b;" "line 1, character 41: parsing error: '{' not closed. (ocaml-etex-ebnf 8)";
    fail_parsing "a: b}" "line 1, character 27: parsing error: A rule (or end of file) is expected. (ocaml-etex-ebnf 9)";
    fail_parsing "a: b)" "line 1, character 27: parsing error: A rule (or end of file) is expected. (ocaml-etex-ebnf 9)";
    fail_parsing "a: \"b\")" "line 1, character 29: parsing error: A rule (or end of file) is expected. (ocaml-etex-ebnf 9)";
    fail_parsing "a: ... |;" "line 1, character 44: parsing error: A definition is expected after '... |'. (ocaml-etex-ebnf 10)";
    fail_parsing "a: ...;" "line 1, character 42: parsing error: '|' is expected after '...'. (ocaml-etex-ebnf 11)";
    fail_parsing "a:" "line 1, character 37: parsing error: A definition is expected. (ocaml-etex-ebnf 12)";
    fail_parsing "a: b |;" "line 1, character 42: parsing error: A definition is expected after '|'. (ocaml-etex-ebnf 13)";
    fail_parsing "a: b ... c ..." "line 1, character 34: parsing error: Something else (e.g. another rule, '|', etc.) is expected after a range. (ocaml-etex-ebnf 14)";
    fail_parsing "a: b ...;" "line 1, character 44: parsing error: A definition is expected after '...'. (ocaml-etex-ebnf 15)";
    fail_parsing "a" "line 1, character 23: parsing error: A rule (of form 'name:') is expected. (ocaml-etex-ebnf 16)";
  ]
end

module UnitTests = struct
  open Tst

  let test = "Parse" >:: [
    IsoEbnfUnitTests.test;
    PythonEbnfUnitTests.test;
    OCamlETexEbnfUnitTests.test;
  ]
end
