
(* This file was auto-generated based on "src/OCamlETexEbnf_Messages.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "A rule (of form 'name:') is expected. (ocaml-etex-ebnf 16)\n"
    | 15 ->
        "A definition is expected after '...'. (ocaml-etex-ebnf 15)\n"
    | 19 ->
        "Something else (e.g. another rule, '|', etc.) is expected after a range. (ocaml-etex-ebnf 14)\n"
    | 20 ->
        "A definition is expected after '|'. (ocaml-etex-ebnf 13)\n"
    | 1 ->
        "A definition is expected. (ocaml-etex-ebnf 12)\n"
    | 2 ->
        "'|' is expected after '...'. (ocaml-etex-ebnf 11)\n"
    | 4 ->
        "A definition is expected after '... |'. (ocaml-etex-ebnf 10)\n"
    | 31 ->
        "A rule (or end of file) is expected. (ocaml-etex-ebnf 9)\n"
    | 28 ->
        "'{' not closed. (ocaml-etex-ebnf 8)\n"
    | 7 ->
        "A definition is expected after '{'. (ocaml-etex-ebnf 7)\n"
    | 26 ->
        "'{{' not closed. (ocaml-etex-ebnf 6)\n"
    | 8 ->
        "A definition is expected after '{{'. (ocaml-etex-ebnf 5)\n"
    | 24 ->
        "']' not closed. (ocaml-etex-ebnf 4)\n"
    | 9 ->
        "A definition is expected after '['. (ocaml-etex-ebnf 3)\n"
    | 22 ->
        "'(' not closed. (ocaml-etex-ebnf 2)\n"
    | 10 ->
        "A definition is expected after '('. (ocaml-etex-ebnf 1)\n"
    | _ ->
        raise Not_found
