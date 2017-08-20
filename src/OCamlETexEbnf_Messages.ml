
(* This file was auto-generated based on "src/OCamlETexEbnf_Messages.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "We are working on better error messages. (ocaml-etex-ebnf 16)\n"
    | 15 ->
        "We are working on better error messages. (ocaml-etex-ebnf 15)\n"
    | 19 ->
        "We are working on better error messages. (ocaml-etex-ebnf 14)\n"
    | 20 ->
        "We are working on better error messages. (ocaml-etex-ebnf 13)\n"
    | 1 ->
        "We are working on better error messages. (ocaml-etex-ebnf 12)\n"
    | 2 ->
        "We are working on better error messages. (ocaml-etex-ebnf 11)\n"
    | 4 ->
        "We are working on better error messages. (ocaml-etex-ebnf 10)\n"
    | 31 ->
        "We are working on better error messages. (ocaml-etex-ebnf 9)\n"
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
