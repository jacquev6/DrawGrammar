
(* This file was auto-generated based on "src/IsoEbnf_Messages.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "A rule name is expected. (iso-ebnf 18)\n"
    | 1 ->
        "'=' is expected after rule name. (iso-ebnf 17)\n"
    | 38 ->
        "Another rule (or end of file) is expected after a rule. (iso-ebnf 16)\n"
    | 7 ->
        "';' or '-' is expected after a definition. (iso-ebnf 15)\n"
    | 36 ->
        "';' is expected after a definition. (iso-ebnf 14)\n"
    | 13 ->
        "A definition is expected after '{'. (iso-ebnf 13)\n"
    | 19 ->
        "'{' not closed. (iso-ebnf 12)\n"
    | 21 ->
        "A definition is expected after '['. (iso-ebnf 11)\n"
    | 22 ->
        "'[' not closed. (iso-ebnf 10)\n"
    | 24 ->
        "A definition is expected after '('. (iso-ebnf 9)\n"
    | 25 ->
        "'(' not closed. (iso-ebnf 8)\n"
    | 2 ->
        "A definition is expected after '='. (iso-ebnf 7)\n"
    | 3 ->
        "A '*' is expected after an integer. (iso-ebnf 6)\n"
    | 11 ->
        "A definition is expected after '*'. (iso-ebnf 5)\n"
    | 8 ->
        "A definition is expected after '-'. (iso-ebnf 4)\n"
    | 5 ->
        "A ';' or a ',' is expected after a definition. (iso-ebnf 3)\n"
    | 15 ->
        "A definition is expected after '|'. (iso-ebnf 2)\n"
    | 6 ->
        "A definition is expected after ','. (iso-ebnf 1)\n"
    | _ ->
        raise Not_found
