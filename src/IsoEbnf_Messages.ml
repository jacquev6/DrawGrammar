
(* This file was auto-generated based on "src/IsoEbnf_Messages.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "(IsoEbnf 18)\n"
    | 1 ->
        "(IsoEbnf 17)\n"
    | 38 ->
        "(IsoEbnf 16)\n"
    | 7 ->
        "(IsoEbnf 15)\n"
    | 36 ->
        "(IsoEbnf 14)\n"
    | 13 ->
        "(IsoEbnf 13)\n"
    | 19 ->
        "(IsoEbnf 12)\n"
    | 21 ->
        "(IsoEbnf 11)\n"
    | 22 ->
        "(IsoEbnf 10)\n"
    | 24 ->
        "(IsoEbnf 9)\n"
    | 25 ->
        "(IsoEbnf 8)\n"
    | 2 ->
        "(IsoEbnf 7)\n"
    | 3 ->
        "(IsoEbnf 6)\n"
    | 11 ->
        "(IsoEbnf 5)\n"
    | 8 ->
        "(IsoEbnf 4)\n"
    | 5 ->
        "(IsoEbnf 3)\n"
    | 15 ->
        "(IsoEbnf 2)\n"
    | 6 ->
        "(IsoEbnf 1)\n"
    | _ ->
        raise Not_found
