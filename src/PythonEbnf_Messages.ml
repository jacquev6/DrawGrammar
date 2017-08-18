
(* This file was auto-generated based on "src/PythonEbnf_Messages.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "(PythonEbnf 9)\n"
    | 13 ->
        "(PythonEbnf 8)\n"
    | 23 ->
        "(PythonEbnf 7)\n"
    | 11 ->
        "(PythonEbnf 6)\n"
    | 20 ->
        "(PythonEbnf 5)\n"
    | 4 ->
        "(PythonEbnf 4)\n"
    | 17 ->
        "(PythonEbnf 3)\n"
    | 5 ->
        "(PythonEbnf 2)\n"
    | 1 ->
        "(PythonEbnf 1)\n"
    | _ ->
        raise Not_found
