open General.Abbr

(* @todo Parse and draw comments *)

module Terminal = struct
  type t = {
    value: string;
  }

  let value {value} = value

  let to_string {value} =
    Frmt.apply "%S" value
end

module Token = struct
  type t = {
    name: string;
  }

  let name {name} = name

  let to_string {name} =
    name
end

module NonTerminal = struct
  type t = {
    name: string;
  }

  let name {name} = name

  let to_string {name} =
    name
end

module Special = struct
  type t = {
    value: string;
  }

  let value {value} = value

  let to_string {value} =
    Frmt.apply "Special(%S)" value
end

module rec Sequence: sig
  type t = {
    elements: Definition.t list;
  }
  val elements: t -> Definition.t list
  val to_string: t -> string
end = struct
  type t = {
    elements: Definition.t list;
  }

  let elements {elements} = elements

  let to_string {elements} =
    elements
    |> Li.map ~f:Definition.to_string
    |> StrLi.join ~sep:", "
    |> Frmt.apply "Sequence(%s)"
end

and Alternative: sig
  type t = {
    elements: Definition.t list;
  }
  val elements: t -> Definition.t list
  val to_string: t -> string
end = struct
  type t = {
    elements: Definition.t list;
  }

  let elements {elements} = elements

  let to_string {elements} =
    elements
    |> Li.map ~f:Definition.to_string
    |> StrLi.join ~sep:", "
    |> Frmt.apply "Alternative(%s)"
end

and Range: sig
  type t = {
    min: Definition.t;
    max: Definition.t;
  }
  val min: t -> Definition.t
  val max: t -> Definition.t
  val to_string: t -> string
end = struct
  type t = {
    min: Definition.t;
    max: Definition.t;
  }

  let min {min; _} = min

  let max {max; _} = max

  let to_string {min; max} =
    Frmt.apply "Range(%s, %s)" (Definition.to_string min) (Definition.to_string max)
end

and Repetition: sig
  (* @todo Add an int option for the number of repetitions *)
  type t = {
    forward: Definition.t;
    backward: Definition.t;
  }
  val forward: t -> Definition.t
  val backward: t -> Definition.t
  val to_string: t -> string
end = struct
  type t = {
    forward: Definition.t;
    backward: Definition.t;
  }

  let forward {forward; _} = forward

  let backward {backward; _} = backward

  let to_string {forward; backward} =
    Frmt.apply "Repetition(%s, %s)" (Definition.to_string forward) (Definition.to_string backward)
end

and Except: sig
  type t = {
    base: Definition.t;
    except: Definition.t;
  }
  val base: t -> Definition.t
  val except: t -> Definition.t
  val to_string: t -> string
end = struct
  type t = {
    base: Definition.t;
    except: Definition.t;
  }

  let base {base; _} = base

  let except {except; _} = except

  let to_string {base; except} =
    Frmt.apply "Except(%s, %s)" (Definition.to_string base) (Definition.to_string except)
end

and Definition: sig
  type t =
    | Null
    | Terminal of Terminal.t
    | Token of Token.t
    | NonTerminal of NonTerminal.t
    | Sequence of Sequence.t
    | Alternative of Alternative.t
    | Range of Range.t
    | Repetition of Repetition.t
    | Special of Special.t
    | Except of Except.t

  val to_string: t -> string
end = struct
  type t =
    | Null
    | Terminal of Terminal.t
    | Token of Token.t
    | NonTerminal of NonTerminal.t
    | Sequence of Sequence.t
    | Alternative of Alternative.t
    | Range of Range.t
    | Repetition of Repetition.t
    | Special of Special.t
    | Except of Except.t

  let to_string = function
    | Null -> "ε"
    | Terminal x -> Terminal.to_string x
    | Token x -> Token.to_string x
    | NonTerminal x -> NonTerminal.to_string x
    | Sequence x -> Sequence.to_string x
    | Alternative x -> Alternative.to_string x
    | Range x -> Range.to_string x
    | Repetition x -> Repetition.to_string x
    | Special x -> Special.to_string x
    | Except x -> Except.to_string x
end

module Rule = struct
  type t = {
    name: string;
    definition: Definition.t;
  }

  let name {name; _} = name

  let definition {definition; _} = definition

  let to_string {name; definition} =
    Frmt.apply "%s = %s;\n" name (Definition.to_string definition)
end

type t = {
  rules: Rule.t list;
}

let rules {rules} = rules

let to_string {rules} =
  rules
  |> Li.map ~f:Rule.to_string
  |> StrLi.join ~sep:"\n"

module Constructors = struct
  let null = Definition.Null

  let non_terminal name = Definition.NonTerminal {NonTerminal.name}

  let token name = Definition.Token {Token.name}

  let terminal value = Definition.Terminal {Terminal.value}


  let special value = Definition.Special {Special.value}

  let sequence elements =
    let elements =
      elements
      |> Li.flat_map ~f:(function
        | Definition.Sequence {Sequence.elements} -> elements
        | element -> [element]
      )
      |> Li.filter ~f:(fun x -> x <> Definition.Null)
    in
    match elements with
      | [] -> Definition.Null
      | [e] -> e
      | elements -> Definition.Sequence {Sequence.elements}

  let alternative elements =
    let elements =
      elements
      |> Li.flat_map ~f:(function
        | Definition.Alternative {Alternative.elements} -> elements
        | element -> [element]
      )
    in
    let has_null = Li.Poly.contains elements Definition.Null
    and elements = Li.filter ~f:(fun x -> x <> Definition.Null) elements in
    let elements = if has_null then Definition.Null::elements else elements in
    match elements with
      | [] -> Exn.failure "Empty alternative"
      | [element] -> element
      | _ -> Definition.Alternative {Alternative.elements}

  let range min max = Definition.Range {Range.min; max}

  let repetition forward backward = Definition.Repetition {Repetition.forward; backward}

  let except base except = Definition.Except {Except.base; except}

  let rule name definition = {Rule.name; definition}

  let grammar rules = {rules}
end

module Raw = struct
  open Constructors

  let n = null
  let nt = non_terminal
  let t = terminal
  (* let tr = terminal_range *)
  (* let sp = special *)
  let seq elements = Definition.Sequence {Sequence.elements}
  let alt elements = Definition.Alternative {Alternative.elements}
  let rep = repetition
  (* let ex = except *)
  let r = rule
  let g = grammar
end

module ConstructorsUnitTests = struct
  open Tst

  let make expected actual =
    (Definition.to_string actual) >: (lazy (
      check_poly ~repr:to_string ~expected:Raw.(g [r "r" expected]) Constructors.(grammar [rule "r" actual])
    ))

  let test = "constructors" >:: [
    make Raw.n Constructors.null;
    make Raw.(t "t") Constructors.(terminal "t");
    make Raw.(nt "nt") Constructors.(non_terminal "nt");
    make Raw.(t "t1") Constructors.(sequence [terminal "t1"]);
    make Raw.(t "t1") Constructors.(sequence [sequence [terminal "t1"]]);
    make Raw.(t "t1") Constructors.(sequence [sequence [sequence [terminal "t1"]]]);
    make Raw.(seq [t "t1"; t "t2"]) Constructors.(sequence [sequence [terminal "t1"; terminal "t2"]]);
    make Raw.(seq [t "t1"; t "t2"; t "t3"]) Constructors.(sequence [terminal "t1"; sequence [terminal "t2"; terminal "t3"]]);
    make Raw.(seq [t "t1"; t "t2"; t "t3"]) Constructors.(sequence [terminal "t1"; sequence [sequence [terminal "t2"; terminal "t3"]]]);
    make Raw.(seq [t "t1"; t "t2"; t "t3"]) Constructors.(sequence [terminal "t1"; sequence [sequence [sequence [terminal "t2"; terminal "t3"]]]]);
    make Raw.(t "t1") Constructors.(alternative [terminal "t1"]);
    make Raw.(t "t1") Constructors.(alternative [alternative [terminal "t1"]]);
    make Raw.(t "t1") Constructors.(alternative [alternative [alternative [terminal "t1"]]]);
    make Raw.(alt [t "t1"; t "t2"]) Constructors.(alternative [alternative [terminal "t1"; terminal "t2"]]);
    make Raw.(alt [t "t1"; t "t2"; t "t3"]) Constructors.(alternative [terminal "t1"; alternative [terminal "t2"; terminal "t3"]]);
    make Raw.(alt [t "t1"; t "t2"; t "t3"]) Constructors.(alternative [terminal "t1"; alternative [alternative [terminal "t2"; terminal "t3"]]]);
    make Raw.(alt [t "t1"; t "t2"; t "t3"]) Constructors.(alternative [terminal "t1"; alternative [alternative [alternative [terminal "t2"; terminal "t3"]]]]);
    make Raw.(seq [t "t1"; t "t2"]) Constructors.(sequence [terminal "t1"; null; terminal "t2"]);
    make Raw.(alt [n; t "t1"; t "t2"]) Constructors.(alternative [terminal "t1"; null; terminal "t2"]);
  ]
end

include Constructors

let simplify =
  (* @todo Factorize common pre/suffixes in alternatives? We have an example with the "=" in type-representation in OCaml's "Type definitions" *)
  (* More generaly common parts before railtracks join or after railtrack splits could be merged. *)
  let common_prefix =
    let rec aux rev_prefix xs ys =
      match (xs, ys) with
        | (x::xs, y::ys) when x = y -> aux (x::rev_prefix) xs ys
        | _ -> (Li.reverse rev_prefix, xs, ys)
    in
    aux []
  in
  let process_sequence_elements =
    let rec aux rev_before = function
      | [] -> Li.reverse rev_before
      | current::after -> begin
        match current with 
          | Definition.Repetition {Repetition.forward; backward} ->
            let backward = match backward with
              | Definition.Sequence {Sequence.elements} -> elements
              | _ -> [backward]
            in
            let (prefix, backward, after) = common_prefix backward after in
            let (rev_suffix, rev_backward, rev_before) = common_prefix (Li.reverse backward) rev_before in
            let backward = Li.reverse rev_backward
            and suffix = Li.reverse rev_suffix in
            let current = repetition (sequence [sequence suffix; forward; sequence prefix]) (sequence backward) in
            aux (current::rev_before) after
          | _ ->
            aux (current::rev_before) after
      end
    in
    aux []
  in
  let rec aux = function
    | (Definition.Null | Definition.NonTerminal _ | Definition.Token _ | Definition.Terminal _ | Definition.Special _) as x -> x
    | Definition.Alternative {Alternative.elements} ->
      let elements = Li.map ~f:aux elements in
      Definition.Alternative {Alternative.elements}
    | Definition.Range {Range.min; max} ->
      let min = aux min and max = aux max in
      Definition.Range {Range.min; max}
    | Definition.Sequence {Sequence.elements} ->
      let elements =
        elements
        |> Li.map ~f:aux
        |> process_sequence_elements
      in
      sequence elements
    | Definition.Repetition {Repetition.forward; backward} ->
      let forward = aux forward
      and backward = aux backward in
      Definition.Repetition {Repetition.forward; backward}
    | Definition.Except {Except.base; except} ->
      let base = aux base
      and except = aux except in
      Definition.Except {Except.base; except}
  in
  function {rules} ->
    let rules =
      rules
      |> Li.map ~f:(fun {Rule.name; definition} ->
        let definition = aux definition in
        {Rule.name; definition}
      )
    in
    {rules}

module SimplifyUnitTests = struct
  open Tst

  open Raw

  let f1 = t "f1"
  let f2 = t "f2"
  let f3 = t "f3"
  let p1 = t "p1"
  let p2 = t "p2"
  let p3 = t "p3"
  let s1 = t "s1"
  let s2 = t "s2"
  let s3 = t "s3"
  let x1 = t "x1"
  let x2 = t "x2"
  let x3 = t "x3"
  let x4 = t "x4"
  let x5 = t "x5"
  let x6 = t "x6"
  let x7 = t "x7"
  let x8 = t "x8"
  let x9 = t "x9"

  let make expected definition =
    (Definition.to_string definition) >: (lazy (
      check_poly ~repr:to_string ~expected:(g [r "r" expected]) (simplify (g [r "r" definition]))
    ))

  let test = "simplify" >:: [
    make
      (seq [x1; x2; x3; rep (seq [p1; p2; p3; f1; f2; f3; s1; s2; s3]) (seq [x4; x5; x6]); x7; x8; x9])
      (seq [x1; x2; x3; p1; p2; p3; rep (seq [f1; f2; f3]) (seq [s1; s2; s3; x4; x5; x6; p1; p2; p3]); s1; s2; s3; x7; x8; x9])
    ;
    make
      (seq [x1; x2; x3; rep s1 n; x7; x8; x9])
      (seq [x1; x2; x3; rep n s1; s1; x7; x8; x9])
    ;
    make
      (seq [x1; x2; x3; rep p1 n; x7; x8; x9])
      (seq [x1; x2; x3; p1; rep n p1; x7; x8; x9])
    ;
  ]
end

module UnitTests = struct
  open Tst

  let test = "Grammar" >:: [
    ConstructorsUnitTests.test;
    SimplifyUnitTests.test;
  ]
end
