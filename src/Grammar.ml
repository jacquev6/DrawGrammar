open General.Abbr

let sprintf = OCamlStandard.Printf.sprintf

module Terminal = struct
  type t = {
    value: string;
  }

  let to_string {value} =
    sprintf "'%s'" value
end

module NonTerminal = struct
  type t = {
    name: string;
  }

  let to_string {name} =
    name
end

module rec Sequence: sig
  type t = {
    elements: Definition.t list;
  }

  val to_string: t -> string
end = struct
  type t = {
    elements: Definition.t list;
  }

  let to_string {elements} =
    elements
    |> Li.map ~f:Definition.to_string
    |> StrLi.concat ~sep:", "
    |> sprintf "(%s)"
end

and Alternative: sig
  type t = {
    elements: Definition.t list;
  }

  val to_string: t -> string
end = struct
  type t = {
    elements: Definition.t list;
  }

  let to_string {elements} =
    elements
    |> Li.map ~f:Definition.to_string
    |> StrLi.concat ~sep:" | "
    |> sprintf "(%s)"
end

and Repetition: sig
  type t = {
    forward: Definition.t;
    backward: Definition.t;
  }

  val to_string: t -> string
end = struct
  type t = {
    forward: Definition.t;
    backward: Definition.t;
  }

  let to_string {forward; backward} =
    sprintf "Repetition(%s, %s)" (Definition.to_string forward) (Definition.to_string backward)
end

and Definition: sig
  type t =
    | Null
    | Terminal of Terminal.t
    | NonTerminal of NonTerminal.t
    | Sequence of Sequence.t
    | Alternative of Alternative.t
    | Repetition of Repetition.t

  val to_string: t -> string
end = struct
  type t =
    | Null
    | Terminal of Terminal.t
    | NonTerminal of NonTerminal.t
    | Sequence of Sequence.t
    | Alternative of Alternative.t
    | Repetition of Repetition.t

  let to_string = function
    | Null -> "ø"
    | Terminal x -> Terminal.to_string x
    | NonTerminal x -> NonTerminal.to_string x
    | Sequence x -> Sequence.to_string x
    | Alternative x -> Alternative.to_string x
    | Repetition x -> Repetition.to_string x
end

module Rule = struct
  type t = {
    name: string;
    definition: Definition.t;
  }

  let to_string {name; definition} =
    sprintf "%s = %s;\n" name (Definition.to_string definition)
end

type t = {
  rules: Rule.t list;
}

let parse ~syntax:_ _ =
  (* @todo Implement *)
  {
    rules=[
      {Rule.name="terminal"; definition=Definition.Terminal {Terminal.value="value"}};
      {Rule.name="non-terminal"; definition=Definition.NonTerminal {NonTerminal.name="name"}};
    ];
  }

let to_string {rules} =
  rules
  |> Li.map ~f:Rule.to_string
  |> StrLi.concat ~sep:"\n"

let non_terminal name = Definition.NonTerminal {NonTerminal.name}

let terminal value = Definition.Terminal {Terminal.value}

let sequence elements = Definition.Sequence {Sequence.elements}

let alternative elements = Definition.Alternative {Alternative.elements}

let repetition forward backward = Definition.Repetition {Repetition.forward; backward}

let rule name definition = {Rule.name; definition}

let grammar rules = {rules}
