open General.Abbr

module Terminal = struct
  type t = {
    value: string;
  }
end

module NonTerminal = struct
  type t = {
    name: string;
  }
end

module rec Sequence: sig
  type t = {
    elements: Definition.t list;
  }
end = struct
  type t = {
    elements: Definition.t list;
  }
end

and Alternative: sig
  type t = {
    elements: Definition.t list;
  }
end = struct
  type t = {
    elements: Definition.t list;
  }
end

and Repetition: sig
  type t = {
    forward: Definition.t;
    backward: Definition.t;
  }
end = struct
  type t = {
    forward: Definition.t;
    backward: Definition.t;
  }
end

and Definition: sig
  type t =
    | Terminal of Terminal.t
    | NonTerminal of NonTerminal.t
    | Sequence of Sequence.t
    | Alternative of Alternative.t
    | Repetition of Repetition.t
end = struct
  type t =
    | Terminal of Terminal.t
    | NonTerminal of NonTerminal.t
    | Sequence of Sequence.t
    | Alternative of Alternative.t
    | Repetition of Repetition.t
end

module Rule = struct
  type t = {
    name: string;
    definition: Definition.t;
  }
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
