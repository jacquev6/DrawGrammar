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

module Definition = struct
  type t =
    | Terminal of Terminal.t
    | NonTerminal of NonTerminal.t
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
