open General.Abbr

module Terminal = struct
  type t = {
    value: string;
  }
end

module Definition = struct
  type t =
    | Terminal of Terminal.t
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
      {Rule.name="other terminal"; definition=Definition.Terminal {Terminal.value="other value"}};
    ];
  }
