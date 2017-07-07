open General.Abbr

module Make(C: module type of Cairo) = struct
  module Terminal = struct
    let draw {Grammar.Terminal.value=_} ~context:_ =
      ()
  end

  module Definition = struct
    let draw definition ~context =
      match definition with
        | Grammar.Definition.Terminal terminal ->
          Terminal.draw terminal ~context
  end

  module Rule = struct
    let draw {Grammar.Rule.name=_; definition} ~context =
      Definition.draw definition ~context
  end

  let measure _ ~context:_ =
    (507 / 3 - 10, 144 / 3 - 10)

  let draw {Grammar.name=_; rules} ~context =
    rules
    |> Li.iter ~f:(Rule.draw ~context)
end
