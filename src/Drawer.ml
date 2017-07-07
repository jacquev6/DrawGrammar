open General.Abbr

module Make(C: Context.S) = struct
  module Terminal = struct
    let draw {Grammar.Terminal.value} ~context =
      C.show_text context ~x:20. ~y:20. ~t:value
  end

  module Definition = struct
    let draw definition ~context =
      match definition with
        | Grammar.Definition.Terminal terminal ->
          Terminal.draw terminal ~context
  end

  module Rule = struct
    let draw {Grammar.Rule.name; definition} ~context =
      C.show_text context ~x:10. ~y:10. ~t:name;
      Definition.draw definition ~context
  end

  let measure _ ~context:_ =
    (507 / 3 - 10, 144 / 3 - 10)

  let draw {Grammar.name=_; rules} ~context =
    rules
    |> Li.iter ~f:(Rule.draw ~context)
end
