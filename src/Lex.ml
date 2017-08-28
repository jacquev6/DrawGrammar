open General.Abbr

let unescape s =
  (* @todo Use General.String.replace_all when it's implemented *)
  s
  |> Str.split ~sep:"\\ "
  |> StrLi.join ~sep:" "
  |> Str.split ~sep:"\\-"
  |> StrLi.join ~sep:"-"
