(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

let drawing_tests =
  DrawingTests.tests
  |> Li.map ~f:(fun (name, grammar) ->
    object%js (_)
      val name = Js.string name
      method draw_on_canvas_ canvas primary_settings secondary_settings =
        Draw_grammar_js.draw grammar canvas primary_settings secondary_settings
    end
  )
  |> Li.to_array

let () = Js.export "drawing_tests" drawing_tests
