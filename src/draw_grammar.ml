open General.Abbr

let draw_grammar_on_canvas syntax grammar canvas =
  let canvas = Js.Opt.get (Dom_html.CoerceTo.canvas canvas) (fun _ -> failwith "Not a canvas") in
  canvas##width <- 200;
  canvas##height <- 200;

  let context = canvas##getContext (Dom_html._2d_) in
  begin
    context##moveTo (0., 0.);
    context##lineTo (100., 100.);
    context##stroke ();
    context##fillText (syntax, 25., 25.);
    context##fillText (grammar, 25., 50.);
  end

let () = Js.Unsafe.global##draw_grammar_on_canvas_ (* why do I need a last undesrcore? *) <- Js.wrap_callback draw_grammar_on_canvas
