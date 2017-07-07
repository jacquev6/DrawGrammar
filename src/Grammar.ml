open General.Abbr

type t = string * string

let create ~syntax ~grammar =
  (syntax, grammar)

module Drawing(C: Context.S) = struct
  let measure (_syntax, grammar) ~context =
    let {C.TextExtents.advance} = C.measure_text context ~t:grammar in
    (Int.of_float (20. +. advance), 200)

  let draw (syntax, grammar) ~context =
    C.move_to context ~x:0. ~y:0.;
    C.line_to context ~x:100. ~y:100.;
    C.stroke context;
    C.show_text context ~x:10. ~y:10. ~t:syntax;
    C.show_text context ~x:20. ~y:20. ~t:grammar;
end
