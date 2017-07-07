open General.Abbr

type t = Cairo.context

(* Text *)
module TextExtents = struct
  type t = {
    advance: float;
    (* ascend: float; *)
    (* descend: float; *)
  }
end

let measure_text context ~t =
  let {Cairo.x_advance=advance; _} = Cairo.text_extents context t in
  {TextExtents.advance}

let show_text context ~x ~y ~t =
  Cairo.move_to context ~x ~y;
  Cairo.show_text context t

(* Lines *)
let move_to = Cairo.move_to

let line_to = Cairo.line_to

let stroke = Cairo.stroke
