open General.Abbr

module type S = sig
  type t

  (* Text *)
  module TextExtents: sig
    type t = {
      advance: float;
      (* ascend: float; *)
      (* descend: float; *)
    }
  end

  val measure_text: t -> t:string -> TextExtents.t
  val show_text: t -> x:float -> y:float -> t:string -> unit

  (* Lines *)
  val move_to: t -> x:float -> y:float -> unit
  val line_to: t -> x:float -> y:float -> unit
  val stroke: t -> unit
end
