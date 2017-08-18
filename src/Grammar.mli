open General.Abbr

module Terminal: sig
  type t
  val value: t -> string
end

module Token: sig
  type t
  val name: t -> string
end

module NonTerminal: sig
  type t
  val name: t -> string
end

module Special: sig
  type t
  val value: t -> string
end

module rec Sequence: sig
  type t
  val elements: t-> Definition.t list
end

and Alternative: sig
  type t
  val elements: t -> Definition.t list
end

and Range: sig
  type t
  val min: t -> Definition.t
  val max: t -> Definition.t
end

and Repetition: sig
  type t
  val forward: t -> Definition.t
  val backward: t -> Definition.t
end

and Except: sig
  type t
  val base: t -> Definition.t
  val except: t -> Definition.t
end

and Definition: sig
  type t =
    | Null
    | Terminal of Terminal.t
    | Token of Token.t
    | NonTerminal of NonTerminal.t
    | Sequence of Sequence.t
    | Alternative of Alternative.t
    | Range of Range.t
    | Repetition of Repetition.t
    | Special of Special.t
    | Except of Except.t
end

module Rule: sig
  type t
  val name: t -> string
  val definition: t -> Definition.t
end

type t
val rules: t -> Rule.t list
val to_string: t -> string

val simplify: t -> t

val null: Definition.t
val non_terminal: string -> Definition.t
val terminal: string -> Definition.t
val token: string -> Definition.t
val special: string -> Definition.t
val sequence: Definition.t list -> Definition.t
val alternative: Definition.t list -> Definition.t
val range: Definition.t -> Definition.t -> Definition.t
val repetition: Definition.t -> Definition.t -> Definition.t
val except: Definition.t -> Definition.t -> Definition.t
val rule: string -> Definition.t -> Rule.t
val grammar: Rule.t list -> t

module UnitTests: sig
  val test: Tst.Test.t
end
