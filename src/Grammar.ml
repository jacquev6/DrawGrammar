open General.Abbr

let sprintf = OCamlStandard.Printf.sprintf

module Terminal = struct
  type t = {
    value: string;
  }

  let to_string {value} =
    sprintf "%S" value
end

module NonTerminal = struct
  type t = {
    name: string;
  }

  let to_string {name} =
    name
end

module Special = struct
  type t = {
    value: string;
  }

  let to_string {value} =
    sprintf "Special(%S)" value
end

module rec Sequence: sig
  type t = {
    elements: Definition.t list;
  }

  val to_string: t -> string
end = struct
  type t = {
    elements: Definition.t list;
  }

  let to_string {elements} =
    elements
    |> Li.map ~f:Definition.to_string
    |> StrLi.concat ~sep:", "
    |> sprintf "Sequence(%s)"
end

and Alternative: sig
  type t = {
    elements: Definition.t list;
  }

  val to_string: t -> string
end = struct
  type t = {
    elements: Definition.t list;
  }

  let to_string {elements} =
    elements
    |> Li.map ~f:Definition.to_string
    |> StrLi.concat ~sep:", "
    |> sprintf "Alternative(%s)"
end

and Repetition: sig
  (* @todo Add an int option for the number of repetitions *)
  type t = {
    forward: Definition.t;
    backward: Definition.t;
  }

  val to_string: t -> string
end = struct
  type t = {
    forward: Definition.t;
    backward: Definition.t;
  }

  let to_string {forward; backward} =
    sprintf "Repetition(%s, %s)" (Definition.to_string forward) (Definition.to_string backward)
end

and Except: sig
  type t = {
    base: Definition.t;
    except: Definition.t;
  }

  val to_string: t -> string
end = struct
  type t = {
    base: Definition.t;
    except: Definition.t;
  }

  let to_string {base; except} =
    sprintf "Except(%s, %s)" (Definition.to_string base) (Definition.to_string except)
end

and Definition: sig
  type t =
    | Null
    | Terminal of Terminal.t
    | NonTerminal of NonTerminal.t
    | Sequence of Sequence.t
    | Alternative of Alternative.t
    | Repetition of Repetition.t
    | Special of Special.t
    | Except of Except.t

  val to_string: t -> string
end = struct
  type t =
    | Null
    | Terminal of Terminal.t
    | NonTerminal of NonTerminal.t
    | Sequence of Sequence.t
    | Alternative of Alternative.t
    | Repetition of Repetition.t
    | Special of Special.t
    | Except of Except.t

  let to_string = function
    | Null -> "ε"
    | Terminal x -> Terminal.to_string x
    | NonTerminal x -> NonTerminal.to_string x
    | Sequence x -> Sequence.to_string x
    | Alternative x -> Alternative.to_string x
    | Repetition x -> Repetition.to_string x
    | Special x -> Special.to_string x
    | Except x -> Except.to_string x
end

module Rule = struct
  type t = {
    name: string;
    definition: Definition.t;
  }

  let to_string {name; definition} =
    sprintf "%s = %s;\n" name (Definition.to_string definition)
end

type t = {
  rules: Rule.t list;
}

let to_string {rules} =
  rules
  |> Li.map ~f:Rule.to_string
  |> StrLi.concat ~sep:"\n"

let null = Definition.Null

let non_terminal name = Definition.NonTerminal {NonTerminal.name}

let terminal value = Definition.Terminal {Terminal.value}

let special value = Definition.Special {Special.value}

let sequence elements = Definition.Sequence {Sequence.elements}

let alternative elements = Definition.Alternative {Alternative.elements}

let repetition forward backward = Definition.Repetition {Repetition.forward; backward}

let except base except = Definition.Except {Except.base; except}

let rule name definition = {Rule.name; definition}

let grammar rules = {rules}


let simplify =
  let rec simplify_def = Definition.(function
    | (Null | Terminal _ | NonTerminal _ | Special _) as x -> x
    | Sequence {Sequence.elements} ->
      (* @todo Remove Null *)
      (* @todo Merge "a, {a}" and "{a}, a" into Repetition(forward=a, backward=Null) *)
      let elements =
        elements
        |> Li.concat_map ~f:(fun element ->
          match simplify_def element with
            | Sequence {Sequence.elements} ->
              elements
            | element -> [element]
        )
      in begin
        match elements with
          | [] -> Null
          | [element] -> element
          | _ -> Sequence {Sequence.elements}
      end
    | Alternative {Alternative.elements} ->
      (* @todo Deduplicate elements *)
      (* @todo Put Null in front if present *)
      let elements =
        elements
        |> Li.concat_map ~f:(fun element ->
          match simplify_def element with
            | Alternative {Alternative.elements} ->
              elements
            | element -> [element]
        )
      in
      begin
        match elements with
          | [element] -> element
          | _ -> Alternative {Alternative.elements}
      end
    | Repetition {Repetition.forward; backward} ->
      let forward = simplify_def forward
      and backward = simplify_def backward in
      Repetition {Repetition.forward; backward}
    | Except {Except.base; except} ->
      let base = simplify_def base
      and except = simplify_def except in
      Except {Except.base; except}
  ) in
  function {rules} ->
    let rules =
      rules
      |> Li.map ~f:(fun {Rule.name; definition} ->
        let definition = simplify_def definition in
        {Rule.name; definition}
      )
    in
    {rules}


module UnitTests = struct
  open Tst

  let check_definition expected actual =
    check_poly ~to_string:Definition.to_string expected actual

  let make rule expected =
    (Definition.to_string rule) >:: (fun _ ->
      match simplify {rules=[{Rule.name="r"; definition=rule}]} with
        | {rules=[{Rule.name="r"; definition}]} -> check_definition expected definition
        | _ -> fail "weird, really..."
    )

  let s = sequence
  let a = alternative
  let r = repetition
  let nt = non_terminal "nt"
  let t1 = terminal "t1"
  let t2 = terminal "t2"
  let t3 = terminal "t3"

  let test = "Grammar" >::: ["simplify" >::: [
    make Definition.Null Definition.Null;
    make t1 t1;
    make nt nt;
    make (s [t1]) t1;
    make (s [s [t1]]) t1;
    make (s [s [s [t1]]]) t1;
    make (s [s [t1; t2]]) (s [t1; t2]);
    make (s [t1; s [t2; t3]]) (s [t1; t2; t3]);
    make (s [t1; s [s [t2; t3]]]) (s [t1; t2; t3]);
    make (s [t1; s [s [s [t2; t3]]]]) (s [t1; t2; t3]);
    make (a [t1]) t1;
    make (a [a [t1]]) t1;
    make (a [a [a [t1]]]) t1;
    make (a [a [t1; t2]]) (a [t1; t2]);
    make (a [t1; a [t2; t3]]) (a [t1; t2; t3]);
    make (a [t1; a [a [t2; t3]]]) (a [t1; t2; t3]);
    make (a [t1; a [a [a [t2; t3]]]]) (a [t1; t2; t3]);
    make (r (s [t1]) (a [t2])) (r t1 t2);
  ]]
end
