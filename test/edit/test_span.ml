(** Finding an expression in a file, and changing it without disturbing the file
    around it.

    The claim worth checking is not that a coordinate can be replaced — that is
    a substring — but that {b nothing else moves}. A tool that edits a game's
    own source has to leave the comments, the blank lines and the alignment
    exactly as they were, because the developer wrote them and did not ask.
    Parsing to locate and splicing bytes is how that is achieved, and asserting
    on the whole file is how it is checked. *)

let case name body = Alcotest.test_case name `Quick body

(* A description written the way one is: a local open, a comment in the middle
   of it, one argument computed and the rest not, and alignment ocamlformat
   would not have chosen. *)
let fixture =
  {ocaml|open Camlcast

let lit = true

(* The north wall. Its glow follows the brazier, so it cannot be dragged. *)
let level =
  P.(
    wall ~height:2.5 ~material:Surfaces.stone
      (Vec.make (-3.) (-4.))
      (Vec.make 3. (-4.)))

let torch = P.sprite ~size:0.9 ~glow:(if lit then 0.8 else 0.) ~image:flame
    (Vec.make 2. 3.)
|ocaml}

let parsed () =
  match Camlcast_edit.Span.parse ~path:"level.ml" fixture with
  | Ok parsed -> parsed
  | Error (`Msg message) ->
      Alcotest.failf "the fixture does not parse: %s" message

(* Where a piece of the fixture sits, as a line and a column, so that editing
   the fixture cannot silently move an assertion off its target. *)
let anchor needle =
  match String.index_opt fixture 'x' with
  | _ ->
      let rec find index =
        if index + String.length needle > String.length fixture then
          Alcotest.failf "%S is not in the fixture" needle
        else if String.sub fixture index (String.length needle) = needle then
          index
        else find (index + 1)
      in
      let offset = find 0 in
      let line = ref 1 and bol = ref 0 in
      String.iteri
        (fun index char ->
          if index < offset && char = '\n' then begin
            incr line;
            bol := index + 1
          end)
        fixture;
      (!line, offset - !bol)

let slice text (span : Camlcast_edit.Span.span) =
  String.sub text span.start (span.stop - span.start)

let call needle =
  let line, column = anchor needle in
  match Camlcast_edit.Span.at (parsed ()) ~line ~column with
  | Some call -> call
  | None -> Alcotest.failf "no call anchored at %S (%d,%d)" needle line column

(* {1 Finding} *)

let a_position_finds_the_call_it_names () =
  let found = call "wall ~height" in
  Alcotest.(check string) "the callee, as written" "wall" found.callee;
  Alcotest.(check string)
    "and the whole application"
    "wall ~height:2.5 ~material:Surfaces.stone\n\
    \      (Vec.make (-3.) (-4.))\n\
    \      (Vec.make 3. (-4.))"
    (slice fixture found.span)

let a_position_naming_nothing_finds_nothing () =
  Alcotest.(check bool)
    "a line and column with no application starting there" true
    (Option.is_none (Camlcast_edit.Span.at (parsed ()) ~line:1 ~column:0))

(* {1 Classifying} *)

let describe (argument : Camlcast_edit.Span.argument) =
  let what =
    match argument.value with
    | Numbers spans ->
        "numbers " ^ String.concat "," (List.map (slice fixture) spans)
    | Name span -> "name " ^ slice fixture span
    | Computed -> "computed"
  in
  (Option.value argument.label ~default:"_", what)

let arguments_are_numbers_names_or_computed () =
  Alcotest.(check (list (pair string string)))
    "a height and two points are numbers; a material is a name"
    [
      ("height", "numbers 2.5");
      ("material", "name Surfaces.stone");
      ("_", "numbers (-3.),(-4.)");
      ("_", "numbers 3.,(-4.)");
    ]
    (List.map describe (call "wall ~height").arguments)

(* The case the whole design turns on: editability is per argument. A sprite
   whose glow is worked out from state can still be dragged, because where it
   stands is written down and only its glow is not. *)
let one_computed_argument_does_not_spoil_the_rest () =
  Alcotest.(check (list (pair string string)))
    "the glow is computed and the position is not"
    [
      ("size", "numbers 0.9");
      ("glow", "computed");
      ("image", "name flame");
      ("_", "numbers 2.,3.");
    ]
    (List.map describe (call "P.sprite").arguments)

(* {1 Changing} *)

let expected_after_dragging =
  {ocaml|open Camlcast

let lit = true

(* The north wall. Its glow follows the brazier, so it cannot be dragged. *)
let level =
  P.(
    wall ~height:2.5 ~material:Surfaces.stone
      (Vec.make (-3.5) (-4.))
      (Vec.make 3. (-4.)))

let torch = P.sprite ~size:0.9 ~glow:(if lit then 0.8 else 0.) ~image:flame
    (Vec.make 2. 3.)
|ocaml}

let one_number_changes_and_nothing_else_does () =
  let found = call "wall ~height" in
  let first_point = List.nth found.arguments 2 in
  let span =
    match first_point.value with
    | Numbers (span :: _) -> span
    | _ -> Alcotest.fail "the first point should be numbers"
  in
  match
    Camlcast_edit.Span.splice (parsed ())
      [ (span, Camlcast_edit.Span.number (-3.5)) ]
  with
  | Error (`Msg message) -> Alcotest.failf "splice refused: %s" message
  | Ok written ->
      (* The whole file, not the line: the comment, the blank lines and the
         alignment the developer chose are all part of the claim. *)
      Alcotest.(check string)
        "one coordinate, and the file otherwise byte for byte"
        expected_after_dragging written

let expected_after_three_edits =
  {ocaml|open Camlcast

let lit = true

(* The north wall. Its glow follows the brazier, so it cannot be dragged. *)
let level =
  P.(
    wall ~height:10.25 ~material:Surfaces.stone
      (Vec.make (-3.) 8.)
      (Vec.make 3. 9.))

let torch = P.sprite ~size:0.9 ~glow:(if lit then 0.8 else 0.) ~image:flame
    (Vec.make 2. 3.)
|ocaml}

let several_changes_do_not_move_each_other () =
  let found = call "wall ~height" in
  let spans =
    List.concat_map
      (fun (argument : Camlcast_edit.Span.argument) ->
        match argument.value with Numbers spans -> spans | _ -> [])
      found.arguments
  in
  (* Given deliberately out of order and spread across the call: replacing any
     one of these changes where every later one sits, so a caller applying them
     one at a time would write the second into the wrong bytes. *)
  let edits =
    [
      (List.nth spans 4, Camlcast_edit.Span.number 9.);
      (List.nth spans 0, Camlcast_edit.Span.number 10.25);
      (List.nth spans 2, Camlcast_edit.Span.number 8.);
    ]
  in
  match Camlcast_edit.Span.splice (parsed ()) edits with
  | Error (`Msg message) -> Alcotest.failf "splice refused: %s" message
  | Ok written ->
      Alcotest.(check string)
        "three numbers across one call, and the file otherwise untouched"
        expected_after_three_edits written

let overlapping_changes_are_refused () =
  let found = call "wall ~height" in
  let whole = found.span in
  let inside =
    match (List.nth found.arguments 0).value with
    | Numbers (span :: _) -> span
    | _ -> Alcotest.fail "expected a number"
  in
  match
    Camlcast_edit.Span.splice (parsed ()) [ (whole, "x"); (inside, "y") ]
  with
  | Ok _ -> Alcotest.fail "two edits over the same bytes should be refused"
  | Error (`Msg message) ->
      Alcotest.(check bool)
        "and says so in the terms the caller gave" true
        (String.length message > 0)

let a_file_that_does_not_parse_is_a_condition () =
  match Camlcast_edit.Span.parse ~path:"broken.ml" "let x = (" with
  | Ok _ -> Alcotest.fail "that is not OCaml"
  | Error (`Msg message) ->
      Alcotest.(check bool)
        "reported rather than raised, and naming the file" true
        (String.length message > 0)

let () =
  Alcotest.run "Span"
    [
      ( "finding",
        [
          case "a position finds the call it names"
            a_position_finds_the_call_it_names;
          case "a position naming nothing finds nothing"
            a_position_naming_nothing_finds_nothing;
        ] );
      ( "classifying",
        [
          case "arguments are numbers, names or computed"
            arguments_are_numbers_names_or_computed;
          case "one computed argument does not spoil the rest"
            one_computed_argument_does_not_spoil_the_rest;
        ] );
      ( "changing",
        [
          case "one number changes and nothing else does"
            one_number_changes_and_nothing_else_does;
          case "several changes do not move each other"
            several_changes_do_not_move_each_other;
          case "overlapping changes are refused" overlapping_changes_are_refused;
          case "a file that does not parse is a condition"
            a_file_that_does_not_parse_is_a_condition;
        ] );
    ]
