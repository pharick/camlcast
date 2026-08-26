(** The parts of a call an editor can change.

    A sprite's size, a decal's height up the wall, a floor's plane and the air
    a world is seen through are five things in the guide and one thing here.
    These check that the one thing covers all five, that a label says what it
    can honestly say, and that writing a value back produces something that
    still parses. *)

let case name body = Alcotest.test_case name `Quick body

(* One of each: a sprite whose size and glow are numbers and whose image is a
   name; a decal placed by four numbers; a floor that is a plane of three and a
   material; and a world whose air is a name. Written the way a description is,
   with one argument worked out from state so that the line between what can
   be changed and what cannot is in the fixture rather than only in the prose. *)
let fixture =
  {ocaml|open Camlcast

let level lit =
  P.(
    world ~atmosphere:Surfaces.air
      [
        room ~height:3. ~material:Surfaces.stone
          ~floor:(floor ~plane:(Plane.make ~a:0.06 ~b:0.03 ~c:0.) Surfaces.ground)
          ~ceiling:(roof Surfaces.soffit)
          [
            sprite ~size:0.9 ~glow:(if lit then 0.8 else 0.) ~image:Pictures.flame
              (Vec.make 2. (-3.));
            decal ~along:2. ~z:1.6 ~half_width:0.9 ~half_height:0.9
              Pictures.painting;
          ];
      ])
|ocaml}

let parsed () =
  match Camlcast_edit.Span.parse ~path:"level.ml" fixture with
  | Ok parsed -> parsed
  | Error (`Msg m) -> Alcotest.failf "the fixture does not parse: %s" m

(* By callee rather than by where the text sits: a parenthesised call is
   anchored at its bracket, so looking for one by its name lands a column off.
   See Span.at. *)
let call callee =
  match
    List.find_opt
      (fun (c : Camlcast_edit.Span.call) -> c.callee = callee)
      (Camlcast_edit.Span.calls (parsed ()))
  with
  | Some call -> call
  | None -> Alcotest.failf "no call to %S in the fixture" callee

let fields needle = Camlcast_edit.Field.of_call (parsed ()) (call needle)

let describe (field : Camlcast_edit.Field.t) =
  ( field.label,
    match field.value with
    | Number n -> Printf.sprintf "%g" n
    | Name name -> name )

let a_sprite_offers_its_numbers_and_its_picture () =
  Alcotest.(check (list (pair string string)))
    "the glow is worked out from state, so it is not a field at all"
    [
      ("size", "0.9"); ("image", "Pictures.flame"); ("#3.x", "2"); ("#3.y", "-3");
    ]
    (List.map describe (fields "sprite"))

let a_decal_offers_the_four_numbers_that_place_it () =
  Alcotest.(check (list (pair string string)))
    "along and up the wall, and how big"
    [
      ("along", "2");
      ("z", "1.6");
      ("half_width", "0.9");
      ("half_height", "0.9");
      ("#4", "Pictures.painting");
    ]
    (List.map describe (fields "decal"))

let a_plane_is_three_numbers_like_any_other () =
  Alcotest.(check (list (pair string string)))
    "a floor's plane needs no special case to be editable"
    [ ("a", "0.06"); ("b", "0.03"); ("c", "0") ]
    (List.map describe (fields "Plane.make"))

let the_air_a_world_is_seen_through_is_a_name () =
  Alcotest.(check (list (pair string string)))
    "the atmosphere, and the children which are a rule rather than a value"
    [ ("atmosphere", "Surfaces.air") ]
    (List.map describe (fields "world"))

(* {1 Writing} *)

let field needle label =
  match List.find_opt (fun f -> fst (describe f) = label) (fields needle) with
  | Some field -> field
  | None -> Alcotest.failf "no field %S on %S" label needle

let writing_a_number_leaves_something_that_parses () =
  let parsed = parsed () in
  let edits =
    List.map
      (fun (needle, label, number) ->
        match
          Camlcast_edit.Field.write (field needle label)
            (Camlcast_edit.Field.Number number)
        with
        | Ok edit -> edit
        | Error (`Msg m) -> Alcotest.failf "%s: %s" label m)
      [
        ("sprite", "size", 1.4); ("decal", "z", -0.5); ("Plane.make", "a", 0.12);
      ]
  in
  match Camlcast_edit.Span.splice parsed edits with
  | Error (`Msg m) -> Alcotest.failf "splice refused: %s" m
  | Ok written ->
      Alcotest.(check bool)
        "three values across three calls, and it is still OCaml" true
        (match Camlcast_edit.Span.parse ~path:"level.ml" written with
        | Ok _ -> true
        | Error _ -> false);
      let holds needle =
        let rec find i =
          i + String.length needle <= String.length written
          && (String.sub written i (String.length needle) = needle
             || find (i + 1))
        in
        find 0
      in
      Alcotest.(check bool) "the size" true (holds "~size:1.4");
      (* A negative number arrives bracketed, or it would be read as a
         subtraction where it lands. *)
      Alcotest.(check bool)
        "the negative one, bracketed" true (holds "~z:(-0.5)");
      Alcotest.(check bool) "the plane" true (holds "~a:0.12")

let swapping_a_name_writes_the_name () =
  match
    Camlcast_edit.Field.write
      (field "world" "atmosphere")
      (Camlcast_edit.Field.Name "Surfaces.dusk")
  with
  | Error (`Msg m) -> Alcotest.failf "refused: %s" m
  | Ok (_, written) ->
      Alcotest.(check string) "as given, unchecked" "Surfaces.dusk" written

(* A shape question rather than a scope question, and the shape is known. *)
let a_value_of_the_wrong_shape_is_refused () =
  Alcotest.(check bool)
    "a name where a number is" true
    (Result.is_error
       (Camlcast_edit.Field.write (field "sprite" "size")
          (Camlcast_edit.Field.Name "Surfaces.stone")));
  Alcotest.(check bool)
    "and a number where a name is" true
    (Result.is_error
       (Camlcast_edit.Field.write
          (field "world" "atmosphere")
          (Camlcast_edit.Field.Number 3.)))

let () =
  Alcotest.run "Field"
    [
      ( "what a call offers",
        [
          case "a sprite offers its numbers and its picture"
            a_sprite_offers_its_numbers_and_its_picture;
          case "a decal offers the four numbers that place it"
            a_decal_offers_the_four_numbers_that_place_it;
          case "a plane is three numbers like any other"
            a_plane_is_three_numbers_like_any_other;
          case "the air a world is seen through is a name"
            the_air_a_world_is_seen_through_is_a_name;
        ] );
      ( "writing one back",
        [
          case "writing a number leaves something that parses"
            writing_a_number_leaves_something_that_parses;
          case "swapping a name writes the name" swapping_a_name_writes_the_name;
          case "a value of the wrong shape is refused"
            a_value_of_the_wrong_shape_is_refused;
        ] );
    ]
