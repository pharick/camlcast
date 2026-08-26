(** Fitting a room into a panel, and picking a pixel back off it.

    The property worth holding is that the two directions agree. A view that
    drew a corner at one place and picked it up at another would be wrong in a
    way nobody could see from the picture: the wall would move, just not to
    where the pointer was. Everything below is that one claim, asked in the
    ways it can fail. *)

open Camlcast_core
open Support

let case name body = Alcotest.test_case name `Quick body

(* A panel like the one Debug_map places: square, inset from its own edge. *)
let panel = (8, 8, 200, 200)
let inset = 10

let view bounds =
  let x, y, width, height = panel in
  Overhead.fit ~bounds ~x ~y ~width ~height ~inset

(* Within a pixel's worth of world units: a pixel is a whole number and a point
   is not, so that is the whole of what can be asked. *)
let round_trips view point =
  let back = Overhead.to_room view (Overhead.to_panel view point) in
  let tolerance = 1.5 /. Overhead.scale view in
  Float.abs (back.Vec.x -. point.Vec.x) <= tolerance
  && Float.abs (back.Vec.y -. point.Vec.y) <= tolerance

let a_point_survives_the_round_trip () =
  let view = view (-6., -6., 6., 6.) in
  List.iter
    (fun point ->
      Alcotest.(check bool)
        (Printf.sprintf "(%g,%g) comes back" point.Vec.x point.Vec.y)
        true (round_trips view point))
    [
      Vec.make 0. 0.;
      Vec.make (-6.) (-6.);
      Vec.make 6. 6.;
      Vec.make (-6.) 6.;
      Vec.make 2.5 (-3.25);
      Vec.make 0.001 (-0.001);
    ]

(* A room far from its own origin is the case a projection centred on the
   origin would get wrong, and the reason this one is centred on the room. *)
let a_room_far_from_the_origin_round_trips () =
  let view = view (100., -250., 112., -238.) in
  Alcotest.(check bool)
    "a corner two hundred cells out" true
    (round_trips view (Vec.make 100. (-250.)));
  Alcotest.(check bool)
    "and the middle" true
    (round_trips view (Vec.make 106. (-244.)))

let a_long_thin_room_is_centred_not_cornered () =
  let view = view (-10., -1., 10., 1.) in
  let x, y, width, height = panel in
  let px, py = Overhead.to_panel view (Vec.make 0. 0.) in
  Alcotest.(check int)
    "its middle is the panel's middle across"
    (x + (width / 2))
    px;
  Alcotest.(check int) "and down" (y + (height / 2)) py

(* World y grows downward and so does the screen's. A flip here would draw
   every room mirrored and nothing would say so. *)
let the_room_is_not_flipped () =
  let view = view (-6., -6., 6., 6.) in
  let _, top = Overhead.to_panel view (Vec.make 0. (-6.)) in
  let _, bottom = Overhead.to_panel view (Vec.make 0. 6.) in
  Alcotest.(check bool) "smaller y is higher on the panel" true (top < bottom);
  let left, _ = Overhead.to_panel view (Vec.make (-6.) 0.) in
  let right, _ = Overhead.to_panel view (Vec.make 6. 0.) in
  Alcotest.(check bool) "and smaller x is further left" true (left < right)

let a_room_fits_inside_its_inset () =
  let view = view (-6., -6., 6., 6.) in
  let x, y, width, height = panel in
  List.iter
    (fun corner ->
      let px, py = Overhead.to_panel view corner in
      Alcotest.(check bool)
        "a corner lands within the panel, clear of the inset" true
        (px >= x + inset - 1
        && px <= x + width - inset + 1
        && py >= y + inset - 1
        && py <= y + height - inset + 1))
    [ Vec.make (-6.) (-6.); Vec.make 6. 6.; Vec.make 6. (-6.) ]

(* A description can be halfway through being written, and a room whose whole
   boundary is one point would divide by nothing. *)
let a_room_with_no_extent_does_not_divide_by_nothing () =
  let view = view (3., 3., 3., 3.) in
  Alcotest.(check bool)
    "a finite scale" true
    (Float.is_finite (Overhead.scale view));
  let px, py = Overhead.to_panel view (Vec.make 3. 3.) in
  Alcotest.(check bool)
    "and a point that lands somewhere" true
    (px > 0 && py > 0)

let a_room_with_no_walls_has_no_bounds () =
  let empty = Room.make ~floor:flat_floor ~ceiling:flat_ceiling [] in
  Alcotest.(check bool)
    "nothing to measure" true
    (Option.is_none (Overhead.bounds empty));
  Alcotest.(check bool)
    "and the fixture room has some" true
    (Option.is_some (Overhead.bounds room))

let () =
  Alcotest.run "Overhead"
    [
      ( "the two directions agree",
        [
          case "a point survives the round trip" a_point_survives_the_round_trip;
          case "a room far from the origin round trips"
            a_room_far_from_the_origin_round_trips;
        ] );
      ( "where it puts a room",
        [
          case "a long thin room is centred, not cornered"
            a_long_thin_room_is_centred_not_cornered;
          case "the room is not flipped" the_room_is_not_flipped;
          case "a room fits inside its inset" a_room_fits_inside_its_inset;
        ] );
      ( "what it refuses to break on",
        [
          case "a room with no extent does not divide by nothing"
            a_room_with_no_extent_does_not_divide_by_nothing;
          case "a room with no walls has no bounds"
            a_room_with_no_walls_has_no_bounds;
        ] );
    ]
