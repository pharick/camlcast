open Raycaster
open Support

let a_wall_precomputes_its_geometry () =
  let w = Room.wall ~height:2. ~material:pale (Vec.make 1. 1.) (Vec.make 4. 5.) in
  Alcotest.check vec "edge is b - a" (Vec.make 3. 4.) w.Room.edge;
  Alcotest.check close "length is |edge|" 5. w.Room.length;
  Alcotest.check close "the normal is a unit vector" 1.
    (Vec.length w.Room.normal);
  Alcotest.check close "and is perpendicular to the wall" 0.
    (dot w.Room.normal w.Room.edge)

let a_wall_can_wear_decals () =
  let dec =
    {
      Room.along = 1.;
      z = 1.;
      half_width = 0.5;
      half_height = 0.5;
      image = poster;
    }
  in
  let w =
    Room.wall ~height:2. ~material:pale ~decals:[ dec ] (Vec.make 0. 0.)
      (Vec.make 2. 0.)
  in
  Alcotest.(check int) "the decal is kept" 1 (List.length w.Room.decals);
  let plain =
    Room.wall ~height:2. ~material:pale (Vec.make 0. 0.) (Vec.make 2. 0.)
  in
  Alcotest.(check bool) "a plain wall has none" true (plain.Room.decals = [])

let distance_to_a_wall () =
  let w = Room.wall ~height:2. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.) in
  Alcotest.check close "straight out from the middle" 3.
    (Room.distance_to_wall w (Vec.make 2. 3.));
  Alcotest.check close "off the end clamps to the endpoint" 5.
    (Room.distance_to_wall w (Vec.make (-3.) 4.));
  Alcotest.check close "a point on the wall is zero away" 0.
    (Room.distance_to_wall w (Vec.make 1. 0.))

(* Movement collides with walls through [blocked], so it has to report the
   padding disc overlapping any wall. *)
let blocked_within_the_padding () =
  Alcotest.(check bool)
    "the centre of the room is clear" false
    (Room.blocked room centre);
  Alcotest.(check bool)
    "hard against a wall is blocked" true
    (Room.blocked room (Vec.make (4. -. (Config.collision_padding /. 2.)) 2.));
  Alcotest.(check bool)
    "a whisker outside the padding is clear" false
    (Room.blocked room (Vec.make (4. -. (Config.collision_padding *. 2.)) 2.))

(* A step taken straight along a wall is parallel to it, so the cross product
   that finds an ordinary crossing has nothing to find. Collinear overlap has to
   be caught on its own, or a long enough step walks the whole length of a wall
   and comes out the far side. *)
let collinear_segments_still_cross () =
  let a1 = Vec.make 0. 0. and a2 = Vec.make 4. 0. in
  let crosses = Room.segments_cross a1 a2 in
  Alcotest.(check bool)
    "overlapping collinear segments cross" true
    (crosses (Vec.make 1. 0.) (Vec.make 6. 0.));
  Alcotest.(check bool)
    "so does one lying wholly inside the other" true
    (crosses (Vec.make 1. 0.) (Vec.make 2. 0.));
  Alcotest.(check bool)
    "collinear but clear of it does not" false
    (crosses (Vec.make 5. 0.) (Vec.make 6. 0.));
  Alcotest.(check bool)
    "nor does a parallel one off to the side" false
    (crosses (Vec.make 0. 1.) (Vec.make 4. 1.))

let a_step_along_a_wall_is_refused () =
  let level =
    Room.make ~floor:flat_floor ~ceiling:flat_ceiling
      [ Room.wall ~height:2. ~material:pale (Vec.make 1. 0.) (Vec.make 2. 0.) ]
  in
  Alcotest.(check bool)
    "a step down the line of a wall does not pass through it" false
    (Room.can_step level ~from:(Vec.make 0. 0.) ~dest:(Vec.make 3. 0.));
  Alcotest.(check bool)
    "the same step alongside it is free" true
    (Room.can_step level ~from:(Vec.make 0. 0.5) ~dest:(Vec.make 3. 0.5))

(* Two segments that miss each other are as far apart as the nearest of their
   endpoints is from the other segment. *)
let distance_between_two_segments () =
  let d = Room.distance_between_segments in
  Alcotest.check close "parallel segments are their offset apart" 2.
    (d (Vec.make 0. 0.) (Vec.make 4. 0.) (Vec.make 1. 2.) (Vec.make 3. 2.));
  Alcotest.check close "past the end it is endpoint to endpoint" 5.
    (d (Vec.make 0. 0.) (Vec.make 4. 0.) (Vec.make 7. 4.) (Vec.make 9. 6.));
  Alcotest.check close "crossing segments are no distance apart" 0.
    (d (Vec.make 0. 0.) (Vec.make 4. 0.) (Vec.make 2. (-1.)) (Vec.make 2. 1.))

(* The step sweeps the player's padding disc along the path, so it also catches
   what a test at the destination cannot see: a step that slips past the end of
   a wall close enough to have brushed it, ending clear of every wall on the far
   side without its centre line ever crossing one. *)
let a_step_clipping_a_wall_end_is_refused () =
  let level =
    Room.make ~floor:flat_floor ~ceiling:flat_ceiling
      [ Room.wall ~height:2. ~material:pale (Vec.make 0. 1.) (Vec.make 0. 5.) ]
  in
  let brushing_the_end = 1. -. (Config.collision_padding /. 2.) in
  Alcotest.(check bool)
    "a wide step round the end of a wall clips it" false
    (Room.can_step level
       ~from:(Vec.make (-2.) brushing_the_end)
       ~dest:(Vec.make 2. brushing_the_end));
  Alcotest.(check bool)
    "the same step given the end a wider berth is free" true
    (Room.can_step level
       ~from:(Vec.make (-2.) (1. -. (Config.collision_padding *. 2.)))
       ~dest:(Vec.make 2. (1. -. (Config.collision_padding *. 2.))))

(* path and regular_polygon build the walls of the levels. *)
let path_builds_runs_of_walls () =
  let points = [ Vec.make 0. 0.; Vec.make 1. 0.; Vec.make 1. 1. ] in
  Alcotest.(check int)
    "an open path has one fewer wall than points" 2
    (List.length (Room.path ~height:1. ~material:pale points));
  Alcotest.(check int)
    "a closed path joins back up" 3
    (List.length (Room.path ~closed:true ~height:1. ~material:pale points))

let regular_polygon_has_a_wall_per_side () =
  let hexagon =
    Room.regular_polygon ~center:(Vec.make 0. 0.) ~radius:2. ~sides:6
      ~rotation:0. ~height:3. ~material:pale
  in
  Alcotest.(check int) "one wall per side" 6 (List.length hexagon);
  List.iter
    (fun (w : Room.wall) ->
      Alcotest.check close "every side is the same length"
        (List.hd hexagon).Room.length w.Room.length)
    hexagon

(* A doorway has to leave the wall it is cut into and the threshold that fills
   the gap agreeing with each other: same line, same winding, and a lintel
   recording the wall that still stands above the opening. *)
let a_doorway_splits_the_wall_it_is_cut_into () =
  let jambs, t =
    Room.doorway ~name:"east" ~width:2. ~opening:2.5 ~height:4. ~material:dim
      (Vec.make 5. 0.) (Vec.make 5. 10.)
  in
  Alcotest.(check int) "two jambs are left" 2 (List.length jambs);
  Alcotest.check close "the opening is as wide as asked" 2. t.Room.length;
  Alcotest.check vec "centred on the wall" (Vec.make 5. 4.) t.Room.a;
  Alcotest.check vec "and the other end" (Vec.make 5. 6.) t.Room.b;
  Alcotest.check close "the opening is as tall as asked" 2.5 t.Room.height;
  (* Wound the same way as the wall, which is what Transform.between needs: not
     merely parallel to it but pointing the same way along it. *)
  Alcotest.(check bool)
    "wound with the wall, not against it" true
    (Vec.dot t.Room.edge (Vec.make 0. 10.) > 0.);
  match t.Room.lintel with
  | None -> Alcotest.fail "the doorway should carry a lintel"
  | Some l ->
      Alcotest.check close "the lintel is the wall's height" 4. l.Room.top;
      Alcotest.(check bool)
        "and its material, so the strip above matches the wall" true
        (l.Room.material == dim)

let opening ?door () =
  snd
    (Room.doorway ~name:"a" ?door ~width:1. ~opening:2. ~height:3.
       ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.))

let a_doorway_can_hang_a_door () =
  let bare = opening () and hung = opening ~door:(Door.make mesh) () in
  Alcotest.(check bool) "a bare opening has no leaf" true (bare.Room.door = None);
  Alcotest.(check bool) "a door names its material" true
    (match hung.Room.door with
    | Some d -> d.Door.material == mesh
    | None -> false);
  Alcotest.(check bool)
    "and is shut unless it is told otherwise — a door is a thing you open" true
    (match hung.Room.door with
    | Some d -> d.Door.state = Door.Closed
    | None -> false)

(* A door's state decides two things, and they turn out to be one thing: what is
   drawn across the opening, and what stops a step. Both are asked of pure
   helpers rather than of the renderer, which is what makes them testable
   without a window — and is the reason the two can never drift apart. *)
let a_doors_state_decides_what_is_seen_and_what_is_felt () =
  let bare = opening () in
  Alcotest.(check bool)
    "an opening with no door draws nothing and stops nothing" true
    (Room.leaf bare = None && not (Room.shut bare));
  let ajar = opening ~door:(Door.make ~state:Door.Open mesh) () in
  Alcotest.(check bool)
    "and an open door is exactly the same in both respects" true
    (Room.leaf ajar = None && not (Room.shut ajar));
  let closed = opening ~door:(Door.make ~state:Door.Closed mesh) () in
  Alcotest.(check bool)
    "a closed door draws its own leaf" true
    (match Room.leaf closed with Some m -> m == mesh | None -> false);
  Alcotest.(check bool) "and stops a step" true (Room.shut closed)

let () =
  Alcotest.run "Room"
    [
      ( "walls",
        [
          case "a wall precomputes its geometry" a_wall_precomputes_its_geometry;
          case "a wall can wear decals" a_wall_can_wear_decals;
          case "distance to a wall" distance_to_a_wall;
        ] );
      ( "collision",
        [
          case "blocked within the padding" blocked_within_the_padding;
          case "collinear segments still cross" collinear_segments_still_cross;
          case "a step along a wall is refused" a_step_along_a_wall_is_refused;
          case "distance between two segments" distance_between_two_segments;
          case "a step clipping a wall end is refused"
            a_step_clipping_a_wall_end_is_refused;
        ] );
      ( "building",
        [
          case "path builds runs of walls" path_builds_runs_of_walls;
          case "regular polygon has a wall per side"
            regular_polygon_has_a_wall_per_side;
        ] );
      ( "doorways",
        [
          case "a doorway splits the wall it is cut into"
            a_doorway_splits_the_wall_it_is_cut_into;
          case "a doorway can hang a door" a_doorway_can_hang_a_door;
          case "a door's state decides what is seen and what is felt"
            a_doors_state_decides_what_is_seen_and_what_is_felt;
        ] );
    ]
