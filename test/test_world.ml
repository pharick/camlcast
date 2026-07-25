open Raycaster
open Support

let a_wall_precomputes_its_geometry () =
  let w = World.wall ~height:2. ~texture:1 (Vec.make 1. 1.) (Vec.make 4. 5.) in
  Alcotest.check vec "edge is b - a" (Vec.make 3. 4.) w.World.edge;
  Alcotest.check close "length is |edge|" 5. w.World.length;
  Alcotest.check close "the normal is a unit vector" 1.
    (Vec.length w.World.normal);
  Alcotest.check close "and is perpendicular to the wall" 0.
    (dot w.World.normal w.World.edge)

let a_wall_can_wear_decals () =
  let dec =
    {
      World.along = 1.;
      z = 1.;
      half_width = 0.5;
      half_height = 0.5;
      image = Image.poster;
    }
  in
  let w =
    World.wall ~height:2. ~texture:1 ~decals:[ dec ] (Vec.make 0. 0.)
      (Vec.make 2. 0.)
  in
  Alcotest.(check int) "the decal is kept" 1 (List.length w.World.decals);
  let plain =
    World.wall ~height:2. ~texture:1 (Vec.make 0. 0.) (Vec.make 2. 0.)
  in
  Alcotest.(check bool) "a plain wall has none" true (plain.World.decals = [])

let distance_to_a_wall () =
  let w = World.wall ~height:2. ~texture:1 (Vec.make 0. 0.) (Vec.make 4. 0.) in
  Alcotest.check close "straight out from the middle" 3.
    (World.distance_to_wall w (Vec.make 2. 3.));
  Alcotest.check close "off the end clamps to the endpoint" 5.
    (World.distance_to_wall w (Vec.make (-3.) 4.));
  Alcotest.check close "a point on the wall is zero away" 0.
    (World.distance_to_wall w (Vec.make 1. 0.))

(* Movement collides with walls through [blocked], so it has to report the
   padding disc overlapping any wall. *)
let blocked_within_the_padding () =
  Alcotest.(check bool)
    "the centre of the room is clear" false
    (World.blocked room centre);
  Alcotest.(check bool)
    "hard against a wall is blocked" true
    (World.blocked room (Vec.make (4. -. (Config.collision_padding /. 2.)) 2.));
  Alcotest.(check bool)
    "a whisker outside the padding is clear" false
    (World.blocked room (Vec.make (4. -. (Config.collision_padding *. 2.)) 2.))

(* A step taken straight along a wall is parallel to it, so the cross product
   that finds an ordinary crossing has nothing to find. Collinear overlap has to
   be caught on its own, or a long enough step walks the whole length of a wall
   and comes out the far side. *)
let collinear_segments_still_cross () =
  let a1 = Vec.make 0. 0. and a2 = Vec.make 4. 0. in
  let crosses = World.segments_cross a1 a2 in
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
    World.make ~spawn:(Vec.make 0. 0.) ~floor:flat_floor ~ceiling:flat_ceiling
      [ World.wall ~height:2. ~texture:1 (Vec.make 1. 0.) (Vec.make 2. 0.) ]
  in
  Alcotest.(check bool)
    "a step down the line of a wall does not pass through it" false
    (World.can_step level ~from:(Vec.make 0. 0.) ~dest:(Vec.make 3. 0.));
  Alcotest.(check bool)
    "the same step alongside it is free" true
    (World.can_step level ~from:(Vec.make 0. 0.5) ~dest:(Vec.make 3. 0.5))

(* Two segments that miss each other are as far apart as the nearest of their
   endpoints is from the other segment. *)
let distance_between_two_segments () =
  let d = World.distance_between_segments in
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
    World.make ~spawn:(Vec.make 0. 0.) ~floor:flat_floor ~ceiling:flat_ceiling
      [ World.wall ~height:2. ~texture:1 (Vec.make 0. 1.) (Vec.make 0. 5.) ]
  in
  let brushing_the_end = 1. -. (Config.collision_padding /. 2.) in
  Alcotest.(check bool)
    "a wide step round the end of a wall clips it" false
    (World.can_step level
       ~from:(Vec.make (-2.) brushing_the_end)
       ~dest:(Vec.make 2. brushing_the_end));
  Alcotest.(check bool)
    "the same step given the end a wider berth is free" true
    (World.can_step level
       ~from:(Vec.make (-2.) (1. -. (Config.collision_padding *. 2.)))
       ~dest:(Vec.make 2. (1. -. (Config.collision_padding *. 2.))))

(* path and regular_polygon build the walls of the levels. *)
let path_builds_runs_of_walls () =
  let points = [ Vec.make 0. 0.; Vec.make 1. 0.; Vec.make 1. 1. ] in
  Alcotest.(check int)
    "an open path has one fewer wall than points" 2
    (List.length (World.path ~height:1. ~texture:1 points));
  Alcotest.(check int)
    "a closed path joins back up" 3
    (List.length (World.path ~closed:true ~height:1. ~texture:1 points))

let regular_polygon_has_a_wall_per_side () =
  let hexagon =
    World.regular_polygon ~center:(Vec.make 0. 0.) ~radius:2. ~sides:6
      ~rotation:0. ~height:3. ~texture:1
  in
  Alcotest.(check int) "one wall per side" 6 (List.length hexagon);
  List.iter
    (fun (w : World.wall) ->
      Alcotest.check close "every side is the same length"
        (List.hd hexagon).World.length w.World.length)
    hexagon

let the_default_level_is_playable () =
  Alcotest.(check bool)
    "the spawn point is not inside a wall" false
    (World.blocked World.default World.(default.spawn));
  Alcotest.(check bool)
    "and the level is large, with walls facing many ways" true
    (Array.length World.(default.walls) >= 30)

(* The default level's floor is genuinely inclined, and it is open to the sky
   rather than roofed. *)
let the_default_level_is_open_to_the_sky () =
  let tilted p =
    Plane.elevation p (Vec.make 0. 0.) <> Plane.elevation p (Vec.make 1. 0.)
  in
  Alcotest.(check bool)
    "the floor is inclined" true
    (tilted World.(default.floor));
  Alcotest.(check bool)
    "there is no ceiling — the level is open" true
    (World.(default.ceiling) = None)

(* The default level shows off the props: sprites in the world, decals on a
   wall, and at least one see-through wall. *)
let the_default_level_is_furnished () =
  Alcotest.(check bool)
    "it has sprites" true
    (Array.length World.(default.sprites) > 0);
  Alcotest.(check bool)
    "some wall wears decals" true
    (Array.exists
       (fun (w : World.wall) -> w.World.decals <> [])
       World.(default.walls));
  Alcotest.(check bool)
    "some wall is see-through" true
    (Array.exists
       (fun (w : World.wall) ->
         not (Palette.pattern w.World.texture).Texture.opaque)
       World.(default.walls))

let () =
  Alcotest.run "World"
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
      ( "the default level",
        [
          case "is playable" the_default_level_is_playable;
          case "is open to the sky" the_default_level_is_open_to_the_sky;
          case "is furnished" the_default_level_is_furnished;
        ] );
    ]
