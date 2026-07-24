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
        [ case "blocked within the padding" blocked_within_the_padding ] );
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
