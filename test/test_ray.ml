open Raycaster
open Support

let cast direction = nearest_hit room ~origin:centre ~direction

(* From the centre of the 4 x 4 room every wall is 2 cells away, whichever way
   we look. *)
let axis_aligned_distances () =
  List.iter
    (fun (name, direction) ->
      Alcotest.check close (name ^ " distance") 2. (cast direction).Ray.distance)
    [
      ("east", Vec.make 1. 0.);
      ("west", Vec.make (-1.) 0.);
      ("south", Vec.make 0. 1.);
      ("north", Vec.make 0. (-1.));
    ]

(* Distance is measured in units of [direction], not in cells: with a diagonal
   direction, 2 of it is 2*sqrt 2 cells of real distance. That is the property
   that removes the fish-eye. *)
let diagonal_distance () =
  Alcotest.check close "measured in units of direction" 2.
    (cast (Vec.make 1. 1.)).Ray.distance

let reports_the_wall_that_was_hit () =
  let hit = cast (Vec.make 1. 0.) in
  Alcotest.(check int) "the east wall's texture id" 1 hit.Ray.wall.World.texture;
  (* The east wall runs (4,0) -> (4,4); the ray from (2,2) meets it at (4,2),
     halfway, which is 2 along its length of 4. *)
  Alcotest.check close "how far along the wall it struck" 2. hit.Ray.along

(* A ray must not be caught by a wall that lies behind it or off to the side. *)
let walls_behind_and_beside_are_missed () =
  let just_one = Ray.cast room ~origin:centre ~direction:(Vec.make 1. 0.) in
  Alcotest.(check int) "only the wall ahead is hit" 1 (List.length just_one)

(* Variable heights and sloped floors mean a near wall need not hide what is
   behind it, so cast keeps every wall it crosses, farthest first. *)
let collects_every_wall_back_to_front () =
  let hits =
    Ray.cast room_with_pillar ~origin:centre ~direction:(Vec.make 1. 0.)
  in
  Alcotest.(check int) "the pillar and the wall behind it" 2 (List.length hits);
  Alcotest.check (Alcotest.list close) "farthest first" [ 2.; 1. ]
    (List.map (fun (h : Ray.hit) -> h.Ray.distance) hits);
  Alcotest.(check int)
    "the nearest is the pillar" 2
    (Option.get (Ray.nearest hits)).Ray.wall.World.texture

(* [along] threads the texture across the wall, so it has to stay within the
   wall's length whatever direction the ray comes from. *)
let along_stays_on_the_wall () =
  List.iter
    (fun angle ->
      let hit = cast (Vec.of_angle angle) in
      Alcotest.(check bool)
        (Printf.sprintf "along at %.2f rad is on the wall" angle)
        true
        (hit.Ray.along >= 0. && hit.Ray.along <= hit.Ray.wall.World.length))
    (List.init 32 (fun i -> float_of_int i *. Float.pi /. 16.))

(* Defensive: standing on a wall must not produce a zero distance, or the
   renderer would divide by it. *)
let origin_on_a_wall () =
  let hit =
    nearest_hit room ~origin:(Vec.make 0. 2.) ~direction:(Vec.make 1. 0.)
  in
  Alcotest.(check bool)
    "distance stays a usable divisor" true (hit.Ray.distance > 0.)

let () =
  Alcotest.run "Ray"
    [
      ( "distance",
        [
          case "axis aligned" axis_aligned_distances;
          case "diagonal" diagonal_distance;
        ] );
      ( "hits",
        [
          case "reports the wall that was hit" reports_the_wall_that_was_hit;
          case "walls behind and beside are missed"
            walls_behind_and_beside_are_missed;
          case "collects every wall back to front"
            collects_every_wall_back_to_front;
        ] );
      ("texturing", [ case "along stays on the wall" along_stays_on_the_wall ]);
      ("edge cases", [ case "origin on a wall" origin_on_a_wall ]);
    ]
