open Camlcast_core
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
  Alcotest.(check bool)
    "and it is the room's own wall, not a copy" true
    (hit.Ray.wall.Room.material == pale);
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
  Alcotest.(check bool)
    "the nearest is the pillar" true
    ((Option.get (Ray.nearest hits)).Ray.wall.Room.material == dim)

(* [along] threads the material across the wall, so it has to stay within the
   wall's length whatever direction the ray comes from. *)
let along_stays_on_the_wall () =
  List.iter
    (fun angle ->
      let hit = cast (Vec.of_angle angle) in
      Alcotest.(check bool)
        (Printf.sprintf "along at %.2f rad is on the wall" angle)
        true
        (hit.Ray.along >= 0. && hit.Ray.along <= hit.Ray.wall.Room.length))
    (List.init 32 (fun i -> float_of_int i *. Float.pi /. 16.))

(* Thresholds are met by exactly the same segment test as walls, but reported
   separately so the renderer can recurse through them instead of painting them.
   The index is what ties an opening back to its portal. *)
let openings_are_found_like_walls () =
  let first = World.room two_rooms 0 in
  let through = Ray.openings first ~origin:centre ~direction:(Vec.make 1. 0.) in
  Alcotest.(check int) "the doorway ahead is found" 1 (List.length through);
  let opening = List.hd through in
  Alcotest.check close "at the east wall, two cells away" 2.
    opening.Ray.distance;
  Alcotest.(check int) "indexing the room's thresholds" 0 opening.Ray.index;
  (* The doorway runs (4,1.5) -> (4,2.5); the ray from (2,2) meets its middle,
     which is 0.5 along its length of 1. *)
  Alcotest.check close "how far along the opening it passed" 0.5
    opening.Ray.along;
  Alcotest.(check int)
    "a ray that misses it finds nothing" 0
    (List.length
       (Ray.openings first ~origin:centre ~direction:(Vec.make 0. 1.)));
  Alcotest.(check int)
    "and neither does one pointing away from it" 0
    (List.length
       (Ray.openings first ~origin:centre ~direction:(Vec.make (-1.) 0.)))

(* Defensive: standing on a wall must not produce a zero distance, or the
   renderer would divide by it. *)
let origin_on_a_wall () =
  let hit =
    nearest_hit room ~origin:(Vec.make 0. 2.) ~direction:(Vec.make 1. 0.)
  in
  Alcotest.(check bool)
    "distance stays a usable divisor" true (hit.Ray.distance > 0.)

(* {!Ray.segment} and {!Room.segments_cross} are the same cross-product
   intersection written twice. Not similar — the same: the same denominator, the
   same offset, and the same two quotients, bit for bit over half a million
   random configurations. What differs is deliberate and is all guard. A ray runs
   forever and only has to be ahead by [min_distance]; a step stops, so both its
   parameters are bounded. The step has a collinear-overlap branch and the ray
   has none, because a ray sliding along a wall meets nothing worth drawing while
   a step sliding along one still has to be stopped. And the parallel test is
   strict on one side and inclusive on the other, so that a segment of no length
   keeps the branch it has always taken.

   A shared core would be the way to keep the arithmetic from drifting, and it
   was measured rather than assumed. The version that pins what is worth pinning
   has to hand back both parameters, and on this compiler a returned pair
   allocates however hard it is inlined: 327 words per cast against 163, and
   33.9 ms against 30.1 for the same work — thirteen per cent of Ray.cast, which
   bench/frame.ml already calls the frame's dominant cost. A continuation is
   255 words. Handing back one parameter at a time is free and shares only the
   division, leaving the two things that could actually flip — which endpoint
   the offset runs from, and which segment's direction belongs to which
   parameter — written out twice exactly as before.

   So they stay apart, and this is what holds them together instead. Both halves
   of what a cast reports are put to a step: the distance, by walking exactly
   that far and requiring the walk to cross the wall while a walk a hair shorter
   does not; and the offset along the wall, by measuring the hit back to that
   wall's own start. A flipped sign or a swapped pairing moves one of them. *)
let a_cast_agrees_with_a_step_about_where_a_wall_is () =
  let checked = ref 0 in
  List.iter
    (fun (name, direction) ->
      let hits = Ray.cast room ~origin:centre ~direction in
      List.iter
        (fun (hit : Ray.hit) ->
          let w = hit.Ray.wall in
          let t = hit.Ray.distance in
          let where = Vec.add centre (Vec.scale direction t) in
          incr checked;
          (* [along] is the same point, measured the wall's way. *)
          Alcotest.check close
            (Printf.sprintf "%s: along matches the hit point" name)
            hit.Ray.along
            (Vec.length (Vec.sub where w.Room.a));
          (* Bracketed, not sat on. A step ending exactly at the hit is the
             boundary of the step's own [t <= 1.], and rebuilding the endpoint
             as [centre + t * direction] and taking it apart again lands one ulp
             the wrong side of it — 1.0000000000000002 for the oblique ray here.
             That is two routes to one real number, which is not what this is
             about. A tenth of a per cent either side is: it is far finer than
             any flipped sign or swapped pairing could survive, and far coarser
             than the arithmetic. *)
          let step k = Vec.add centre (Vec.scale direction (t *. k)) in
          Alcotest.(check bool)
            (Printf.sprintf "%s: a step just past it crosses" name)
            true
            (Room.segments_cross ~a1:centre ~a2:(step 1.001) ~b1:w.Room.a
               ~b2:w.Room.b);
          Alcotest.(check bool)
            (Printf.sprintf "%s: a step stopping just short does not" name)
            false
            (Room.segments_cross ~a1:centre ~a2:(step 0.999) ~b1:w.Room.a
               ~b2:w.Room.b))
        hits)
    [
      ("east", Vec.make 1. 0.);
      ("north", Vec.make 0. (-1.));
      ("diagonal", Vec.make 1. 1.);
      ("oblique", Vec.make 0.37 (-0.91));
      ("shallow", Vec.make 1. 0.04);
      ("steep", Vec.make (-0.06) 1.);
    ];
  Alcotest.(check bool)
    (Printf.sprintf "every ray met a wall to check (%d)" !checked)
    true (!checked >= 6)

(* The parallel test is the sine of the angle between ray and wall, so it holds
   at every length. Scaled by neither, it would have been an area, and a wall
   short enough would have failed it head-on and gone unrendered — while
   [Room.passable] went on colliding with it, which is the one pairing a wall is
   never allowed to have. A tenth of a picometre is well past anything worth
   authoring; the point is that no length is short enough to break the test. *)
let a_wall_far_shorter_than_a_pixel_is_still_found () =
  let sliver =
    Room.wall ~height:3. ~material:dim
      (Vec.make 3. (2. -. 5e-14))
      (Vec.make 3. (2. +. 5e-14))
  in
  let room =
    Room.make ~floor:flat_floor ~ceiling:flat_ceiling
      (List.init (Room.wall_count room) (Room.wall_at room) @ [ sliver ])
  in
  let hit = nearest_hit room ~origin:centre ~direction:(Vec.make 1. 0.) in
  Alcotest.(check bool)
    "the ray finds it rather than passing through" true
    (hit.Ray.wall.Room.material == dim);
  Alcotest.check close "and at the distance it stands at" 1. hit.Ray.distance;
  (* The half the renderer answers now agrees with the half collision always
     did: both say something is there. *)
  Alcotest.(check bool)
    "and collision agrees there is something there" false
    (Room.passable room ~from:centre ~dest:(Vec.make 3.5 2.))

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
          case "openings are found like walls" openings_are_found_like_walls;
        ] );
      ("texturing", [ case "along stays on the wall" along_stays_on_the_wall ]);
      ( "edge cases",
        [
          case "origin on a wall" origin_on_a_wall;
          case "a wall far shorter than a pixel is still found"
            a_wall_far_shorter_than_a_pixel_is_still_found;
          case "a cast agrees with a step about where a wall is"
            a_cast_agrees_with_a_step_about_where_a_wall_is;
        ] );
    ]
