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
    Room.decal ~along:1. ~z:1. ~half_width:0.5 ~half_height:0.5 poster
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

(* [decal_column] indexes the image by its width and [decal_row] by its height,
   and between them they are the only statement of "is this point on that
   decal" — the renderer and Sight both read them, so what can be picked stays
   exactly what is drawn. A square image would agree with itself under the two
   swapped over, so the picture here is deliberately not square: 12 wide and 3
   high, hung in a space four times as wide as it is tall. *)
let a_decal_is_indexed_by_width_across_and_height_down () =
  let image = Image.make ~height:3 12 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
  let dec =
    Room.decal ~along:2. ~z:1. ~half_width:2. ~half_height:0.5 image
  in
  let column at = Room.decal_column dec ~seen_from:Room.Front ~along:at
  and row at = Room.decal_row dec ~above:at in
  Alcotest.(check (option int)) "the left edge is column 0" (Some 0) (column 0.);
  Alcotest.(check (option int))
    "the middle is the middle of the width" (Some 6) (column 2.);
  Alcotest.(check (option int))
    "and the right edge is the last column" (Some 11) (column 4.);
  Alcotest.(check (option int)) "past the end is off the decal" None (column 4.5);
  Alcotest.(check (option int)) "and before the start too" None (column (-0.5));
  (* Rows run down from the top of the decal, so the highest point is row 0. *)
  Alcotest.(check (option int)) "the top is row 0" (Some 0) (row 1.5);
  Alcotest.(check (option int))
    "the bottom is the last row" (Some 2)
    (row 0.5);
  Alcotest.(check (option int)) "and above it is off the decal" None (row 1.6);
  (* The point of the whole test: 12 is not 3. A version indexing rows by the
     width would answer 8 rather than 2 for the bottom, and clamp. *)
  Alcotest.(check bool)
    "the two extents are not the same number" true
    (image.Image.width <> image.Image.height)

(* Which side of a wall a point is on, and the claim the whole facing rule rests
   on: for a room wound the way every room here is wound, Front is the inside.
   [Vec.perp] is a quarter turn to the left and a counter-clockwise boundary
   keeps its interior on the left, so the normal points in. Nothing else in the
   engine states that, and an author writing a decal is trusting it. *)
let front_is_the_side_the_normal_points_to () =
  let w = Room.wall ~height:2. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.) in
  Alcotest.check vec "this wall's normal points at +y" (Vec.make 0. 1.)
    w.Room.normal;
  Alcotest.(check bool)
    "so a point above it is at the front" true
    (Room.side_of w (Vec.make 2. 1.) = Room.Front);
  Alcotest.(check bool)
    "and one below it at the back" true
    (Room.side_of w (Vec.make 2. (-1.)) = Room.Back);
  (* The load-bearing one: [Support.room] is the four walls of a square given
     counter-clockwise, and its centre is at the front of every one of them. *)
  Alcotest.(check bool)
    "the inside of a counter-clockwise room is Front of all four walls" true
    (Array.for_all (fun w -> Room.side_of w centre = Room.Front) room.Room.walls);
  (* Which is what makes [Front] the right default: it is where you stand. *)
  Alcotest.(check bool)
    "and a decal says so unless told otherwise" true
    ((Room.decal ~along:1. ~z:1. ~half_width:0.5 ~half_height:0.5 poster)
       .Room.facing
    = Room.Front)

(* A mark is on one face. Asked from the other, the rule that says where it is
   says it is nowhere — and it is [decal_column] that says so, the same call
   that decides whether the point is within its width, so the renderer and Sight
   cannot disagree about it. *)
let a_decal_is_only_on_the_face_it_was_drawn_on () =
  let front = Room.decal ~along:2. ~z:1. ~half_width:1. ~half_height:1. poster
  and back =
    Room.decal ~facing:Room.Back ~along:2. ~z:1. ~half_width:1. ~half_height:1.
      poster
  in
  let at d side = Room.decal_column d ~seen_from:side ~along:2. in
  Alcotest.(check bool)
    "the front one, from the front" true
    (at front Room.Front <> None);
  Alcotest.(check (option int))
    "the front one, from the back" None (at front Room.Back);
  Alcotest.(check bool)
    "the back one, from the back" true
    (at back Room.Back <> None);
  Alcotest.(check (option int))
    "the back one, from the front" None (at back Room.Front);
  (* And the vertical half knows nothing about faces, which is the point of
     putting the test in the horizontal one: it is asked once per column rather
     than once per pixel. *)
  Alcotest.(check bool)
    "the row is answered either way" true
    (Room.decal_row back ~above:1. <> None)

(* Marking a wall at run time: the decal goes on the end of that wall's list,
   which is where the topmost one is, and nothing else about the room moves. *)
let a_decal_can_be_added_to_a_wall () =
  let before =
    Room.make ~floor:flat_floor ~ceiling:flat_ceiling
      (Array.to_list room.Room.walls)
  in
  let mark along =
    Room.decal ~along ~z:1.2 ~half_width:0.3 ~half_height:0.3 poster
  in
  let after = Room.add_decal before ~wall:1 (mark 1.) in
  let twice = Room.add_decal after ~wall:1 (mark 3.) in
  Alcotest.(check int)
    "the wall asked for has it" 1
    (List.length after.Room.walls.(1).Room.decals);
  Alcotest.(check int)
    "and no other wall does" 0
    (List.length after.Room.walls.(0).Room.decals);
  Alcotest.(check int) "a second goes on too" 2
    (List.length twice.Room.walls.(1).Room.decals);
  Alcotest.check close "and on the end, which is the top of the pile" 3.
    (List.nth twice.Room.walls.(1).Room.decals 1).Room.along;
  (* Nothing else is rebuilt: the other three walls are the very same values,
     and the room it came from never gained anything. *)
  Alcotest.(check bool)
    "the untouched walls are the same values" true
    (after.Room.walls.(0) == before.Room.walls.(0)
    && after.Room.walls.(2) == before.Room.walls.(2)
    && after.Room.walls.(3) == before.Room.walls.(3));
  Alcotest.(check bool)
    "and both planes" true
    (after.Room.floor == before.Room.floor
    && after.Room.ceiling == before.Room.ceiling);
  Alcotest.(check int)
    "the room it was added to is unchanged" 0
    (List.length before.Room.walls.(1).Room.decals)

(* The sprite half of the same idea. [sprite_column] and [sprite_row] are what
   Viewport.sprite_box is built from and what Sight.touches asks, so they are
   where a billboard's width and its two vertical bounds are decided once. The
   picture is again deliberately not square. *)
let a_sprite_is_indexed_by_width_across_and_height_down () =
  let image = Image.make ~height:4 16 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
  let s = Room.sprite ~size:1. ~image (Vec.make 0. 0.) in
  (* Four times as wide as it is tall, so a sprite one cell tall is four cells
     across and reaches two either side. *)
  Alcotest.check close "half a width" 2. (Room.sprite_half_width s);
  let column at = Room.sprite_column s ~lateral:at in
  Alcotest.(check (option int)) "the left edge is column 0" (Some 0) (column (-2.));
  Alcotest.(check (option int)) "the centre is the middle" (Some 8) (column 0.);
  Alcotest.(check (option int))
    "and the right edge the last column" (Some 15) (column 2.);
  Alcotest.(check (option int)) "past its width is off it" None (column 2.1);
  Alcotest.(check (option int)) "and before it too" None (column (-2.1));
  (* Rows run down from the head, so the top of the sprite is row 0. *)
  let row at = Room.sprite_row s ~floor_z:0. ~z:at in
  Alcotest.(check (option int)) "the head is row 0" (Some 0) (row 1.);
  Alcotest.(check (option int)) "the foot is the last row" (Some 3) (row 0.);
  Alcotest.(check (option int)) "above its head is off it" None (row 1.1);
  Alcotest.(check (option int)) "and below its foot too" None (row (-0.1));
  Alcotest.(check bool)
    "the two extents are not the same number" true
    (image.Image.width <> image.Image.height)

(* A base lifts both bounds and nothing else, and it is measured from the floor
   given rather than from zero — the same rule a decal's [z] follows. *)
let a_base_lifts_a_sprite_off_the_floor_it_is_given () =
  let image = Image.make 8 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
  let ground = Room.sprite ~size:2. ~image (Vec.make 0. 0.)
  and lifted = Room.sprite ~base:1.5 ~size:2. ~image (Vec.make 0. 0.) in
  Alcotest.check close "on the floor, the foot is the floor" 0.
    (Room.sprite_foot ground ~floor_z:0.);
  Alcotest.check close "and the head a size above" 2.
    (Room.sprite_head ground ~floor_z:0.);
  Alcotest.check close "lifted, the foot is the base" 1.5
    (Room.sprite_foot lifted ~floor_z:0.);
  Alcotest.check close "and the head still a size above that" 3.5
    (Room.sprite_head lifted ~floor_z:0.);
  (* Over a floor that has climbed, both move with it. On a slope a sprite
     rides the ground rather than the ground riding through it. *)
  Alcotest.check close "the floor carries the foot" 2.5
    (Room.sprite_foot lifted ~floor_z:1.);
  Alcotest.check close "and the head" 4.5
    (Room.sprite_head lifted ~floor_z:1.);
  (* Which is the same thing said twice: raising the floor and raising the base
     put the picture in the same place. *)
  Alcotest.(check (option int))
    "so a point on one is the same row of the other"
    (Room.sprite_row lifted ~floor_z:0. ~z:2.)
    (Room.sprite_row ground ~floor_z:1.5 ~z:2.);
  Alcotest.check close "a sprite with no base starts on the ground" 0.
    ground.Room.base

(* Sprites are the only part of a room that a game changes every frame, so the
   cheap way of doing it has to keep everything else exactly as it was — not
   equal to it, the same. *)
let replacing_the_sprites_keeps_the_rest () =
  let image = Image.make 8 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
  let before =
    Room.make ~floor:flat_floor ~ceiling:flat_ceiling
      ~sprites:[ Room.sprite ~size:1. ~image (Vec.make 1. 1.) ]
      (Array.to_list room.Room.walls)
  in
  let after =
    Room.with_sprites before
      [
        Room.sprite ~base:2. ~size:1. ~image (Vec.make 1. 1.);
        Room.sprite ~size:1. ~image (Vec.make 2. 2.);
      ]
  in
  Alcotest.(check int) "the new sprites are there" 2
    (Array.length after.Room.sprites);
  Alcotest.check close "and are the ones asked for" 2.
    after.Room.sprites.(0).Room.base;
  Alcotest.(check bool)
    "the walls are the very same array" true
    (after.Room.walls == before.Room.walls);
  Alcotest.(check bool)
    "so are the thresholds" true
    (after.Room.thresholds == before.Room.thresholds);
  Alcotest.(check bool)
    "and both planes" true
    (after.Room.floor == before.Room.floor
    && after.Room.ceiling == before.Room.ceiling);
  Alcotest.(check int)
    "the room it came from is untouched" 1
    (Array.length before.Room.sprites)

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
          case "a decal is indexed by width across and height down"
            a_decal_is_indexed_by_width_across_and_height_down;
          case "distance to a wall" distance_to_a_wall;
        ] );
      ( "faces",
        [
          case "front is the side the normal points to"
            front_is_the_side_the_normal_points_to;
          case "a decal is only on the face it was drawn on"
            a_decal_is_only_on_the_face_it_was_drawn_on;
          case "a decal can be added to a wall" a_decal_can_be_added_to_a_wall;
        ] );
      ( "sprites",
        [
          case "a sprite is indexed by width across and height down"
            a_sprite_is_indexed_by_width_across_and_height_down;
          case "a base lifts a sprite off the floor it is given"
            a_base_lifts_a_sprite_off_the_floor_it_is_given;
          case "replacing the sprites keeps the rest"
            replacing_the_sprites_keeps_the_rest;
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
