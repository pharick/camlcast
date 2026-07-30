open Camlcast
open Support

let a_wall_precomputes_its_geometry () =
  let w =
    Room.wall ~height:2. ~material:pale (Vec.make 1. 1.) (Vec.make 4. 5.)
  in
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
  let image =
    Image.make ~height:3 ~width:12 (fun ~u ~v -> (Color.rgb u v 0, 255))
  in
  let dec = Room.decal ~along:2. ~z:1. ~half_width:2. ~half_height:0.5 image in
  let column at = Room.decal_column dec ~seen_from:Room.Front ~along:at
  and row at = Room.decal_row dec ~above:at in
  Alcotest.(check (option int)) "the left edge is column 0" (Some 0) (column 0.);
  Alcotest.(check (option int))
    "the middle is the middle of the width" (Some 6) (column 2.);
  Alcotest.(check (option int))
    "and the right edge is the last column" (Some 11) (column 4.);
  Alcotest.(check (option int))
    "past the end is off the decal" None (column 4.5);
  Alcotest.(check (option int)) "and before the start too" None (column (-0.5));
  (* Rows run down from the top of the decal, so the highest point is row 0. *)
  Alcotest.(check (option int)) "the top is row 0" (Some 0) (row 1.5);
  Alcotest.(check (option int)) "the bottom is the last row" (Some 2) (row 0.5);
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
  let w =
    Room.wall ~height:2. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.)
  in
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
    (List.for_all
       (fun w -> Room.side_of w centre = Room.Front)
       (List.init (Room.wall_count room) (Room.wall_at room)));
  (* Which is what makes [Front] the right default: it is where you stand. *)
  Alcotest.(check bool)
    "and a decal says so unless told otherwise" true
    ((Room.decal ~along:1. ~z:1. ~half_width:0.5 ~half_height:0.5 poster)
       .Room.facing = Room.Front)

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
      (List.init (Room.wall_count room) (Room.wall_at room))
  in
  let mark along =
    Room.decal ~along ~z:1.2 ~half_width:0.3 ~half_height:0.3 poster
  in
  let after = Room.add_decal before ~wall:1 (mark 1.) in
  let twice = Room.add_decal after ~wall:1 (mark 3.) in
  Alcotest.(check int)
    "the wall asked for has it" 1
    (List.length (Room.wall_at after 1).Room.decals);
  Alcotest.(check int)
    "and no other wall does" 0
    (List.length (Room.wall_at after 0).Room.decals);
  Alcotest.(check int)
    "a second goes on too" 2
    (List.length (Room.wall_at twice 1).Room.decals);
  Alcotest.check close "and on the end, which is the top of the pile" 3.
    (List.nth (Room.wall_at twice 1).Room.decals 1).Room.along;
  (* Nothing else is rebuilt: the other three walls are the very same values,
     and the room it came from never gained anything. *)
  Alcotest.(check bool)
    "the untouched walls are the same values" true
    (Room.wall_at after 0 == Room.wall_at before 0
    && Room.wall_at after 2 == Room.wall_at before 2
    && Room.wall_at after 3 == Room.wall_at before 3);
  Alcotest.(check bool)
    "and both planes" true
    (Room.floor_surface after == Room.floor_surface before
    && Room.ceiling after == Room.ceiling before);
  Alcotest.(check int)
    "the room it was added to is unchanged" 0
    (List.length (Room.wall_at before 1).Room.decals)

(* The sprite half of the same idea. [sprite_column] and [sprite_row] are what
   Viewport.sprite_box is built from and what Sight.touches asks, so they are
   where a billboard's width and its two vertical bounds are decided once. The
   picture is again deliberately not square. *)
let a_sprite_is_indexed_by_width_across_and_height_down () =
  let image =
    Image.make ~height:4 ~width:16 (fun ~u ~v -> (Color.rgb u v 0, 255))
  in
  let s = Room.sprite ~size:1. ~image (Vec.make 0. 0.) in
  (* Four times as wide as it is tall, so a sprite one cell tall is four cells
     across and reaches two either side. *)
  Alcotest.check close "half a width" 2. (Room.sprite_half_width s);
  let column at = Room.sprite_column s ~lateral:at in
  Alcotest.(check (option int))
    "the left edge is column 0" (Some 0) (column (-2.));
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
  let image = Image.make ~width:8 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
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
  Alcotest.check close "and the head" 4.5 (Room.sprite_head lifted ~floor_z:1.);
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
  let image = Image.make ~width:8 (fun ~u ~v -> (Color.rgb u v 0, 255)) in
  let before =
    Room.make ~floor:flat_floor ~ceiling:flat_ceiling
      ~sprites:[ Room.sprite ~size:1. ~image (Vec.make 1. 1.) ]
      (List.init (Room.wall_count room) (Room.wall_at room))
  in
  let after =
    Room.with_sprites before
      [
        Room.sprite ~base:2. ~size:1. ~image (Vec.make 1. 1.);
        Room.sprite ~size:1. ~image (Vec.make 2. 2.);
      ]
  in
  Alcotest.(check int) "the new sprites are there" 2 (Room.sprite_count after);
  Alcotest.check close "and are the ones asked for" 2.
    (Room.sprite_at after 0).Room.base;
  (* The walls are the very same values, one by one — which is what sharing
     means from outside, the array itself no longer being anybody's to compare.
     A [with_sprites] that rebuilt them would cost a {!Room.wall} per wall per
     frame, each one normalizing a vector to arrive back where it started. *)
  Alcotest.(check bool)
    "the walls are the very same values" true
    (Room.wall_count after = Room.wall_count before
    && List.for_all
         (fun i -> Room.wall_at after i == Room.wall_at before i)
         (List.init (Room.wall_count after) Fun.id));
  Alcotest.(check int)
    "and so are the thresholds, there being none either way" 0
    (Room.threshold_count after + Room.threshold_count before);
  Alcotest.(check bool)
    "and both planes" true
    (Room.floor_surface after == Room.floor_surface before
    && Room.ceiling after == Room.ceiling before);
  Alcotest.(check int)
    "the room it came from is untouched" 1 (Room.sprite_count before)

let distance_to_a_wall () =
  let w =
    Room.wall ~height:2. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.)
  in
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
    "the centre of the room is clear" false (Room.blocked room centre);
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
  let crosses b1 b2 = Room.segments_cross ~a1 ~a2 ~b1 ~b2 in
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

(* The parallel test is an area — both lengths times the sine between them — so
   scaled by neither it holds for a crossing at any angle, provided one of the
   two segments is short enough. The one that gets short is the step: movement
   clips a leg where it meets a doorway and asks again about the remainder,
   which lands here as a whisker. A tenth of a picometre is far past anything a
   player could walk on purpose; the point is that no length is short enough to
   break the test. *)
let a_step_far_shorter_than_a_pixel_still_crosses () =
  let b1 = Vec.make 2. 0. and b2 = Vec.make 2. 4. in
  let crosses a1 a2 = Room.segments_cross ~a1 ~a2 ~b1 ~b2 in
  let sliver = 5e-14 in
  Alcotest.(check bool)
    "a whisker of a step across a wall crosses it" true
    (crosses (Vec.make (2. -. sliver) 2.) (Vec.make (2. +. sliver) 2.));
  Alcotest.(check bool)
    "one of the same length stopping short of it does not" false
    (crosses (Vec.make (2. -. (3. *. sliver)) 2.) (Vec.make (2. -. sliver) 2.));
  Alcotest.(check bool)
    "and one taken along it still overlaps as collinear" true
    (crosses (Vec.make 2. 2.) (Vec.make 2. (2. +. sliver)))

let a_step_along_a_wall_is_refused () =
  let level =
    Room.make ~floor:flat_floor ~ceiling:flat_ceiling
      [ Room.wall ~height:2. ~material:pale (Vec.make 1. 0.) (Vec.make 2. 0.) ]
  in
  Alcotest.(check bool)
    "a step down the line of a wall does not pass through it" false
    (Room.passable level ~from:(Vec.make 0. 0.) ~dest:(Vec.make 3. 0.));
  Alcotest.(check bool)
    "the same step alongside it is free" true
    (Room.passable level ~from:(Vec.make 0. 0.5) ~dest:(Vec.make 3. 0.5))

(* Two segments that miss each other are as far apart as the nearest of their
   endpoints is from the other segment. *)
let distance_between_two_segments () =
  let d a1 a2 b1 b2 = Room.distance_between_segments ~a1 ~a2 ~b1 ~b2 in
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
    (Room.passable level
       ~from:(Vec.make (-2.) brushing_the_end)
       ~dest:(Vec.make 2. brushing_the_end));
  Alcotest.(check bool)
    "the same step given the end a wider berth is free" true
    (Room.passable level
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

(* A doorway is cut by dividing by the length of the wall it is cut into, so a
   wall of no length hands back a threshold whose every coordinate is [nan] —
   and [nan] is refused by nothing downstream, because every ordered comparison
   it is given answers false. A world would be built out of it, and its
   transform would be [nan] throughout. The other two are the same kind of
   mistake caught at the same moment: an opening of no width is not one, and one
   wider than its wall leaves the jambs wound backwards, which is the winding
   every transform derived from the opening depends on. *)
let a_doorway_that_could_not_be_cut_is_refused () =
  let raises what message body =
    Alcotest.check_raises what (Invalid_argument message) body
  in
  let cut ?(opening = 2.) ?(height = 3.) ~width a b =
   fun () ->
    ignore
      (Room.doorway ~name:"gate" ~width ~opening ~height ~material:pale a b)
  in
  let a = Vec.make 0. 0. and b = Vec.make 4. 0. in
  let no_wall = "Room.doorway: no wall to cut a doorway into: gate" in
  raises "a wall with no length" no_wall
    (cut ~width:1. (Vec.make 2. 2.) (Vec.make 2. 2.));
  (* An infinite end used to pass this and surface from [threshold] instead,
     under a name the caller never wrote. *)
  raises "a wall running off to infinity" no_wall
    (cut ~width:1. (Vec.make 0. 0.) (Vec.make Float.infinity 0.));
  raises "an opening with no width"
    "Room.doorway: a doorway has to have a width: gate" (cut ~width:0. a b);
  raises "wider than the wall"
    "Room.doorway: wider than the wall it is cut into: gate" (cut ~width:5. a b);
  raises "a wall that does not rise"
    "Room.doorway: the wall has to rise above the floor: gate"
    (cut ~width:1. ~height:0. a b);
  let no_fit = "Room.doorway: the opening has to fit under the wall: gate" in
  raises "an opening of no height" no_fit (cut ~width:1. ~opening:0. a b);
  raises "an opening taller than its wall" no_fit
    (cut ~width:1. ~opening:4. ~height:3. a b);
  raises "a nan opening" no_fit (cut ~width:1. ~opening:Float.nan a b);
  (* An opening exactly as tall as its wall is allowed, and leaves a lintel of
     no depth — the same way a doorway as wide as its wall leaves no jamb. *)
  let _, threshold =
    Room.doorway ~name:"gate" ~width:1. ~opening:3. ~height:3. ~material:pale a
      b
  in
  Alcotest.(check (float 1e-9))
    "an opening flush with the top of its wall is allowed" 3.
    threshold.Room.height

(* A doorway exactly as wide as the wall it is cut into is allowed — a whole
   side of a room that is one opening — and leaves nothing standing either side
   of it. Those ends are dropped rather than handed back as walls of no length,
   which {!Room.wall} refuses. *)
let a_doorway_as_wide_as_its_wall_leaves_no_jamb () =
  let jambs, t =
    Room.doorway ~name:"whole" ~width:4. ~opening:2. ~height:3. ~material:pale
      (Vec.make 0. 0.) (Vec.make 4. 0.)
  in
  Alcotest.(check int) "no jamb is left standing" 0 (List.length jambs);
  Alcotest.check close "the opening is the whole wall" 4. t.Room.length;
  Alcotest.check vec "from one end of it" (Vec.make 0. 0.) t.Room.a;
  Alcotest.check vec "to the other" (Vec.make 4. 0.) t.Room.b

(* The same argument as the doorway above, one type down. A decal of no width
   and a sprite of no size both survive being written and both fail later,
   inside a frame: {!Room.decal_column} divides by twice the half width,
   {!Room.sprite_half_width} divides by the picture's height, and
   {!Viewport.sprite_box} divides by the size. Every one of those answers [nan],
   and [nan] is refused by nothing downstream — so it is refused here. Each test
   is the negation of the passing condition, which is what catches the [nan]
   that was handed in rather than derived. *)
let a_decal_or_sprite_of_no_size_is_refused () =
  let raises what message body =
    Alcotest.check_raises what (Invalid_argument message) body
  in
  let poster =
    Image.make ~width:4 (fun ~u:_ ~v:_ -> (Color.rgb 200 200 200, 255))
  in
  let mark ?glow ~half_width ~half_height () =
   fun () ->
    ignore (Room.decal ?glow ~along:1. ~z:1. ~half_width ~half_height poster)
  in
  raises "a decal with no width" "Room.decal: a decal has to have a width"
    (mark ~half_width:0. ~half_height:1. ());
  raises "a decal with a nan width" "Room.decal: a decal has to have a width"
    (mark ~half_width:Float.nan ~half_height:1. ());
  raises "a decal with no height" "Room.decal: a decal has to have a height"
    (mark ~half_width:1. ~half_height:(-1.) ());
  raises "glow over one" "Room.decal: glow is a fraction from 0 to 1"
    (mark ~glow:1.5 ~half_width:1. ~half_height:1. ());
  raises "glow under zero" "Room.decal: glow is a fraction from 0 to 1"
    (mark ~glow:(-0.5) ~half_width:1. ~half_height:1. ());
  (* Both ends of the range are in it, since paint and phosphorescence are the
     two decals anyone writes down on purpose. *)
  List.iter
    (fun glow ->
      ignore
        (Room.decal ~glow ~along:1. ~z:1. ~half_width:1. ~half_height:1. poster))
    [ 0.; 1. ];
  List.iter
    (fun size ->
      raises (Printf.sprintf "a sprite of size %f" size)
        "Room.sprite: a sprite has to have a size" (fun () ->
          ignore (Room.sprite ~size ~image:poster (Vec.make 0. 0.))))
    [ 0.; -1.; Float.nan ]

let opening ?door () =
  snd
    (Room.doorway ~name:"a" ?door ~width:1. ~opening:2. ~height:3.
       ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.))

let a_doorway_can_hang_a_door () =
  let bare = opening () and hung = opening ~door:(Door.make mesh) () in
  Alcotest.(check bool) "a bare opening has no leaf" true (bare.Room.door = None);
  Alcotest.(check bool)
    "a door names its material" true
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

(* [with_thresholds] is public, so a game can reach it even though a room cannot
   be built by hand. A room outlives the call that made it and the array handed
   in does not have to, so the room takes its own copy: otherwise a threshold
   moved afterwards would slip past {!World.replace_room}, which matches a
   doorway by where it is, and leave a portal describing an opening that is no
   longer there. *)
let with_thresholds_keeps_a_copy () =
  let jambs, threshold =
    Room.doorway ~name:"gate" ~width:1. ~opening:2. ~height:3. ~material:pale
      (Vec.make 0. 0.) (Vec.make 4. 0.)
  in
  let before =
    Room.make ~thresholds:[ threshold ] ~floor:flat_floor ~ceiling:flat_ceiling
      jambs
  in
  let handed =
    Array.init (Room.threshold_count before) (Room.threshold_at before)
  in
  let after = Room.with_thresholds before handed in
  (* The room being abstract is what puts its own array out of reach; what this
     has to show is the other half, that the one handed in was not adopted.
     Writing into it afterwards is the whole test, and a sharper one than
     comparing the two arrays would have been. *)
  handed.(0) <-
    snd
      (Room.doorway ~name:"elsewhere" ~width:1. ~opening:2. ~height:3.
         ~material:pale (Vec.make 0. 4.) (Vec.make 4. 4.));
  Alcotest.(check string)
    "writing into the array afterwards changes nothing" "gate"
    (Room.threshold_at after 0).Room.name;
  Alcotest.(check bool)
    "the walls are still shared, as everything unreplaced is" true
    (Room.wall_count after = Room.wall_count before
    && List.for_all
         (fun i -> Room.wall_at after i == Room.wall_at before i)
         (List.init (Room.wall_count after) Fun.id))

(* The constructors and the accessors are two ends of the same statement: what
   goes in through [floor]/[roof]/[open_sky] is what comes back out of the
   reads, with the match on the ceiling variant written once, in the library. *)
let bounds_read_back_through_the_accessors () =
  let ground = Plane.horizontal 0. in
  let lid = Plane.above ground 3. in
  let walls =
    Room.rectangle ~height:3. ~material:pale (Vec.make (-2.) (-2.))
      (Vec.make 2. 2.)
  in
  let boxed =
    Room.make
      ~floor:(Room.floor ~plane:ground ~material:pale)
      ~ceiling:(Room.roof ~plane:lid ~material:mesh)
      walls
  in
  Alcotest.(check bool)
    "the floor's plane reads back" true
    (Room.floor_plane boxed = ground);
  Alcotest.(check bool)
    "and its material" true
    (Room.floor_material boxed == pale);
  Alcotest.(check bool)
    "a roofed room has a ceiling plane" true
    (Room.ceiling_plane boxed = Some lid);
  Alcotest.(check bool) "and no sky" true (Room.sky boxed = None);
  let yard =
    Room.make
      ~floor:(Room.floor ~plane:ground ~material:pale)
      ~ceiling:(Room.open_sky Sky.default)
      walls
  in
  Alcotest.(check bool)
    "an open room says which sky" true
    (Room.sky yard = Some Sky.default);
  Alcotest.(check bool)
    "and has no roof to read" true
    (Room.ceiling_surface yard = None && Room.ceiling_plane yard = None)

(* The one authoring mistake a box invites is winding it backwards. The
   constructor's whole promise is that it cannot be made through it, whichever
   two opposite corners arrive, in whichever order. *)
let a_rectangle_is_wound_inward_from_either_corner_pair () =
  let centre = Vec.make 1. 1. in
  let check_walls what walls =
    Alcotest.(check int) (what ^ " has four walls") 4 (List.length walls);
    List.iter
      (fun w ->
        Alcotest.(check bool)
          (what ^ ": the centre is on the front")
          true
          (Room.side_of w centre = Room.Front))
      walls
  in
  check_walls "low corner first"
    (Room.rectangle ~height:2. ~material:pale (Vec.make 0. 0.) (Vec.make 2. 2.));
  check_walls "high corner first"
    (Room.rectangle ~height:2. ~material:pale (Vec.make 2. 2.) (Vec.make 0. 0.));
  check_walls "the other diagonal"
    (Room.rectangle ~height:2. ~material:pale (Vec.make 0. 2.) (Vec.make 2. 0.))

let a_flat_rectangle_is_refused () =
  let raises what corner =
    Alcotest.check_raises what
      (Invalid_argument "Room.rectangle: the corners have to span an area")
      (fun () ->
        ignore
          (Room.rectangle ~height:2. ~material:pale (Vec.make 0. 0.) corner))
  in
  raises "no width" (Vec.make 0. 3.);
  raises "no height" (Vec.make 3. 0.);
  raises "no extent at all" (Vec.make 0. 0.);
  raises "a nan corner" (Vec.make Float.nan 2.);
  (* Under [rectangle]'s own name, not [wall]'s or [path]'s — the caller wrote
     this one. *)
  let raises what height =
    Alcotest.check_raises what
      (Invalid_argument "Room.rectangle: the walls have to rise above the floor")
      (fun () ->
        ignore
          (Room.rectangle ~height ~material:pale (Vec.make 0. 0.)
             (Vec.make 3. 3.)))
  in
  raises "walls of no height" 0.;
  raises "walls below the floor" (-2.);
  raises "a nan height" Float.nan

(* A wall between two points that are the same has a normal [Vec.normalize]
   could not scale, and it hands one it cannot scale straight back — so the
   normal is neither a unit vector nor perpendicular to anything, which is what
   [side_of] and [Atmosphere.face_shading] read. Nothing downstream objects
   either: [Ray.cast] finds no intersection with an edge of no length, so the
   wall is invisible, while [distance_to_wall] degrades to a point and blocks
   whoever walks into it. A threshold is the same mistake with more resting on
   it, its normal being what [Transform.between] turns into a portal's frame
   change. Both tests are the negation of the passing condition, so an end that
   is [nan] or infinite fails with the coincident ones — a length folds every
   bad coordinate into one number. *)
let a_wall_or_threshold_with_no_length_is_refused () =
  let raises what message body =
    Alcotest.check_raises what (Invalid_argument message) body
  in
  let wall a b () = ignore (Room.wall ~height:3. ~material:pale a b) in
  let threshold a b () = ignore (Room.threshold ~name:"gate" ~height:2. a b) in
  let no_length = "Room.wall: the two ends have to be apart" in
  raises "the same point twice" no_length
    (wall (Vec.make 2. 2.) (Vec.make 2. 2.));
  raises "a nan end" no_length (wall (Vec.make 0. 0.) (Vec.make Float.nan 1.));
  raises "an infinite end" no_length
    (wall (Vec.make 0. 0.) (Vec.make Float.infinity 1.));
  let no_length = "Room.threshold: the two ends have to be apart: gate" in
  raises "a threshold of no length" no_length
    (threshold (Vec.make 2. 2.) (Vec.make 2. 2.));
  raises "a threshold with a nan end" no_length
    (threshold (Vec.make 0. 0.) (Vec.make 0. Float.nan))

(* The same invisible blocker reached from the other end of the wall. The
   renderer takes a wall's top to be the floor plus its height and draws nothing
   unless that clears the floor, while [blocked] and [passable] never read the
   height at all — so a wall that does not rise is walked into and never seen.
   A threshold that does not rise is worse still: [World.passable] is flat and
   never consults the height, so the opening is walked through while the
   renderer draws it as a sliver or as nothing. Negated, so [nan] and infinity
   fail with the flat ones. *)
let a_wall_or_threshold_that_does_not_rise_is_refused () =
  let raises what message body =
    Alcotest.check_raises what (Invalid_argument message) body
  in
  let a = Vec.make 0. 0. and b = Vec.make 4. 0. in
  let wall height () = ignore (Room.wall ~height ~material:pale a b) in
  let threshold height () = ignore (Room.threshold ~name:"gate" ~height a b) in
  let flat = "Room.wall: the wall has to rise above the floor" in
  raises "no height at all" flat (wall 0.);
  raises "a height below the floor" flat (wall (-1.));
  raises "a nan height" flat (wall Float.nan);
  raises "an infinite height" flat (wall Float.infinity);
  let flat = "Room.threshold: the opening has to rise above the floor: gate" in
  raises "an opening of no height" flat (threshold 0.);
  raises "an opening below the floor" flat (threshold (-2.));
  raises "a nan opening" flat (threshold Float.nan)

(* A lintel is the strip of wall above an opening, so one whose top does not
   reach the opening it stands over describes a wall that cannot be drawn.
   [doorway] cannot produce one — it takes the wall's own height for the
   lintel and refuses an opening taller than it — so this is about the
   threshold built by hand. *)
let a_lintel_below_its_opening_is_refused () =
  let raises what top =
    Alcotest.check_raises what
      (Invalid_argument
         "Room.threshold: the lintel has to sit above the opening: gate")
      (fun () ->
        ignore
          (Room.threshold ~name:"gate" ~height:2.
             ~lintel:{ Room.top; material = pale }
             (Vec.make 0. 0.) (Vec.make 4. 0.)))
  in
  raises "a lintel below its opening" 1.;
  raises "a lintel at the floor" 0.;
  raises "a nan lintel" Float.nan;
  let flush =
    Room.threshold ~name:"gate" ~height:2.
      ~lintel:{ Room.top = 2.; material = pale }
      (Vec.make 0. 0.) (Vec.make 4. 0.)
  in
  Alcotest.(check bool)
    "a lintel flush with the top of its opening is allowed" true
    (Option.is_some flush.Room.lintel)

(* And hanging one afterwards is held to the same terms. A threshold is private
   so that it carries one invariant rather than one per route to it, and
   [with_lintel] is a route to it: an opening built with a lintel the
   constructor refused could otherwise be given that lintel a line later. The
   message names [with_lintel] because that is what was called. *)
let with_lintel_is_refused_the_same_lintel () =
  let gate =
    Room.threshold ~name:"gate" ~height:2. (Vec.make 0. 0.) (Vec.make 4. 0.)
  in
  let raises what top =
    Alcotest.check_raises what
      (Invalid_argument
         "Room.with_lintel: the lintel has to sit above the opening: gate")
      (fun () ->
        ignore (Room.with_lintel gate (Some { Room.top; material = pale })))
  in
  raises "a lintel below its opening" 1.;
  raises "a lintel at the floor" 0.;
  raises "a nan lintel" Float.nan;
  raises "an infinite lintel" Float.infinity;
  let flush = Room.with_lintel gate (Some { Room.top = 2.; material = pale })
  and none = Room.with_lintel gate None in
  Alcotest.(check bool)
    "one flush with the top of its opening is allowed" true
    (Option.is_some flush.Room.lintel);
  Alcotest.(check bool)
    "and taking the lintel away asks nothing at all" true
    (none.Room.lintel = None)

(* Shutting a closed loop by repeating the first point at the end is the natural
   way to write one down, and exactly the mistake: [closed] already joins them,
   so the repeated pair is a wall of no length. Refused under [path]'s own name
   rather than left to [wall], because the wall it would have built is one the
   caller never wrote. *)
let a_path_that_stands_still_is_refused () =
  let raises what ~closed points =
    Alcotest.check_raises what
      (Invalid_argument "Room.path: two points in a row are the same")
      (fun () -> ignore (Room.path ~closed ~height:1. ~material:pale points))
  in
  let a = Vec.make 0. 0. and b = Vec.make 1. 0. and c = Vec.make 1. 1. in
  raises "a point repeated in the middle" ~closed:false [ a; b; b; c ];
  raises "a closed loop shut by hand" ~closed:true [ a; b; c; a ];
  raises "the wrap of a closed run" ~closed:true [ a; b; c; c ];
  raises "a nan point" ~closed:false [ a; Vec.make Float.nan 0.; c ];
  Alcotest.check_raises "a closed run of two points"
    (Invalid_argument
       "Room.path: a closed path has to have at least three points") (fun () ->
      ignore (Room.path ~closed:true ~height:1. ~material:pale [ a; b ]));
  Alcotest.(check int)
    "an open run too short to have a wall in it is empty" 0
    (List.length (Room.path ~height:1. ~material:pale [ a ]));
  (* Under [path]'s own name for the same reason the repeated point is. *)
  let raises what height =
    Alcotest.check_raises what
      (Invalid_argument "Room.path: the walls have to rise above the floor")
      (fun () -> ignore (Room.path ~height ~material:pale [ a; b; c ]))
  in
  raises "walls of no height" 0.;
  raises "walls below the floor" (-1.);
  raises "a nan height" Float.nan

(* Two sides are a pair of coincident walls wound against each other, one is a
   wall of no length, none is a room with no boundary at all, and a negative
   count would reach [List.init] and be refused under its name rather than this
   one. A radius of nothing stacks every side on the center. All four are the
   negation of what would pass, so a [nan] argument fails with the flat ones. *)
let a_polygon_that_is_not_one_is_refused () =
  let polygon ?(center = Vec.make 0. 0.) ?(radius = 2.) ?(sides = 6)
      ?(rotation = 0.) ?(height = 3.) () =
   fun () ->
    ignore
      (Room.regular_polygon ~center ~radius ~sides ~rotation ~height
         ~material:pale)
  in
  let raises what message body =
    Alcotest.check_raises what (Invalid_argument message) body
  in
  let too_few =
    "Room.regular_polygon: a polygon has to have at least three sides"
  in
  raises "two sides" too_few (polygon ~sides:2 ());
  raises "one side" too_few (polygon ~sides:1 ());
  raises "no sides" too_few (polygon ~sides:0 ());
  raises "fewer than none" too_few (polygon ~sides:(-1) ());
  let no_radius = "Room.regular_polygon: a polygon has to have a radius" in
  raises "no radius" no_radius (polygon ~radius:0. ());
  raises "a nan radius" no_radius (polygon ~radius:Float.nan ());
  raises "a nan rotation"
    "Room.regular_polygon: the rotation has to be a number"
    (polygon ~rotation:Float.nan ());
  raises "a nan center" "Room.regular_polygon: the center has to be a point"
    (polygon ~center:(Vec.make Float.nan 0.) ());
  (* Under [regular_polygon]'s own name, not the [path] it delegates to. *)
  let flat = "Room.regular_polygon: the walls have to rise above the floor" in
  raises "walls of no height" flat (polygon ~height:0. ());
  raises "walls below the floor" flat (polygon ~height:(-3.) ());
  raises "a nan height" flat (polygon ~height:Float.nan ())

(* [across] is [Transform.between] with the endpoint pairing already right —
   the same pairing {!World.make} uses for a link, which is the fact a game
   authoring a neighbour's floor depends on. *)
let across_is_the_link_transform () =
  let t1 = Room.threshold ~name:"a" ~height:2. (Vec.make 1. 0.) (Vec.make 3. 0.)
  and t2 =
    Room.threshold ~name:"b" ~height:2. (Vec.make 7. 4.) (Vec.make 7. 6.)
  in
  Alcotest.(check bool)
    "the same transform as spelling out the endpoints" true
    (Room.across t1 t2
    = Transform.between ~a1:t1.Room.a ~a2:t1.Room.b ~b1:t2.Room.a ~b2:t2.Room.b
    )

let doorway_in ~name ?door a b =
  snd
    (Room.doorway ~name ?door ~width:1. ~opening:2. ~height:3. ~material:pale a
       b)

(* Nearest-wins, gated by [where] and cut off by [within] — the whole of
   "press the key at the door in front of you". *)
let nearest_threshold_picks_the_nearest_that_qualifies () =
  let near = doorway_in ~name:"near" (Vec.make 0. 0.) (Vec.make 4. 0.)
  and far =
    doorway_in ~name:"far" ~door:(Door.make mesh) (Vec.make 0. 8.)
      (Vec.make 4. 8.)
  in
  let room =
    Room.make ~thresholds:[ near; far ] ~floor:flat_floor ~ceiling:flat_ceiling
      (Room.rectangle ~height:3. ~material:pale (Vec.make 0. 0.)
         (Vec.make 4. 8.))
  in
  let at = Vec.make 2. 1. in
  Alcotest.(check (option int))
    "the nearer threshold wins" (Some 0)
    (Room.nearest_threshold room at);
  Alcotest.(check (option int))
    "where filters — only the far one has a door" (Some 1)
    (Room.nearest_threshold ~where:(fun t -> t.Room.door <> None) room at);
  Alcotest.(check (option int))
    "within cuts the far one off" None
    (Room.nearest_threshold ~within:3.
       ~where:(fun t -> t.Room.door <> None)
       room at);
  Alcotest.(check (option int))
    "a room with no thresholds answers none" None
    (Room.nearest_threshold
       (Room.make ~floor:flat_floor ~ceiling:flat_ceiling
          (Room.rectangle ~height:3. ~material:pale (Vec.make 0. 0.)
             (Vec.make 4. 4.)))
       at)

(* The three functions that stand where a record update used to: each touches
   the field it names and shares every derived one, which is the property the
   warning on {!Room.type-wall} is about. *)
let threshold_edits_share_the_derived_fields () =
  let t = doorway_in ~name:"gate" (Vec.make 0. 0.) (Vec.make 4. 0.) in
  let hung = Room.with_door t (Some (Door.make mesh)) in
  Alcotest.(check bool)
    "with_door hangs the door" true
    (hung.Room.door <> None && t.Room.door = None);
  Alcotest.(check bool)
    "and shares the geometry" true
    (hung.Room.edge == t.Room.edge && hung.Room.normal == t.Room.normal);
  let bare = Room.with_lintel t None in
  Alcotest.(check bool)
    "with_lintel takes the lintel away" true
    (bare.Room.lintel = None && t.Room.lintel <> None);
  let w = Room.threshold_wall t ~height:2.5 ~material:mesh in
  Alcotest.(check bool)
    "threshold_wall copies the segment across" true
    (w.Room.a == t.Room.a && w.Room.edge == t.Room.edge
    && w.Room.length = t.Room.length
    && w.Room.normal == t.Room.normal);
  Alcotest.(check bool)
    "wearing the height and material it was told" true
    (w.Room.height = 2.5 && w.Room.material == mesh && w.Room.decals = [])

(* [threshold_wall] is the one route to a [wall] that does not go through
   [wall], and a private type is only worth having if every route to it holds.
   The geometry it copies came from a threshold that already passed the same
   test; the height comes from the caller, and one that does not rise is the
   invisible collision blocker [wall]'s own comment is about. *)
let threshold_wall_will_not_build_a_wall_that_does_not_rise () =
  let t = doorway_in ~name:"gate" (Vec.make 0. 0.) (Vec.make 4. 0.) in
  let raises what height =
    Alcotest.check_raises what
      (Invalid_argument
         "Room.threshold_wall: the wall has to rise above the floor") (fun () ->
        ignore (Room.threshold_wall t ~height ~material:pale))
  in
  raises "a wall of no height" 0.;
  raises "a wall below the floor" (-2.);
  raises "a nan wall" Float.nan;
  raises "an infinite wall" Float.infinity

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
          case "a decal or sprite of no size is refused"
            a_decal_or_sprite_of_no_size_is_refused;
        ] );
      ( "collision",
        [
          case "blocked within the padding" blocked_within_the_padding;
          case "collinear segments still cross" collinear_segments_still_cross;
          case "a step far shorter than a pixel still crosses"
            a_step_far_shorter_than_a_pixel_still_crosses;
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
          case "a rectangle is wound inward from either corner pair"
            a_rectangle_is_wound_inward_from_either_corner_pair;
          case "a flat rectangle is refused" a_flat_rectangle_is_refused;
          case "a wall or threshold with no length is refused"
            a_wall_or_threshold_with_no_length_is_refused;
          case "a wall or threshold that does not rise is refused"
            a_wall_or_threshold_that_does_not_rise_is_refused;
          case "a lintel below its opening is refused"
            a_lintel_below_its_opening_is_refused;
          case "with_lintel is refused the same lintel"
            with_lintel_is_refused_the_same_lintel;
          case "a path that stands still is refused"
            a_path_that_stands_still_is_refused;
          case "a polygon that is not one is refused"
            a_polygon_that_is_not_one_is_refused;
        ] );
      ( "bounds",
        [
          case "bounds read back through the accessors"
            bounds_read_back_through_the_accessors;
        ] );
      ( "doorways",
        [
          case "a doorway splits the wall it is cut into"
            a_doorway_splits_the_wall_it_is_cut_into;
          case "a doorway that could not be cut is refused"
            a_doorway_that_could_not_be_cut_is_refused;
          case "a doorway as wide as its wall leaves no jamb"
            a_doorway_as_wide_as_its_wall_leaves_no_jamb;
          case "a doorway can hang a door" a_doorway_can_hang_a_door;
          case "with_thresholds keeps a copy" with_thresholds_keeps_a_copy;
          case "a door's state decides what is seen and what is felt"
            a_doors_state_decides_what_is_seen_and_what_is_felt;
          case "across is the link transform" across_is_the_link_transform;
          case "nearest threshold picks the nearest that qualifies"
            nearest_threshold_picks_the_nearest_that_qualifies;
          case "threshold edits share the derived fields"
            threshold_edits_share_the_derived_fields;
          case "threshold_wall will not build a wall that does not rise"
            threshold_wall_will_not_build_a_wall_that_does_not_rise;
        ] );
    ]
