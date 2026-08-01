(** [Sight] answers what the crosshair is on, through a doorway and in names
    rather than pixels. It touches no SDL — the vertical it works in comes from
    {!Viewport.centre_rise}, which is a function of pitch alone — so all of it
    tests headlessly. *)

open Camlcast_core
open Support

(* Two rooms, joined, with something to look at in each. The near room's is off
   to one side so it is out of the way of a level look east; the far room's
   stands square in front of the doorway.

   Both rooms are the same 0..4 square in their own coordinates, so nothing here
   works by accident of a shared frame. *)
let figure pos = Room.sprite ~size:1.4 ~image:poster pos

let rooms ?door ?lintel ?lintel_top ?(ceiling = flat_ceiling) ?(bare = false)
    ?(near = []) ?(far = []) () =
  (* {!Room.doorway} gives the jambs and the strip of wall above the opening one
     material, and a transom is precisely the case where those differ, so it is
     put back afterwards rather than asked for. [lintel_top] moves the top of
     that strip, which {!Room.doorway} also has no way to ask for: it always
     takes the wall's own height. [bare] takes the strip away altogether. *)
  let over (t : Room.threshold) =
    if bare then Room.with_lintel t None
    else
      match (lintel, lintel_top) with
      | None, None -> t
      | _ ->
          let was = Option.get t.Room.lintel in
          Room.with_lintel t
            (Some
               {
                 Room.top = Option.value lintel_top ~default:was.Room.top;
                 material = Option.value lintel ~default:was.Room.material;
               })
  in
  let first_jambs, east =
    Room.doorway ~name:"east" ?door ~width:1. ~opening:2. ~height:3.
      ~material:pale (Vec.make 4. 0.) (Vec.make 4. 4.)
  and second_jambs, west =
    Room.doorway ~name:"west" ?door ~width:1. ~opening:2. ~height:3.
      ~material:dim (Vec.make 0. 4.) (Vec.make 0. 0.)
  in
  let east = over east and west = over west in
  (* [ceiling] roofs the room the player stands in and not the one beyond it, so
     a case about the roof over the crosshair leaves the far room alone. *)
  let first =
    Room.make ~thresholds:[ east ] ~floor:flat_floor ~ceiling ~sprites:near
      (first_jambs
      @ [
          Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~material:pale (Vec.make 4. 4.) (Vec.make 0. 4.);
          Room.wall ~height:3. ~material:pale (Vec.make 0. 4.) (Vec.make 0. 0.);
        ])
  and second =
    Room.make ~thresholds:[ west ] ~floor:flat_floor ~ceiling:flat_ceiling
      ~sprites:far
      (second_jambs
      @ [
          Room.wall ~height:3. ~material:dim (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~material:dim (Vec.make 4. 0.) (Vec.make 4. 4.);
          Room.wall ~height:3. ~material:dim (Vec.make 4. 4.) (Vec.make 0. 4.);
        ])
  in
  World.make
    ~rooms:[ ("first", first); ("second", second) ]
    ~links:[ (("first", "east"), ("second", "west")) ]
    ~atmosphere:air ~spawn:("first", centre)

(* Standing in the first room looking due east, straight at the doorway. The
   second room's own copy of the doorway is at x = 0 there, so something at
   (2, 2) in it is two cells beyond the threshold. *)
let looking_east ?(pitch = 0.) ?(from = centre) () =
  let p = Player.make ~room:0 ~pos:from ~angle:0. in
  Player.pitch_by p ~radians:pitch

let describe = function
  | None -> "nothing"
  | Some { Sight.kind = Sight.Wall w; room; _ } ->
      Printf.sprintf "wall %d of room %d" w.index room
  | Some { Sight.kind = Sight.Sprite s; room; _ } ->
      Printf.sprintf "sprite %d of room %d" s.index room
  | Some { Sight.kind = Sight.Doorway d; room; _ } ->
      Printf.sprintf "doorway %d of room %d" d.index room

let is what got = Alcotest.(check string) "" what (describe got)

(* A sprite is cut out against nothing, so what the crosshair is on depends on
   the image and not only on the box around it — and the horizontal half of that
   comes from the image's {e width}. The picture here is 16 across and 12 down,
   so the two extents are different numbers and reading across by the wrong one
   lands somewhere else: its left half is clear and its right half solid, and a
   version indexing columns by the height would put the middle of the box at
   column 6 rather than 8, which is on the clear side.

   The renderer maps a sprite's screen box onto the image exactly this way, so
   this is also what keeps what can be picked the same as what is drawn. *)
let a_sprite_is_read_across_by_its_width () =
  let split =
    Image.make ~height:12 ~width:16 (fun ~u ~v:_ ->
        if u < 8 then Image.clear else (Color.rgb 200 60 60, 255))
  in
  let world =
    rooms ~near:[ Room.sprite ~size:1.4 ~image:split (Vec.make 3.5 2.) ] ()
  in
  (* Dead ahead: the middle of the sprite's width, which is the first solid
     column. *)
  is "sprite 0 of room 0"
    (Sight.look world (looking_east ~from:(Vec.make 2. 2.) ()));
  (* A quarter of the way across it, which is on the side that was cut away. The
     crosshair passes through and carries on into the room beyond, so what it
     finds there is the far room's business — all this case is asserting is that
     the sprite is not it. *)
  let past =
    describe (Sight.look world (looking_east ~from:(Vec.make 2. 2.35) ()))
  in
  Alcotest.(check bool)
    (Printf.sprintf "the cut-away side is seen through (found %s)" past)
    true
    (past <> "sprite 0 of room 0")

(* Close enough and a sprite stops being a target, because close enough it stops
   being drawn. {!Renderer} will not draw one nearer than
   {!Config.sprite_near_clip} — a billboard is scaled by one over its distance,
   so past that there is nothing left worth placing — and sprites are not
   collision geometry, so the player may walk into one and arrive there. Both
   sides read the one constant; this is the pair of cases that says so, and that
   fails if either side is given a cutoff of its own again. *)
let a_sprite_nearer_than_the_clip_is_not_picked () =
  let ahead d = rooms ~near:[ figure (Vec.make (2. +. d) 2.) ] () in
  let clip = Config.sprite_near_clip in
  (* Just beyond it, dead ahead: the middle of the poster, which is solid. *)
  is "sprite 0 of room 0" (Sight.look (ahead (clip +. 0.01)) (looking_east ()));
  (* Just inside it, and the ray carries on as though the sprite were not there.
     What it finds instead is the far room's business — all this asserts is that
     the sprite is not it. *)
  let inside = describe (Sight.look (ahead (clip -. 0.01)) (looking_east ())) in
  Alcotest.(check bool)
    (Printf.sprintf "one nearer than the clip is not picked (found %s)" inside)
    true
    (inside <> "sprite 0 of room 0")

(* A sprite that floats is picked where it floats. The crosshair here is level,
   so it runs along eye height — under a sprite lifted clear of it, and through
   the middle of the same sprite standing on the floor. What the ray finds
   instead is the far room's business; all this says is that it is not the
   sprite, and that the sprite is still there to be found once the view tips up
   towards it. *)
let a_lifted_sprite_is_looked_at_where_it_floats () =
  let raised base =
    rooms
      ~near:[ Room.sprite ~base ~size:1. ~image:poster (Vec.make 3.5 2.) ]
      ()
  in
  is "sprite 0 of room 0"
    (Sight.look (raised 0.) (looking_east ~from:(Vec.make 2. 2.) ()));
  let under =
    describe (Sight.look (raised 0.9) (looking_east ~from:(Vec.make 2. 2.) ()))
  in
  Alcotest.(check bool)
    (Printf.sprintf "level, the crosshair passes under it (found %s)" under)
    true
    (under <> "sprite 0 of room 0");
  (* Tipped up, the same crosshair reaches it: the centre ray gains
     {!Viewport.centre_rise} of height per cell, and the sprite is a cell and a
     half away. *)
  is "sprite 0 of room 0"
    (Sight.look (raised 0.9)
       (looking_east ~pitch:0.6 ~from:(Vec.make 2. 2.) ()))

(* Through an open doorway, into the room beyond, at a sprite standing there.
   This is the whole feature: the thing looked at is in another room, in another
   coordinate frame, and is named without going in. *)
let through_an_open_doorway () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let seen = Sight.look world (looking_east ()) in
  is "sprite 0 of room 1" seen;
  match seen with
  | None -> Alcotest.fail "expected to see the sprite"
  | Some s ->
      Alcotest.(check int) "one doorway away" 1 s.Sight.crossed;
      (* Two cells to the threshold, then two more to the sprite. *)
      Alcotest.check close "distance adds up across the doorway" 4.
        s.Sight.distance

(* The same look, into a neighbour that folds back on itself. {!Support.recessed}
   sets the second room's doorway in the back of a blind slot, so along this ray
   that slot's back wall stands a cell and a half away — nearer than the doorway
   itself. Nearer, and so behind the player's own east wall rather than beyond
   it: in the first room's coordinates it is half a cell {e this} side of the
   opening, in space the player is standing in. Not something the crosshair can
   be on, however much of the second room it belongs to.

   What it can be on is the far side of the room the doorway opens into, two
   cells past it. Asserted by distance rather than by which wall, so that
   reordering the fixture's walls does not fail this. *)
let what_stands_in_front_of_a_doorway_is_not_seen_through_it () =
  match Sight.look (recessed ()) (looking_east ()) with
  | None -> Alcotest.fail "expected to see the far wall"
  | Some s ->
      Alcotest.(check int) "one doorway away" 1 s.Sight.crossed;
      Alcotest.(check int) "and in the room beyond it" 1 s.Sight.room;
      (* Two cells to the doorway, two more to the far wall. The slot's back
         wall, were it picked, would answer 1.5. *)
      Alcotest.check close "the far wall and not the near one" 4.
        s.Sight.distance

(* A shut door stops the ray where an open one passed it, and says which
   doorway it was — which is what a game needs to open it. *)
let a_shut_door_stops_it () =
  let closed =
    rooms ~door:(Door.make dim) ~far:[ figure (Vec.make 2. 2.) ] ()
  in
  is "doorway 0 of room 0" (Sight.look closed (looking_east ()));
  (* And opening it lets the eye through again. *)
  let opened = World.set_door closed ~room:0 ~threshold:0 Door.Open in
  is "sprite 0 of room 1" (Sight.look opened (looking_east ()))

(* A leaf of a material you see through stops the ray no more than a
   see-through wall does — the renderer draws the room behind it, and what can
   be picked is what can be seen. What it still does is refuse the step: the two
   questions are different questions, and this is the case that says so.

   {!Support.glass} and not {!Support.mesh}, because the claim here is about the
   material and not about the aim: glass is see-through at every texel, so this
   holds wherever the crosshair lands on the leaf. A grille would be answering a
   different question, and {!a_grille_stops_the_ray_along_its_bars} asks it. *)
let a_see_through_leaf_does_not_stop_it () =
  let barred =
    rooms ~door:(Door.make glass) ~far:[ figure (Vec.make 2. 2.) ] ()
  in
  is "sprite 0 of room 1" (Sight.look barred (looking_east ()));
  Alcotest.(check bool)
    "and it is still a door to walk into" true
    (Room.shut (Room.threshold_at (World.room barred 0) 0))

(* The same of a glazed transom. Looking over the opening, an opaque strip of
   wall is what the ray meets; one you can see through is looked past, into the
   far room and at something standing high enough in it to be up there.

   The steepest pitch and the far side of the room, for the reason
   {!looking_over_the_opening_meets_the_lintel} spells out. *)
let a_see_through_lintel_does_not_stop_it () =
  let over_the_door lintel =
    Sight.look
      (rooms ?lintel
         ~far:[ Room.sprite ~base:2.7 ~size:1. ~image:poster (Vec.make 1. 2.) ]
         ())
      (looking_east ~pitch:Config.max_pitch ~from:(Vec.make 1. 2.) ())
  in
  is "doorway 0 of room 0" (over_the_door None);
  is "sprite 0 of room 1" (over_the_door (Some glass))

(* A nearer opaque thing wins, wherever it stands. *)
let a_nearer_thing_occludes () =
  let world =
    rooms ~near:[ figure (Vec.make 3. 2.) ] ~far:[ figure (Vec.make 2. 2.) ] ()
  in
  is "sprite 0 of room 0" (Sight.look world (looking_east ()));
  (* The near one is in the room the player is standing in, so a game that only
     collects from the next room reads [crossed] and discards this. *)
  match Sight.look world (looking_east ()) with
  | Some s -> Alcotest.(check int) "no doorway crossed" 0 s.Sight.crossed
  | None -> Alcotest.fail "expected the near sprite"

(* A screen across the near room, added to it after the fact — the same rebuild
   the cases below all want, and the reason they can all say "wall 5": it goes
   on the end of the room's own four walls and its jamb, so it is always the
   last of them. [a] and [b] are its ends, which the see-through cases move
   about to aim the crosshair at one texel of it or another. *)
let screened ?decals ~material a b world =
  World.replace_room world ~room:0
    ~replacement:
      (let before = World.room world 0 in
       Room.make
         ~thresholds:
           (List.init (Room.threshold_count before) (Room.threshold_at before))
         ~floor:flat_floor ~ceiling:flat_ceiling
         (List.init (Room.wall_count before) (Room.wall_at before)
         @ [ Room.wall ~height:3. ~material ?decals a b ]))

(* A wall in the way of the doorway does the same, and reports which wall — the
   index, not a copy of it, so it still means something after the room has been
   rebuilt around it. *)
let a_wall_occludes_and_names_itself () =
  let blocked =
    screened ~material:pale (Vec.make 3. 1.) (Vec.make 3. 3.)
      (rooms ~far:[ figure (Vec.make 2. 2.) ] ())
  in
  match Sight.look blocked (looking_east ()) with
  | Some { Sight.kind = Sight.Wall w; room; distance; crossed } ->
      Alcotest.(check int) "the room the player is in" 0 room;
      Alcotest.(check int) "no doorway crossed" 0 crossed;
      Alcotest.(check int) "the wall just added, last in the array" 5 w.index;
      Alcotest.check close "one cell ahead" 1. distance;
      Alcotest.check close "struck in the middle of its length" 1. w.along;
      (* Eye height above the flat floor, since the view is level. *)
      Alcotest.check close "at eye height" Config.eye_height w.z
  | other -> Alcotest.failf "expected a wall, got %s" (describe other)

(* Where an aim lands on {!Support.mesh}, said in the fixture's own terms rather
   than asked of {!Material.opaque_at}, which is the thing under test: it is bar
   where either index falls in the first three of every eight — the rows offset
   by five, so that eye height is a hole and not a bar — and clear in the
   five-by-five hole between. [along] is how far across the screen the ray
   crosses it and [above] how high up it.

   This shares two functions with the code under test, so it pins the aim and
   not the arithmetic. What pins the arithmetic is elsewhere and on purpose:
   {!Texture.row_of_height} has its own cases in [test_texture], and the
   agreement with the {e picture} is asserted on real pixels in
   [test_renderer]'s "a grille is picked where it is drawn". *)
let on_a_bar ~along ~above =
  let p = mesh.Material.pattern in
  let u = Texture.column_of_offset p (along -. Float.floor along)
  and v = Texture.row_of_height p above in
  u mod 8 < 3 || (v + 5) mod 8 < 3

(* A see-through wall is an obstacle to picking exactly where it is one to the
   eye, and nowhere else. The renderer decides that per texel — a solid one is
   written over the column and a clear one leaves what is behind — so the
   crosshair has to decide it per texel too, or it names the sprite behind a bar
   the picture shows no sprite through.

   Geometry as in [a_wall_occludes_and_names_itself]: a screen two cells long
   with the doorway and a sprite behind it, crossed level at eye height. Sliding
   its near end moves where along it the ray lands, which is the only thing that
   changes between the two halves. *)
let a_see_through_wall_stops_the_ray_at_its_bars () =
  let world () = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let at y0 =
    screened ~material:mesh (Vec.make 3. y0) (Vec.make 3. (y0 +. 2.)) (world ())
  in
  (* The ray crosses at y = 2, so [along] is that much past the screen's near
     end, and [above] is eye height over the flat floor. Asserted rather than
     assumed: a change to the fixture or to [Config.eye_height] should fail here
     and say which aim moved, not silently swap the two answers below. *)
  let aim y0 = (2. -. y0, Config.eye_height) in
  let bar_along, bar_above = aim 1. and hole_along, hole_above = aim 0.9 in
  Alcotest.(check bool)
    "the near end squarely on a cell boundary lands on a bar" true
    (on_a_bar ~along:bar_along ~above:bar_above);
  Alcotest.(check bool)
    "and a tenth of a cell along lands in a hole" false
    (on_a_bar ~along:hole_along ~above:hole_above);
  (* A bar covers the pixel, so the wall is what is being looked at — named the
     same way any other wall would be. *)
  is "wall 5 of room 0" (Sight.look (at 1.) (looking_east ()));
  (* A hole covers nothing, so the ray goes on through the doorway behind it. *)
  is "sprite 0 of room 1" (Sight.look (at 0.9) (looking_east ()))

(* Where no texel of a surface is solid, no aim at it stops the ray. Glass is
   drawn — the renderer blends it over the column and it tints what is behind —
   but blending is exactly the case where what is behind is still showing, and
   what is still showing is still pickable. The line is at a full 255 and not at
   "drawn at all", because a window you cannot look through is not a window.

   The same two placements the grille was given, which there answered
   differently and here do not: that is the difference between a material and an
   aim, said side by side. *)
let a_pane_of_glass_is_looked_through () =
  List.iter
    (fun y0 ->
      is "sprite 0 of room 1"
        (Sight.look
           (screened ~material:glass (Vec.make 3. y0)
              (Vec.make 3. (y0 +. 2.))
              (rooms ~far:[ figure (Vec.make 2. 2.) ] ()))
           (looking_east ())))
    [ 1.; 0.9 ]

(* A wall stops at the ceiling over it, however tall it was authored, because
   that is where {!Renderer} stops drawing it: above the roof the rows are the
   ceiling [draw_planes] has already painted, and a wall that carried on up
   there would be a wall nobody can see. Reported at all and the crosshair names
   masonry where the picture shows a roof — and a decal hung high on such a wall
   would be pickable through it.

   The roof is read at the hit point rather than once for the room, so the
   second half tilts one instead of lowering it: the same ray, at the same
   height, meets a wall at one end of the room and the ceiling at the other. *)
let a_wall_is_not_picked_over_the_ceiling () =
  (* Where the crosshair is when it reaches a wall two cells off, read off
     {!Viewport} so that a change to the field of view moves it with the
     picture. *)
  let up =
    Config.eye_height +. (Viewport.centre_rise ~pitch:Config.max_pitch *. 2.)
  in
  let boxed ceiling =
    World.make
      ~rooms:
        [
          ( "only",
            Room.make ~floor:flat_floor ~ceiling
              [
                Room.wall ~height:9. ~material:pale (Vec.make 0. 0.)
                  (Vec.make 4. 0.);
                Room.wall ~height:9. ~material:pale (Vec.make 4. 0.)
                  (Vec.make 4. 4.);
                Room.wall ~height:9. ~material:pale (Vec.make 4. 4.)
                  (Vec.make 0. 4.);
                Room.wall ~height:9. ~material:pale (Vec.make 0. 4.)
                  (Vec.make 0. 0.);
              ] );
        ]
      ~links:[] ~atmosphere:air ~spawn:("only", centre)
  in
  let looking ?(angle = 0.) pitch =
    Player.pitch_by (Player.make ~room:0 ~pos:centre ~angle) ~radians:pitch
  in
  Alcotest.(check bool)
    "the eye is under the roof and the crosshair over it" true
    (Config.eye_height < up -. 0.2);
  let low =
    Room.Roof { Room.plane = Plane.horizontal (up -. 0.2); material = dim }
  in
  (* Level, the wall is there to be found; pitched up over the roof it is not,
     though nine cells of it stand behind that roof. *)
  is "wall 1 of room 0" (Sight.look (boxed low) (looking 0.));
  is "nothing" (Sight.look (boxed low) (looking Config.max_pitch));
  (* A roof that rises towards the west: over the east wall it is under the
     crosshair, over the west wall it is clear of it. One ray, one height, two
     answers, from the slope alone. *)
  let tilted =
    Room.Roof
      {
        Room.plane = Plane.make ~a:(-0.25) ~b:0. ~c:(up +. 0.3);
        material = dim;
      }
  in
  is "nothing" (Sight.look (boxed tilted) (looking Config.max_pitch));
  is "wall 3 of room 0"
    (Sight.look (boxed tilted) (looking ~angle:Float.pi Config.max_pitch))

(* Looking somewhere else finds something else, or nothing. The sprite is a
   cut-out and mostly empty, so this also covers the texel test: a crosshair
   inside its bounding box but outside its picture is looking past it. *)
let the_wrong_angle_misses () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let turned radians =
    Player.turn (Player.make ~room:0 ~pos:centre ~angle:0.) ~radians
  in
  is "wall 3 of room 0" (Sight.look world (turned 1.6));
  (* Level, but aimed at the jamb above the opening rather than through it. *)
  is "wall 1 of room 0" (Sight.look world (turned 0.6));
  (* Pitched down far enough that the ray is under every wall it meets before
     it would reach one — the floor is not something this picks. *)
  is "nothing" (Sight.look world (looking_east ~pitch:(-.Config.max_pitch) ()))

(* Looking up over the opening meets the wall standing above it, and not the
   room beyond: a doorway is a hole of a certain height, not a gap in the whole
   wall.

   It takes both the steepest pitch the camera allows and the far side of the
   room to manage it — the crosshair rises about 0.65 cells per cell at the
   limit, so clearing an opening two cells tall needs three cells of run. That
   is worth knowing: from close up, a player simply cannot look over a doorway,
   whatever they do with the mouse. *)
let looking_over_the_opening_meets_the_lintel () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let far_side = Vec.make 1. 2. in
  is "doorway 0 of room 0"
    (Sight.look world (looking_east ~pitch:Config.max_pitch ~from:far_side ()));
  (* Level from the same spot, it goes straight through. *)
  is "sprite 0 of room 1" (Sight.look world (looking_east ~from:far_side ()))

(* With no lintel at all it meets nothing instead, which is the same answer read
   from the other side. Omitting one says the opening already reaches the top of
   the wall it was cut into, so above its head there is no strip of wall left to
   stop the ray — and no way through either, since what is up there is this
   room's own ceiling, which {!Sight} does not pick. The far room's sprite
   stands high enough to be seen if the ray did carry on, and it is not. *)
let a_bare_opening_has_nothing_over_it () =
  let world =
    rooms ~bare:true
      ~far:[ Room.sprite ~base:2.7 ~size:1. ~image:poster (Vec.make 1. 2.) ]
      ()
  in
  is "nothing"
    (Sight.look world
       (looking_east ~pitch:Config.max_pitch ~from:(Vec.make 1. 2.) ()))

(* A strip of wall has a top of its own, and above it the answer is the bare
   case again: the renderer draws the strip from the opening's head up to
   {!Room.type-lintel}'s [top] and leaves the rows over that to the ceiling, so
   a crosshair up there is on the ceiling whatever the strip is made of.
   {!Room.doorway} always takes the wall's own height for the lintel, which puts
   its top and the ceiling in the same place and hides the question; a threshold
   built by hand can stop the strip short, and this is that. The roof is left
   well clear of the ray so that it is the lintel's own top doing the work. *)
let a_lintel_is_not_picked_above_its_top () =
  let rise = Viewport.centre_rise ~pitch:Config.max_pitch in
  (* The doorway stands at [x = 4], so a look east from [x] meets it [4 - x]
     away. Read off {!Viewport} rather than written down, so that a change to
     the field of view moves the expectation with the picture. *)
  let z_from x = Config.eye_height +. (rise *. (4. -. x)) in
  let under = 1. and over = 0.4 in
  let top = (z_from under +. z_from over) /. 2. in
  Alcotest.(check bool)
    "the strip stands over the opening, and the roof clear of the ray" true
    (2. < top && z_from over < 3.);
  let world = rooms ~lintel_top:top () in
  let looking x =
    Sight.look world
      (looking_east ~pitch:Config.max_pitch ~from:(Vec.make x 2.) ())
  in
  is "doorway 0 of room 0" (looking under);
  is "nothing" (looking over)

(* And the roof caps the opening itself, on all three of the paths through it.
   A ceiling hanging below a doorway's head is drawn across those rows, and
   neither the leaf nor the room beyond reaches them — so an opaque leaf is not
   there to be picked, and a bare opening or one you can see through is not a
   way through, exactly as {!Renderer} has it. A gap in a wall is no way past
   the roof over it.

   Half the usual pitch, so the crosshair is inside the opening rather than over
   its head: this is the leaf's band and not the lintel's. *)
let the_roof_caps_what_an_opening_shows () =
  let pitch = Config.max_pitch /. 2. in
  let at_doorway = Config.eye_height +. (Viewport.centre_rise ~pitch *. 2.) in
  let ceiling = at_doorway -. 0.15 in
  Alcotest.(check bool)
    "the crosshair is inside the opening, and the roof under it but over the \
     eye"
    true
    (at_doorway < 2. && Config.eye_height < ceiling);
  let low =
    Room.Roof { Room.plane = Plane.horizontal ceiling; material = dim }
  in
  let looking = looking_east ~pitch () in
  let solid = Door.make pale and glazed = Door.make glass in
  (* With the roof where the fixture puts it, three cells up and clear of all of
     this, each of the three answers as it always did. *)
  is "doorway 0 of room 0" (Sight.look (rooms ~door:solid ()) looking);
  List.iter
    (fun (what, world) ->
      let found = describe (Sight.look world looking) in
      Alcotest.(check bool)
        (Printf.sprintf
           "%s: with the roof clear it looks into the far room (found %s)" what
           found)
        true (mentions found "room 1"))
    [ ("through the glass", rooms ~door:glazed ()); ("bare", rooms ()) ];
  (* Drop the roof below the doorway's head and every one of them is ceiling. *)
  is "nothing" (Sight.look (rooms ~ceiling:low ~door:solid ()) looking);
  is "nothing" (Sight.look (rooms ~ceiling:low ~door:glazed ()) looking);
  is "nothing" (Sight.look (rooms ~ceiling:low ()) looking)

(* One doorway by default, because that is what the design asks for. Asking for
   none is asking about the room you are standing in. *)
let it_looks_as_far_as_it_is_told_to () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  is "sprite 0 of room 1" (Sight.look world (looking_east ()));
  is "doorway 0 of room 0" (Sight.look ~through:0 world (looking_east ()))

(* Asked twice, it answers the same. Nothing here consumes anything: collecting
   a sign is a change to the game's own record, and the engine's part is a pure
   function of the world and the pose. *)
let asking_twice_gives_the_same_answer () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let once = Sight.look world (looking_east ())
  and twice = Sight.look world (looking_east ()) in
  is (describe once) twice;
  Alcotest.(check bool)
    "and the distance with it" true
    (match (once, twice) with
    | Some a, Some b -> a.Sight.distance = b.Sight.distance
    | _ -> false)

(* A wall hit says which of the wall's decals is under the crosshair, so a
   picture hung on a wall is as targetable as a thing standing in front of one.
   [poster] is opaque within its frame and clear around the edge, which is what
   makes the last of these work. *)
let a_decal_on_a_wall_is_named () =
  let hung =
    Room.wall ~height:3. ~material:dim (Vec.make 4. 0.) (Vec.make 4. 4.)
      ~decals:
        [
          Room.decal ~along:2. ~z:Config.eye_height ~half_width:0.6
            ~half_height:0.6 poster;
        ]
  in
  let world =
    World.make
      ~rooms:
        [
          ( "only",
            Room.make ~floor:flat_floor ~ceiling:flat_ceiling
              [
                hung;
                Room.wall ~height:3. ~material:pale (Vec.make 0. 0.)
                  (Vec.make 4. 0.);
                Room.wall ~height:3. ~material:pale (Vec.make 4. 4.)
                  (Vec.make 0. 4.);
                Room.wall ~height:3. ~material:pale (Vec.make 0. 4.)
                  (Vec.make 0. 0.);
              ] );
        ]
      ~links:[] ~atmosphere:air ~spawn:("only", centre)
  in
  let decal_under player =
    match Sight.look world player with
    | Some { Sight.kind = Sight.Wall w; _ } -> w.decal
    | _ -> None
  in
  (* Level, straight at the middle of the picture, which is hung at eye height. *)
  Alcotest.(check (option int))
    "the picture the crosshair is on" (Some 0)
    (decal_under (Player.make ~room:0 ~pos:centre ~angle:0.));
  (* The same wall, a foot to one side of the frame: wall, and no picture. *)
  Alcotest.(check (option int))
    "beside it is bare wall" None
    (decal_under (Player.make ~room:0 ~pos:(Vec.make 2. 0.8) ~angle:0.));
  (* And the wall is still what was hit, picture or no picture. *)
  is "wall 0 of room 0"
    (Sight.look world (Player.make ~room:0 ~pos:centre ~angle:0.))

(* A mark on a wall you can see through is drawn, so it can be picked.
   [Renderer.draw_wall] runs its decal loop outside the test on the wall's own
   texel, so a decal is painted whether or not the wall under it was — and a
   see-through wall reaches that same function through the translucent pass.
   This is the half of that agreement which says the crosshair follows the
   paint.

   The exception is the mark's and not the wall's: the same screen left bare is
   still looked straight through, which is what the two halves of this test say
   next to each other. So the screen is aimed at a {e hole} of the grille — it
   runs north from (3, 0.9), which puts the crossing a tenth of a cell along a
   texel that is clear, by [a_see_through_wall_stops_the_ray_at_its_bars]'s
   reckoning. Aimed at a bar the wall would stop the ray by itself and the two
   halves would agree for the wrong reason. The mark is wide enough to cover
   that crossing either way. *)
let a_decal_on_a_see_through_wall_is_picked () =
  let world ~decals =
    screened ~material:mesh ~decals (Vec.make 3. 0.9) (Vec.make 3. 2.9)
      (rooms ~far:[ figure (Vec.make 2. 2.) ] ())
  in
  let marked =
    world
      ~decals:
        [
          Room.decal ~along:1. ~z:Config.eye_height ~half_width:0.6
            ~half_height:0.6 poster;
        ]
  in
  let bare = world ~decals:[] in
  (* Bare, it is no obstacle at all, exactly as before. *)
  is "sprite 0 of room 1" (Sight.look bare (looking_east ()));
  (* Marked, the mark stops the ray — and what is named is the wall it is on,
     since a decal is something on a wall and never a thing of its own. *)
  is "wall 5 of room 0" (Sight.look marked (looking_east ()));
  Alcotest.(check (option int))
    "and the mark under the crosshair" (Some 0)
    (match Sight.look marked (looking_east ()) with
    | Some { Sight.kind = Sight.Wall w; _ } -> w.decal
    | _ -> None)

(* The whole of dynamic decals in one test: what a wall hit reports is exactly
   what a decal is placed in.

   Aim at a bare wall, take the four numbers back — which wall, how far along,
   how high, which face — hand them straight to {!Room.add_decal} without
   converting anything, put the room back with {!World.replace_room}, and aim
   again from where you were standing. The mark has to be under the crosshair,
   because the crosshair is where it was put.

   Nothing here computes a position. If the two ends of this disagreed about
   what [along] or [z] meant, or about which way round the faces are, the second
   sighting would find bare wall. *)
let a_wall_can_be_marked_where_the_crosshair_is () =
  let world = rooms () in
  let aim =
    Player.make ~room:0 ~pos:(Vec.make 2. 2.) ~angle:(-.Float.pi /. 2.)
  in
  let mark =
    Image.make ~width:8 (fun ~u:_ ~v:_ -> (Color.rgb 240 240 240, 255))
  in
  match Sight.look world aim with
  | Some { Sight.kind = Sight.Wall w; room; _ } ->
      Alcotest.(check (option int)) "bare wall to begin with" None w.decal;
      let marked =
        World.replace_room world ~room
          ~replacement:
            (Room.add_decal (World.room world room) ~wall:w.index
               (Room.decal ~facing:w.facing ~along:w.along ~z:w.z
                  ~half_width:0.25 ~half_height:0.25 mark))
      in
      (match Sight.look marked aim with
      | Some { Sight.kind = Sight.Wall w'; _ } ->
          Alcotest.(check int) "the same wall" w.index w'.index;
          Alcotest.(check (option int))
            "and the mark is under the crosshair" (Some 0) w'.decal
      | other ->
          Alcotest.failf "expected the marked wall, got %s" (describe other));
      (* From the other face of that wall there is nothing to find. The wall
         here is a room boundary, so getting behind it means asking Room
         directly — which is the same question the renderer asks. *)
      let wall = Room.wall_at (World.room marked room) w.index in
      let behind = if w.facing = Room.Front then Room.Back else Room.Front in
      Alcotest.(check (option int))
        "and nothing of it from behind" None
        (Room.decal_column (List.hd wall.Room.decals) ~seen_from:behind
           ~along:w.along)
  | other -> Alcotest.failf "expected a wall, got %s" (describe other)

(* What is targeted has to be drawable: the sighting carries the pose of the
   room the thing is in, and {!Viewport.sprite_box} placed with it lands on the
   same rectangle the renderer drew the sprite in. Checked through a doorway,
   where the two rooms' coordinates differ and getting it wrong is easy. *)
let a_target_can_be_found_on_the_screen () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let player = looking_east () in
  match Sight.look world player with
  | Some { Sight.kind = Sight.Sprite s; room; pose; distance; crossed } ->
      Alcotest.(check int) "in the room next door" 1 crossed;
      let viewport =
        Viewport.make ~pitch:0.
          ~eye_z:
            (Plane.elevation flat_floor.Room.plane centre +. Config.eye_height)
          ~width:640 ~height:400
      in
      let sprite = Room.sprite_at (World.room world room) s.index in
      let left, top, right, bottom =
        Viewport.sprite_box viewport pose
          ~floor_z:(Plane.elevation flat_floor.Room.plane sprite.Room.pos)
          ~distance sprite
      in
      (* Dead ahead, so it is centred; and drawn from a square picture, so its
         width is its height. *)
      Alcotest.check close "centred across the screen" 320.
        ((left +. right) /. 2.);
      Alcotest.check close "as wide as it is tall" (bottom -. top)
        (right -. left);
      Alcotest.(check bool)
        "and on the screen at all" true
        (top > 0. && bottom < 400. && left > 0. && right < 640.)
  | other -> Alcotest.failf "expected a sprite, got %s" (describe other)

let () =
  Alcotest.run "Sight"
    [
      ( "through a doorway",
        [
          case "through an open doorway" through_an_open_doorway;
          case "what stands in front of a doorway is not seen through it"
            what_stands_in_front_of_a_doorway_is_not_seen_through_it;
          case "a shut door stops it" a_shut_door_stops_it;
          case "a see-through leaf does not stop it"
            a_see_through_leaf_does_not_stop_it;
          case "a see-through lintel does not stop it"
            a_see_through_lintel_does_not_stop_it;
          case "looking over the opening meets the lintel"
            looking_over_the_opening_meets_the_lintel;
          case "a bare opening has nothing over it"
            a_bare_opening_has_nothing_over_it;
          case "a lintel is not picked above its top"
            a_lintel_is_not_picked_above_its_top;
          case "the roof caps what an opening shows"
            the_roof_caps_what_an_opening_shows;
          case "it looks as far as it is told to"
            it_looks_as_far_as_it_is_told_to;
        ] );
      ( "occlusion",
        [
          case "a nearer thing occludes" a_nearer_thing_occludes;
          case "a wall occludes and names itself"
            a_wall_occludes_and_names_itself;
          case "a see-through wall stops the ray at its bars"
            a_see_through_wall_stops_the_ray_at_its_bars;
          case "a pane of glass is looked through"
            a_pane_of_glass_is_looked_through;
          case "a sprite is read across by its width"
            a_sprite_is_read_across_by_its_width;
          case "a lifted sprite is looked at where it floats"
            a_lifted_sprite_is_looked_at_where_it_floats;
          case "a sprite nearer than the clip is not picked"
            a_sprite_nearer_than_the_clip_is_not_picked;
          case "a wall is not picked over the ceiling"
            a_wall_is_not_picked_over_the_ceiling;
          case "the wrong angle misses" the_wrong_angle_misses;
          case "asking twice gives the same answer"
            asking_twice_gives_the_same_answer;
        ] );
      ( "naming what was found",
        [
          case "a decal on a wall is named" a_decal_on_a_wall_is_named;
          case "a decal on a see-through wall is picked"
            a_decal_on_a_see_through_wall_is_picked;
          case "a wall can be marked where the crosshair is"
            a_wall_can_be_marked_where_the_crosshair_is;
          case "a target can be found on the screen"
            a_target_can_be_found_on_the_screen;
        ] );
    ]
