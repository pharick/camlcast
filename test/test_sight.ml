(** [Sight] answers what the crosshair is on, through a doorway and in names
    rather than pixels. It touches no SDL — the vertical it works in comes from
    {!Viewport.centre_rise}, which is a function of pitch alone — so all of it
    tests headlessly. *)

open Raycaster
open Support

(* Two rooms, joined, with something to look at in each. The near room's is off
   to one side so it is out of the way of a level look east; the far room's
   stands square in front of the doorway.

   Both rooms are the same 0..4 square in their own coordinates, so nothing here
   works by accident of a shared frame. *)
let figure pos = Room.sprite ~size:1.4 ~image:poster pos

let rooms ?door ?(near = []) ?(far = []) () =
  let first_jambs, east =
    Room.doorway ~name:"east" ?door ~width:1. ~opening:2. ~height:3.
      ~material:pale (Vec.make 4. 0.) (Vec.make 4. 4.)
  and second_jambs, west =
    Room.doorway ~name:"west" ?door ~width:1. ~opening:2. ~height:3.
      ~material:dim (Vec.make 0. 4.) (Vec.make 0. 0.)
  in
  let first =
    Room.make ~thresholds:[ east ] ~floor:flat_floor ~ceiling:flat_ceiling
      ~sprites:near
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
  let p = Player.create ~room:0 ~pos:from ~angle:0. in
  Player.pitch_by p ~delta:pitch

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
    Image.make ~height:12 16 (fun ~u ~v:_ ->
        if u < 8 then Image.clear else (Color.rgb 200 60 60, 255))
  in
  let world =
    rooms ~near:[ Room.sprite ~size:1.4 ~image:split (Vec.make 3.5 2.) ] ()
  in
  (* Dead ahead: the middle of the sprite's width, which is the first solid
     column. *)
  is "sprite 0 of room 0"
    (Sight.cast world (looking_east ~from:(Vec.make 2. 2.) ()));
  (* A quarter of the way across it, which is on the side that was cut away. The
     crosshair passes through and carries on into the room beyond, so what it
     finds there is the far room's business — all this case is asserting is that
     the sprite is not it. *)
  let past =
    describe (Sight.cast world (looking_east ~from:(Vec.make 2. 2.35) ()))
  in
  Alcotest.(check bool)
    (Printf.sprintf "the cut-away side is seen through (found %s)" past)
    true
    (past <> "sprite 0 of room 0")

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
    (Sight.cast (raised 0.) (looking_east ~from:(Vec.make 2. 2.) ()));
  let under =
    describe (Sight.cast (raised 0.9) (looking_east ~from:(Vec.make 2. 2.) ()))
  in
  Alcotest.(check bool)
    (Printf.sprintf "level, the crosshair passes under it (found %s)" under)
    true
    (under <> "sprite 0 of room 0");
  (* Tipped up, the same crosshair reaches it: the centre ray gains
     {!Viewport.centre_rise} of height per cell, and the sprite is a cell and a
     half away. *)
  is "sprite 0 of room 0"
    (Sight.cast (raised 0.9)
       (looking_east ~pitch:0.6 ~from:(Vec.make 2. 2.) ()))

(* Through an open doorway, into the room beyond, at a sprite standing there.
   This is the whole feature: the thing looked at is in another room, in another
   coordinate frame, and is named without going in. *)
let through_an_open_doorway () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let seen = Sight.cast world (looking_east ()) in
  is "sprite 0 of room 1" seen;
  match seen with
  | None -> Alcotest.fail "expected to see the sprite"
  | Some s ->
      Alcotest.(check int) "one doorway away" 1 s.Sight.crossed;
      (* Two cells to the threshold, then two more to the sprite. *)
      Alcotest.check close "distance adds up across the doorway" 4.
        s.Sight.distance

(* A shut door stops the ray where an open one passed it, and says which
   doorway it was — which is what a game needs to open it. *)
let a_shut_door_stops_it () =
  let closed =
    rooms ~door:(Door.make dim) ~far:[ figure (Vec.make 2. 2.) ] ()
  in
  is "doorway 0 of room 0" (Sight.cast closed (looking_east ()));
  (* And opening it lets the eye through again. *)
  let opened = World.set_door closed ~room:0 ~threshold:0 Door.Open in
  is "sprite 0 of room 1" (Sight.cast opened (looking_east ()))

(* A nearer opaque thing wins, wherever it stands. *)
let a_nearer_thing_occludes () =
  let world =
    rooms ~near:[ figure (Vec.make 3. 2.) ] ~far:[ figure (Vec.make 2. 2.) ] ()
  in
  is "sprite 0 of room 0" (Sight.cast world (looking_east ()));
  (* The near one is in the room the player is standing in, so a game that only
     collects from the next room reads [crossed] and discards this. *)
  match Sight.cast world (looking_east ()) with
  | Some s -> Alcotest.(check int) "no doorway crossed" 0 s.Sight.crossed
  | None -> Alcotest.fail "expected the near sprite"

(* A wall in the way of the doorway does the same, and reports which wall — the
   index, not a copy of it, so it still means something after the room has been
   rebuilt around it. *)
let a_wall_occludes_and_names_itself () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let blocked =
    World.replace_room world ~room:0
      ~replacement:
        (let before = World.room world 0 in
         Room.make
           ~thresholds:(Array.to_list before.Room.thresholds)
           ~floor:flat_floor ~ceiling:flat_ceiling
           (Array.to_list before.Room.walls
           @ [
               Room.wall ~height:3. ~material:pale (Vec.make 3. 1.)
                 (Vec.make 3. 3.);
             ]))
  in
  match Sight.cast blocked (looking_east ()) with
  | Some { Sight.kind = Sight.Wall w; room; distance; crossed } ->
      Alcotest.(check int) "the room the player is in" 0 room;
      Alcotest.(check int) "no doorway crossed" 0 crossed;
      Alcotest.(check int) "the wall just added, last in the array" 5 w.index;
      Alcotest.check close "one cell ahead" 1. distance;
      Alcotest.check close "struck in the middle of its length" 1. w.along;
      (* Eye height above the flat floor, since the view is level. *)
      Alcotest.check close "at eye height" Config.eye_height w.z
  | other -> Alcotest.failf "expected a wall, got %s" (describe other)

(* A see-through wall is no more of an obstacle to picking than it is to the
   eye: the mesh fixture carries an alpha, so the sprite behind it wins. *)
let a_see_through_wall_does_not_occlude () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let screened =
    World.replace_room world ~room:0
      ~replacement:
        (let before = World.room world 0 in
         Room.make
           ~thresholds:(Array.to_list before.Room.thresholds)
           ~floor:flat_floor ~ceiling:flat_ceiling
           (Array.to_list before.Room.walls
           @ [
               Room.wall ~height:3. ~material:mesh (Vec.make 3. 1.)
                 (Vec.make 3. 3.);
             ]))
  in
  is "sprite 0 of room 1" (Sight.cast screened (looking_east ()))

(* Looking somewhere else finds something else, or nothing. The sprite is a
   cut-out and mostly empty, so this also covers the texel test: a crosshair
   inside its bounding box but outside its picture is looking past it. *)
let the_wrong_angle_misses () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let turned radians =
    Player.turn (Player.create ~room:0 ~pos:centre ~angle:0.) ~radians
  in
  is "wall 3 of room 0" (Sight.cast world (turned 1.6));
  (* Level, but aimed at the jamb above the opening rather than through it. *)
  is "wall 1 of room 0" (Sight.cast world (turned 0.6));
  (* Pitched down far enough that the ray is under every wall it meets before
     it would reach one — the floor is not something this picks. *)
  is "nothing" (Sight.cast world (looking_east ~pitch:(-.Config.max_pitch) ()))

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
    (Sight.cast world (looking_east ~pitch:Config.max_pitch ~from:far_side ()));
  (* Level from the same spot, it goes straight through. *)
  is "sprite 0 of room 1" (Sight.cast world (looking_east ~from:far_side ()))

(* One doorway by default, because that is what the design asks for. Asking for
   none is asking about the room you are standing in. *)
let it_looks_as_far_as_it_is_told_to () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  is "sprite 0 of room 1" (Sight.cast world (looking_east ()));
  is "doorway 0 of room 0" (Sight.cast ~through:0 world (looking_east ()))

(* Asked twice, it answers the same. Nothing here consumes anything: collecting
   a sign is a change to the game's own record, and the engine's part is a pure
   function of the world and the pose. *)
let asking_twice_gives_the_same_answer () =
  let world = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
  let once = Sight.cast world (looking_east ())
  and twice = Sight.cast world (looking_east ()) in
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
    match Sight.cast world player with
    | Some { Sight.kind = Sight.Wall w; _ } -> w.decal
    | _ -> None
  in
  (* Level, straight at the middle of the picture, which is hung at eye height. *)
  Alcotest.(check (option int))
    "the picture the crosshair is on" (Some 0)
    (decal_under (Player.create ~room:0 ~pos:centre ~angle:0.));
  (* The same wall, a foot to one side of the frame: wall, and no picture. *)
  Alcotest.(check (option int))
    "beside it is bare wall" None
    (decal_under (Player.create ~room:0 ~pos:(Vec.make 2. 0.8) ~angle:0.));
  (* And the wall is still what was hit, picture or no picture. *)
  is "wall 0 of room 0"
    (Sight.cast world (Player.create ~room:0 ~pos:centre ~angle:0.))

(* A mark on a wall you can see through is drawn, so it can be picked.
   [Renderer.draw_wall] runs its decal loop outside the test on the wall's own
   texel, so a decal is painted whether or not the wall under it was — and a
   see-through wall reaches that same function through the translucent pass.
   This is the half of that agreement which says the crosshair follows the
   paint.

   The exception is the mark's and not the wall's: the same screen left bare is
   still looked straight through, which is what the two halves of this test say
   next to each other. Geometry as in [a_wall_occludes_and_names_itself] — the
   screen runs north from (3, 1), so a level look east crosses it one cell
   along, at eye height. *)
let a_decal_on_a_see_through_wall_is_picked () =
  let world ~decals =
    let plain = rooms ~far:[ figure (Vec.make 2. 2.) ] () in
    World.replace_room plain ~room:0
      ~replacement:
        (let before = World.room plain 0 in
         Room.make
           ~thresholds:(Array.to_list before.Room.thresholds)
           ~floor:flat_floor ~ceiling:flat_ceiling
           (Array.to_list before.Room.walls
           @ [
               Room.wall ~height:3. ~material:mesh ~decals (Vec.make 3. 1.)
                 (Vec.make 3. 3.);
             ]))
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
  is "sprite 0 of room 1" (Sight.cast bare (looking_east ()));
  (* Marked, the mark stops the ray — and what is named is the wall it is on,
     since a decal is something on a wall and never a thing of its own. *)
  is "wall 5 of room 0" (Sight.cast marked (looking_east ()));
  Alcotest.(check (option int))
    "and the mark under the crosshair" (Some 0)
    (match Sight.cast marked (looking_east ()) with
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
    Player.create ~room:0 ~pos:(Vec.make 2. 2.) ~angle:(-.Float.pi /. 2.)
  in
  let mark = Image.make 8 (fun ~u:_ ~v:_ -> (Color.rgb 240 240 240, 255)) in
  match Sight.cast world aim with
  | Some { Sight.kind = Sight.Wall w; room; _ } ->
      Alcotest.(check (option int)) "bare wall to begin with" None w.decal;
      let marked =
        World.replace_room world ~room
          ~replacement:
            (Room.add_decal (World.room world room) ~wall:w.index
               (Room.decal ~facing:w.facing ~along:w.along ~z:w.z
                  ~half_width:0.25 ~half_height:0.25 mark))
      in
      (match Sight.cast marked aim with
      | Some { Sight.kind = Sight.Wall w'; _ } ->
          Alcotest.(check int) "the same wall" w.index w'.index;
          Alcotest.(check (option int))
            "and the mark is under the crosshair" (Some 0) w'.decal
      | other ->
          Alcotest.failf "expected the marked wall, got %s" (describe other));
      (* From the other face of that wall there is nothing to find. The wall
         here is a room boundary, so getting behind it means asking Room
         directly — which is the same question the renderer asks. *)
      let wall = (World.room marked room).Room.walls.(w.index) in
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
  match Sight.cast world player with
  | Some { Sight.kind = Sight.Sprite s; room; pose; distance; crossed } ->
      Alcotest.(check int) "in the room next door" 1 crossed;
      let viewport =
        Viewport.create ~pitch:0.
          ~eye_z:
            (Plane.elevation flat_floor.Room.plane centre +. Config.eye_height)
          ~width:640 ~height:400
      in
      let sprite = (World.room world room).Room.sprites.(s.index) in
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
          case "a shut door stops it" a_shut_door_stops_it;
          case "looking over the opening meets the lintel"
            looking_over_the_opening_meets_the_lintel;
          case "it looks as far as it is told to"
            it_looks_as_far_as_it_is_told_to;
        ] );
      ( "occlusion",
        [
          case "a nearer thing occludes" a_nearer_thing_occludes;
          case "a wall occludes and names itself"
            a_wall_occludes_and_names_itself;
          case "a see-through wall does not occlude"
            a_see_through_wall_does_not_occlude;
          case "a sprite is read across by its width"
            a_sprite_is_read_across_by_its_width;
          case "a lifted sprite is looked at where it floats"
            a_lifted_sprite_is_looked_at_where_it_floats;
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
