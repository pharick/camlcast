open Camlcast
open Support

(* Where a step ends up, with the doorways it went through dropped. The engine
   walks through {!Player.traverse} and reads the crossings; most of the suite
   below only cares where the player landed, so it says so once here. *)
let walk world player ~forward ~strafe =
  (Player.traverse world player ~forward ~strafe).Player.player

let facing_east ?(pos = centre) () = Player.create ~room:0 ~pos ~angle:0.

(* Viewport builds every ray as [dir + right * k]. That is only correct while
   both are unit vectors and they stay perpendicular. *)
let camera_basis () =
  List.iter
    (fun angle ->
      let p = Player.create ~room:0 ~pos:centre ~angle in
      Alcotest.check close "dir is a unit vector" 1. (Vec.length p.Player.dir);
      Alcotest.check close "right is a unit vector" 1.
        (Vec.length p.Player.right);
      Alcotest.check close "right is perpendicular to dir" 0.
        (dot p.Player.dir p.Player.right))
    [ 0.; 0.9; 2.7; -1.4 ]

(* Facing east (+x), your right hand points south (+y). Getting this backwards
   would mirror the whole world. *)
let right_is_actually_to_the_right () =
  Alcotest.check vec "east faces south-right" (Vec.make 0. 1.)
    (facing_east ()).Player.right

let turning_preserves_the_basis () =
  let turned = Player.turn (facing_east ()) ~radians:0.7 in
  Alcotest.check close "dir stays a unit vector" 1.
    (Vec.length turned.Player.dir);
  Alcotest.check close "right stays a unit vector" 1.
    (Vec.length turned.Player.right);
  Alcotest.check close "the basis stays orthogonal" 0.
    (dot turned.Player.dir turned.Player.right)

(* Pitch is a separate, clamped axis: it tips the view but never past the limit
   where the sheared image would look wrong, and it never touches the basis. *)
let pitch_tips_within_a_limit () =
  let p = facing_east () in
  Alcotest.check close "starts level" 0. p.Player.pitch;
  Alcotest.check close "a small tip is kept" 0.1
    (Player.pitch_by p ~radians:0.1).Player.pitch;
  Alcotest.(check bool)
    "looking too far up is capped" true
    ((Player.pitch_by p ~radians:100.).Player.pitch <= Config.max_pitch +. 1e-9);
  Alcotest.(check bool)
    "and too far down" true
    ((Player.pitch_by p ~radians:(-100.)).Player.pitch
   >= -.Config.max_pitch -. 1e-9)

let turning_does_not_move_the_player () =
  let turned = Player.turn (facing_east ()) ~radians:0.7 in
  Alcotest.check vec "same position" centre turned.Player.pos;
  Alcotest.(check bool)
    "a positive turn swings dir towards +y" true (turned.Player.dir.y > 0.)

let walking_follows_the_facing () =
  let forward = walk world (facing_east ()) ~forward:0.5 ~strafe:0. in
  Alcotest.check vec "forward moves along dir, without drift" (Vec.make 2.5 2.)
    forward.Player.pos;
  let back = walk world (facing_east ()) ~forward:(-0.5) ~strafe:0. in
  Alcotest.check vec "backwards moves against dir" (Vec.make 1.5 2.)
    back.Player.pos

(* Sidestepping must cover the same ground as walking, or the two axes fight
   each other when you move diagonally. *)
let strafing_matches_walking_speed () =
  let strafed = walk world (facing_east ()) ~forward:0. ~strafe:0.5 in
  Alcotest.check vec "sideways, and the same distance" (Vec.make 2. 2.5)
    strafed.Player.pos

(* The two axes are added together, so a diagonal must be clamped back: without
   it, holding forward and strafe at once walks [sqrt 2] times faster than
   either key alone. *)
let a_diagonal_is_no_faster () =
  let start = facing_east () in
  let travelled p = Vec.length (Vec.sub p.Player.pos start.Player.pos) in
  Alcotest.check close "one axis covers the step" 0.5
    (travelled (walk world start ~forward:0.5 ~strafe:0.));
  Alcotest.check close "two axes cover no more" 0.5
    (travelled (walk world start ~forward:0.5 ~strafe:0.5));
  let corner = 0.5 /. Float.sqrt 2. in
  Alcotest.check vec "and it still goes diagonally, evenly split"
    (Vec.make (2. +. corner) (2. +. corner))
    (walk world start ~forward:0.5 ~strafe:0.5).Player.pos

let walls_block_movement () =
  let player =
    List.fold_left
      (fun p _ -> walk world p ~forward:0.5 ~strafe:0.)
      (facing_east ()) (List.init 20 Fun.id)
  in
  Alcotest.(check bool)
    "the player never ends up inside a wall" false
    (Room.blocked room player.Player.pos);
  Alcotest.(check bool)
    "and stops short of the east wall" true
    (player.Player.pos.x <= 4.0 -. Config.collision_padding)

(* Axes are resolved independently, so a blocked direction must not cancel the
   free one — otherwise you stick to walls instead of sliding along. *)
let a_blocked_axis_does_not_block_the_other () =
  let player = facing_east ~pos:(Vec.make 3.7 2.) () in
  Alcotest.check vec "x is blocked, y still moves"
    (Vec.make 3.7 (2. +. (0.5 /. Float.sqrt 2.)))
    (walk world player ~forward:0.5 ~strafe:0.5).Player.pos

(* A diagonal step through a doorway has to resolve its second leg in the room it
   has arrived in. The first leg carries it far enough through that the second no
   longer comes anywhere near the opening, so nothing in the first room can
   vouch for where it goes — and what it would otherwise finish inside is the low
   wall standing just inside the second. *)
let a_diagonal_through_a_doorway_lands_clear () =
  let start = Player.create ~room:0 ~pos:(Vec.make 3.8 2.2) ~angle:0. in
  let moved = walk two_rooms start ~forward:0.5 ~strafe:0.3 in
  Alcotest.(check int) "ends up in the second room" 1 moved.Player.room;
  Alcotest.(check bool)
    "and not inside its wall" false
    (Room.blocked (World.room two_rooms moved.Player.room) moved.Player.pos);
  let scale = 0.5 /. Vec.length (Vec.make 0.5 0.3) in
  Alcotest.check vec "having slid along that wall instead"
    (Vec.make (3.8 +. (0.5 *. scale) -. 4.) 2.2)
    moved.Player.pos

let spawn_uses_the_world () =
  Alcotest.check vec "spawn point" (World.spawn two_rooms).World.pos
    (Player.spawn two_rooms).Player.pos

let walking_through_a_doorway () =
  let start = Player.create ~room:0 ~pos:(Vec.make 3.8 2.) ~angle:0. in
  let crossed = walk two_rooms start ~forward:0.4 ~strafe:0. in
  Alcotest.(check int) "room changes" 1 crossed.room;
  Alcotest.check vec "lands inside" (Vec.make 0.2 2.) crossed.pos;
  Alcotest.check close "dir stays unit" 1. (Vec.length crossed.dir);
  Alcotest.check close "right stays unit" 1. (Vec.length crossed.right);
  Alcotest.check close "basis stays perpendicular" 0.
    (dot crossed.dir crossed.right)

(* Walking through and straight back has to land where it started, in the room
   it started in. The two portals of a link carry a transform and its inverse,
   so anything else means the pair has drifted apart. *)
let walking_through_and_back_returns_you () =
  let start = Player.create ~room:0 ~pos:(Vec.make 3.8 2.) ~angle:0. in
  let there = walk two_rooms start ~forward:0.4 ~strafe:0. in
  Alcotest.(check int) "through" 1 there.Player.room;
  let back = walk two_rooms there ~forward:(-0.4) ~strafe:0. in
  Alcotest.(check int) "and back again" start.Player.room back.Player.room;
  Alcotest.check vec "to where it started" start.Player.pos back.Player.pos;
  Alcotest.check vec "facing the same way" start.Player.dir back.Player.dir;
  Alcotest.check vec "with the same right" start.Player.right back.Player.right

(* A step that rounds a jamb — out through the opening and straight back in —
   crosses the threshold's line but finishes on the side it started, so it must
   not be counted as going through. *)
let rounding_a_jamb_is_not_a_crossing () =
  let player = Player.create ~room:0 ~pos:(Vec.make 3.9 1.9) ~angle:0. in
  let moved = walk two_rooms player ~forward:0. ~strafe:0.1 in
  Alcotest.(check int) "still in the first room" 0 moved.Player.room

(* {1 Traversal traces}

   A movement reports the doorways it went through, in order. What reads that
   list is a game locking the door it has just come through, counting the rooms
   it has seen, and keeping a route home it can walk backwards — so the order
   and the identities have to be exact, not merely the count. *)

let at world ~room ~pos = Player.create ~room ~pos ~angle:0.

(* Most frames go through no doorway at all, and the list has to be empty rather
   than approximately empty. *)
let a_step_that_crosses_nothing_reports_nothing () =
  let moved =
    Player.traverse world (at world ~room:0 ~pos:centre) ~forward:0.5 ~strafe:0.
  in
  Alcotest.(check int) "no crossings" 0 (List.length moved.Player.crossings);
  Alcotest.(check int) "and the same room" 0 moved.Player.player.Player.room

let one_crossing_names_both_sides_of_the_doorway () =
  let start = at two_rooms ~room:0 ~pos:(Vec.make 3.8 2.) in
  let moved = Player.traverse two_rooms start ~forward:0.4 ~strafe:0. in
  match moved.Player.crossings with
  | [ crossing ] ->
      Alcotest.(check int) "out of the first room" 0 crossing.Player.from_room;
      Alcotest.(check int)
        "by its only doorway" 0 crossing.Player.from_threshold;
      Alcotest.(check int) "into the second" 1 crossing.Player.to_room;
      Alcotest.(check int)
        "arriving at the doorway's other side" 0 crossing.Player.to_threshold;
      (* The transform recorded is the one that was actually applied. *)
      Alcotest.check vec "and it is the transform that was used"
        moved.Player.player.Player.pos
        (Transform.point crossing.Player.onto (Vec.make 4.2 2.));
      Alcotest.(check int)
        "the pose agrees about the room" 1 moved.Player.player.Player.room
  | other -> Alcotest.failf "expected one crossing, got %d" (List.length other)

(* One axis-resolved frame is an L, and each of its two legs can go through a
   doorway of its own. The first leg carries far enough into the second room
   that the second leg starts nowhere near the opening it came in by — and there
   meets a different one. *)
let one_frame_can_cross_two_doorways () =
  let start = at loop ~room:0 ~pos:centre in
  let moved = Player.slide loop start (Vec.make 4. 2.5) in
  Alcotest.(check int)
    "two doorways in one step" 2
    (List.length moved.Player.crossings);
  match moved.Player.crossings with
  | [ first; second ] ->
      (* In order: the leg along x went first, so its doorway is first. *)
      Alcotest.(check int) "out of a" 0 first.Player.from_room;
      Alcotest.(check int) "by a's east doorway" 0 first.Player.from_threshold;
      Alcotest.(check int) "into b" 1 first.Player.to_room;
      Alcotest.(check int) "at b's west doorway" 0 first.Player.to_threshold;
      Alcotest.(check int) "then out of b" 1 second.Player.from_room;
      Alcotest.(check int) "by b's north doorway" 1 second.Player.from_threshold;
      Alcotest.(check int) "and back into a" 0 second.Player.to_room;
      Alcotest.(check int) "at a's south doorway" 1 second.Player.to_threshold
  | other -> Alcotest.failf "expected two crossings, got %d" (List.length other)

(* Going round the loop comes back to the room it started in — by a route whose
   two ends do not agree about where that room is. The crossings are what says
   it happened at all: the room index alone would look like a step that never
   left. *)
let a_loop_returns_to_the_room_it_left () =
  let start = at loop ~room:0 ~pos:centre in
  let moved = Player.slide loop start (Vec.make 4. 2.5) in
  Alcotest.(check int)
    "back in the room it set out from" start.Player.room
    moved.Player.player.Player.room;
  Alcotest.(check bool)
    "somewhere else in it, though" true
    (Vec.length (Vec.sub moved.Player.player.Player.pos start.Player.pos) > 1e-6);
  Alcotest.(check int)
    "having gone through two doorways to get there" 2
    (List.length moved.Player.crossings)

(* What a return route is: the crossings walked backwards through the inverse of
   each transform. Every link's two portals carry a transform and its inverse
   exactly, so unwinding the list lands on the pose it started from — however
   impossible the loop it went round. *)
let the_crossings_unwind_to_where_it_started () =
  let start = at loop ~room:0 ~pos:centre in
  let moved = Player.slide loop start (Vec.make 4. 2.5) in
  let home =
    List.fold_left
      (fun pose (crossing : Player.crossing) ->
        Player.through
          (Transform.inverse crossing.Player.onto)
          ~room:crossing.Player.from_room pose)
      moved.Player.player
      (List.rev moved.Player.crossings)
  in
  Alcotest.(check int)
    "the room it set out from" start.Player.room home.Player.room;
  (* Unwinding the frames, not the walking: it lands where the step finished,
     expressed in the frame it began in. *)
  Alcotest.check vec "and the step measured from there"
    (Vec.make (centre.x +. 4.) (centre.y +. 2.5))
    home.Player.pos

(* Two rooms joined at an angle: room b's side of the doorway is room a's,
   carried through a rotation and reversed, so the link's transform is a real
   rotation rather than the translation every other fixture here happens to
   have. Room b is the carried jambs and nothing else, so nothing of its own can
   refuse a step and what a walk through it finds is the doorway alone. *)
let angled radians =
  let carry (v : Vec.t) =
    Vec.add (Vec.rotate v radians) (Vec.make 1.37 (-2.11))
  in
  let a_jambs, east =
    Room.doorway ~name:"east" ~width:1. ~opening:2. ~height:3. ~material:pale
      (Vec.make 4. 0.) (Vec.make 4. 4.)
  in
  let west =
    Room.threshold ~name:"west" ~height:2. (carry east.Room.b)
      (carry east.Room.a)
  in
  let a =
    Room.make ~thresholds:[ east ] ~floor:flat_floor ~ceiling:flat_ceiling
      (a_jambs
      @ [
          Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~material:pale (Vec.make 4. 4.) (Vec.make 0. 4.);
          Room.wall ~height:3. ~material:pale (Vec.make 0. 4.) (Vec.make 0. 0.);
        ])
  and b =
    Room.make ~thresholds:[ west ] ~floor:flat_floor ~ceiling:flat_ceiling
      [
        Room.wall ~height:3. ~material:dim
          (carry (Vec.make 4. 0.))
          (carry (Vec.make 4. 1.5));
        Room.wall ~height:3. ~material:dim
          (carry (Vec.make 4. 2.5))
          (carry (Vec.make 4. 4.));
      ]
  in
  World.make
    ~rooms:[ ("a", a); ("b", b) ]
    ~links:[ (("a", "east"), ("b", "west")) ]
    ~atmosphere:air ~spawn:("a", centre)

(* Going through a doorway once has to mean going through it once, whatever
   angle the two rooms meet at.

   A leg is walked opening by opening, so the part of it left over after a
   crossing sets out standing on the twin of the doorway it just came through —
   at a point that got there by being carried through the link's rotation, and
   which therefore sits on that opening's line only to within a bit or two
   either way. Ask whether that leg goes through the twin by whether its two
   ends fall on different sides and the answer is decided by which way the last
   bit rounded: half the angles throw the player straight back where they came
   from. Ask it by where the leg {e ends} — which is most of an opening's width
   from the line — and the rounding cannot reach the answer. *)
let a_crossing_does_not_double_back_however_the_rooms_meet () =
  List.iter
    (fun radians ->
      let world = angled radians in
      let start = Player.create ~room:0 ~pos:(Vec.make 3.8 2.) ~angle:0. in
      let moved = Player.slide world start (Vec.make 0.4 0.) in
      Alcotest.(check int)
        (Printf.sprintf "one crossing at %.3g radians" radians)
        1
        (List.length moved.Player.crossings);
      Alcotest.(check int)
        (Printf.sprintf "and it stays in the room beyond at %.3g" radians)
        1 moved.Player.player.Player.room)
    [ 0.05; 0.31; 0.7; 1.234; 1.9; 2.4; 2.9; -0.45; -1.77 ]

(* {1 A leg that clears a whole room}

   Three rooms in a line, the middle one narrower than a single step can be, so
   that one leg of a frame passes clean through it and out the far side. That is
   the shape a leg applied in one jump cannot get right: past the first opening
   the room it set out from has nothing left to say about where it went, and
   whatever stands beyond the second — a wall, a shut leaf — is in a room that
   room has never heard of.

   [beyond] stands walls inside the third room; [door] hangs a leaf in the
   second doorway, on both sides at once, since a world refuses a link whose two
   sides disagree about one. *)
let corridor ?door ?(beyond = []) () =
  let gap = 0.3 in
  let cut ?door ~name ~material a b =
    Room.doorway ~name ?door ~width:1. ~opening:2. ~height:3. ~material a b
  in
  let wall material a b = Room.wall ~height:3. ~material a b in
  let first_jambs, first_east =
    cut ~name:"east" ~material:pale (Vec.make 4. 0.) (Vec.make 4. 4.)
  and middle_west_jambs, middle_west =
    cut ~name:"west" ~material:dim (Vec.make 0. 4.) (Vec.make 0. 0.)
  and middle_east_jambs, middle_east =
    cut ?door ~name:"east" ~material:dim (Vec.make gap 0.) (Vec.make gap 4.)
  and last_jambs, last_west =
    cut ?door ~name:"west" ~material:pale (Vec.make 0. 4.) (Vec.make 0. 0.)
  in
  let first =
    Room.make ~thresholds:[ first_east ] ~floor:flat_floor ~ceiling:flat_ceiling
      (first_jambs
      @ [
          wall pale (Vec.make 0. 0.) (Vec.make 4. 0.);
          wall pale (Vec.make 4. 4.) (Vec.make 0. 4.);
          wall pale (Vec.make 0. 4.) (Vec.make 0. 0.);
        ])
  and middle =
    Room.make
      ~thresholds:[ middle_west; middle_east ]
      ~floor:flat_floor ~ceiling:flat_ceiling
      (middle_west_jambs @ middle_east_jambs
      @ [
          wall dim (Vec.make 0. 0.) (Vec.make gap 0.);
          wall dim (Vec.make gap 4.) (Vec.make 0. 4.);
        ])
  and last =
    Room.make ~thresholds:[ last_west ] ~floor:flat_floor ~ceiling:flat_ceiling
      (last_jambs @ beyond
      @ [
          wall pale (Vec.make 0. 0.) (Vec.make 4. 0.);
          wall pale (Vec.make 4. 0.) (Vec.make 4. 4.);
          wall pale (Vec.make 4. 4.) (Vec.make 0. 4.);
        ])
  in
  World.make
    ~rooms:[ ("first", first); ("middle", middle); ("last", last) ]
    ~links:
      [
        (("first", "east"), ("middle", "west"));
        (("middle", "east"), ("last", "west"));
      ]
    ~atmosphere:air ~spawn:("first", centre)

(* Both doorways have to be applied, not just the nearest. Applying only the
   nearest leaves the player holding the middle room's index at a position past
   the far room's doorway — standing in a room they were never carried into, and
   which nothing has measured their step against.

   It is also where an opening the step merely touches must not hide one it
   genuinely goes through. The walk's second part sets out standing on the
   middle room's west threshold, which is nearer than its east one and is not a
   crossing at all; ranked, it would swallow the crossing that is. *)
let a_leg_clears_a_room_and_keeps_going () =
  let world = corridor () in
  let start = at world ~room:0 ~pos:(Vec.make 3.8 2.) in
  let moved = Player.slide world start (Vec.make 0.8 0.) in
  Alcotest.(check int)
    "two doorways in one leg" 2
    (List.length moved.Player.crossings);
  Alcotest.(check int)
    "and it ends in the third room" 2 moved.Player.player.Player.room;
  Alcotest.check vec "just inside its doorway" (Vec.make 0.3 2.)
    moved.Player.player.Player.pos

(* A wall standing inside the third room is in a room the first two thirds of
   the leg were never in. It still has to stop the step: the part of the leg
   that is in that room is measured against that room's walls, and refusing it
   leaves the player in the doorway rather than through the wall. *)
let a_wall_beyond_the_second_doorway_stops_the_step () =
  let world =
    corridor
      ~beyond:
        [
          Room.wall ~height:1. ~material:pale (Vec.make 0.25 1.)
            (Vec.make 0.25 3.);
        ]
      ()
  in
  let start = at world ~room:0 ~pos:(Vec.make 3.8 2.) in
  let moved = Player.slide world start (Vec.make 0.8 0.) in
  let ended = moved.Player.player in
  Alcotest.(check int) "still carried into the third room" 2 ended.Player.room;
  Alcotest.(check bool)
    "stopping short of the wall" true
    (ended.Player.pos.x <= 0.25 -. Config.collision_padding);
  Alcotest.(check bool)
    "and never inside it" false
    (Room.blocked (World.room world ended.Player.room) ended.Player.pos)

(* The same again for a shut leaf, which nothing in the first room can see:
   collision looks one room ahead, and one room ahead of the first is the
   middle. Only resolving the rest of the leg while standing in the middle room
   finds it. *)
let a_shut_door_beyond_the_first_doorway_stops_the_step () =
  let world = corridor ~door:(Door.make ~state:Door.Closed dim) () in
  let start = at world ~room:0 ~pos:(Vec.make 3.8 2.) in
  let moved = Player.slide world start (Vec.make 0.8 0.) in
  Alcotest.(check int)
    "through the first doorway only" 1
    (List.length moved.Player.crossings);
  Alcotest.(check int)
    "left in the middle room" 1 moved.Player.player.Player.room;
  Alcotest.check vec "standing where it came in" (Vec.make 0. 2.)
    moved.Player.player.Player.pos

let () =
  Alcotest.run "Player"
    [
      ( "camera",
        [
          case "basis invariants" camera_basis;
          case "right is actually to the right" right_is_actually_to_the_right;
          case "turning preserves the basis" turning_preserves_the_basis;
          case "pitch tips within a limit" pitch_tips_within_a_limit;
          case "turning does not move the player"
            turning_does_not_move_the_player;
          case "spawn uses the world" spawn_uses_the_world;
        ] );
      ( "movement",
        [
          case "walking follows the facing" walking_follows_the_facing;
          case "strafing matches walking speed" strafing_matches_walking_speed;
          case "a diagonal is no faster" a_diagonal_is_no_faster;
          case "walking through a doorway" walking_through_a_doorway;
          case "walking through and back returns you"
            walking_through_and_back_returns_you;
          case "rounding a jamb is not a crossing"
            rounding_a_jamb_is_not_a_crossing;
        ] );
      ( "traces",
        [
          case "a step that crosses nothing reports nothing"
            a_step_that_crosses_nothing_reports_nothing;
          case "one crossing names both sides of the doorway"
            one_crossing_names_both_sides_of_the_doorway;
          case "one frame can cross two doorways"
            one_frame_can_cross_two_doorways;
          case "a loop returns to the room it left"
            a_loop_returns_to_the_room_it_left;
          case "the crossings unwind to where it started"
            the_crossings_unwind_to_where_it_started;
          case "a leg clears a room and keeps going"
            a_leg_clears_a_room_and_keeps_going;
          case "a crossing does not double back however the rooms meet"
            a_crossing_does_not_double_back_however_the_rooms_meet;
        ] );
      ( "collision",
        [
          case "walls block movement" walls_block_movement;
          case "a blocked axis does not block the other"
            a_blocked_axis_does_not_block_the_other;
          case "a diagonal through a doorway lands clear"
            a_diagonal_through_a_doorway_lands_clear;
          case "a wall beyond the second doorway stops the step"
            a_wall_beyond_the_second_doorway_stops_the_step;
          case "a shut door beyond the first doorway stops the step"
            a_shut_door_beyond_the_first_doorway_stops_the_step;
        ] );
    ]
