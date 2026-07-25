(** Testables and fixtures shared by the per-module suites. *)

open Raycaster

let case name body = Alcotest.test_case name `Quick body

(** Floating point comparisons need slack: the maths goes through [cos], [tan]
    and a few divisions, none of which are exact. *)
let close = Alcotest.float 1e-9

let vec =
  Alcotest.testable
    (fun ppf (v : Vec.t) -> Format.fprintf ppf "(%g, %g)" v.x v.y)
    (fun (a : Vec.t) (b : Vec.t) ->
      Float.abs (a.x -. b.x) <= 1e-9 && Float.abs (a.y -. b.y) <= 1e-9)

let color =
  Alcotest.testable
    (fun ppf (c : Color.t) -> Format.fprintf ppf "#%02x%02x%02x" c.r c.g c.b)
    ( = )

(** {1 Fixtures} *)

let flat_floor = Plane.horizontal 0.
let flat_ceiling = Some (Plane.horizontal 3.)

(** A square room, 4 x 4, its four walls given counter-clockwise so [along]
    grows predictably. Small enough that every expected distance is obvious:
    from the centre each wall is exactly 2 cells away. *)
let room =
  Room.make ~floor:flat_floor ~ceiling:flat_ceiling
    [
      Room.wall ~height:3. ~texture:1 (Vec.make 0. 0.) (Vec.make 4. 0.);
      Room.wall ~height:3. ~texture:1 (Vec.make 4. 0.) (Vec.make 4. 4.);
      Room.wall ~height:3. ~texture:1 (Vec.make 4. 4.) (Vec.make 0. 4.);
      Room.wall ~height:3. ~texture:1 (Vec.make 0. 4.) (Vec.make 0. 0.);
    ]

(** The same room with a short free-standing wall of texture id 2, one cell east
    of the centre, spanning the ray fired east from it. Used to check that a ray
    keeps the walls behind a near one. *)
let room_with_pillar =
  Room.make ~floor:flat_floor ~ceiling:flat_ceiling
    (Array.to_list room.Room.walls
    @ [ Room.wall ~height:1. ~texture:2 (Vec.make 3. 1.5) (Vec.make 3. 2.5) ])

(** The same room as the only room of a world, for the suites that need a
    {!World.t} but nothing to do with doorways. *)
let world =
  World.make ~rooms:[ ("room", room) ] ~links:[] ~spawn:("room", Vec.make 2. 2.)

(** Two 4 x 4 rooms joined through a doorway one cell wide, each authored in its
    own coordinates so both rooms occupy [0..4] squared and the link's transform
    has real work to do. The first is roofed and the second open to the sky, so
    a test can tell which room a thing came from.

    Both boundaries are wound counter-clockwise and both doorways are cut with
    {!Room.doorway}, which is the winding rule {!Transform.between} relies on;
    the gaps land at [y = 1.5 .. 2.5] of the first room's east wall and of the
    second's west wall, and the transform between them is a translation by
    [(-4, 0)].

    The second room has a short wall standing just inside its doorway. The two
    rooms' jambs are collinear — they are the same opening — so that wall is the
    only thing in either room that the {e other} one cannot see, which is what
    makes it possible to test that collision consults the neighbour at all. *)
let two_rooms =
  let first_jambs, east =
    Room.doorway ~name:"east" ~width:1. ~opening:2. ~height:3. ~texture:1
      (Vec.make 4. 0.) (Vec.make 4. 4.)
  and second_jambs, west =
    Room.doorway ~name:"west" ~width:1. ~opening:2. ~height:3. ~texture:2
      (Vec.make 0. 4.) (Vec.make 0. 0.)
  in
  let first =
    Room.make ~thresholds:[ east ] ~floor:flat_floor ~ceiling:flat_ceiling
      (first_jambs
      @ [
          Room.wall ~height:3. ~texture:1 (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~texture:1 (Vec.make 4. 4.) (Vec.make 0. 4.);
          Room.wall ~height:3. ~texture:1 (Vec.make 0. 4.) (Vec.make 0. 0.);
        ])
  and second =
    Room.make ~thresholds:[ west ] ~floor:flat_floor ~ceiling:None
      (second_jambs
      @ [
          Room.wall ~height:3. ~texture:2 (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~texture:2 (Vec.make 4. 0.) (Vec.make 4. 4.);
          Room.wall ~height:3. ~texture:2 (Vec.make 4. 4.) (Vec.make 0. 4.);
          (* Just inside the doorway, and invisible to the first room. *)
          Room.wall ~height:1. ~texture:2 (Vec.make 0.25 2.45)
            (Vec.make 1.2 2.45);
        ])
  in
  World.make ~rooms:[ ("first", first); ("second", second) ]
    ~links:[ (("first", "east"), ("second", "west")) ]
    ~spawn:("first", Vec.make 2. 2.)

(** Centre of the room, 2 cells from every wall. *)
let centre = Vec.make 2. 2.

let dot (a : Vec.t) (b : Vec.t) = (a.x *. b.x) +. (a.y *. b.y)

(** The nearest wall a ray meets. [Ray.cast] returns every wall the ray crosses;
    most tests only care about the closest one. *)
let nearest_hit world ~origin ~direction =
  Option.get (Ray.nearest (Ray.cast world ~origin ~direction))
