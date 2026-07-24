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
  World.make ~spawn:(Vec.make 2. 2.) ~floor:flat_floor ~ceiling:flat_ceiling
    [
      World.wall ~height:3. ~texture:1 (Vec.make 0. 0.) (Vec.make 4. 0.);
      World.wall ~height:3. ~texture:1 (Vec.make 4. 0.) (Vec.make 4. 4.);
      World.wall ~height:3. ~texture:1 (Vec.make 4. 4.) (Vec.make 0. 4.);
      World.wall ~height:3. ~texture:1 (Vec.make 0. 4.) (Vec.make 0. 0.);
    ]

(** The same room with a short free-standing wall of texture id 2, one cell east
    of the centre, spanning the ray fired east from it. Used to check that a ray
    keeps the walls behind a near one. *)
let room_with_pillar =
  World.make ~spawn:(Vec.make 2. 2.) ~floor:flat_floor ~ceiling:flat_ceiling
    (Array.to_list room.World.walls
    @ [ World.wall ~height:1. ~texture:2 (Vec.make 3. 1.5) (Vec.make 3. 2.5) ])

(** Centre of the room, 2 cells from every wall. *)
let centre = Vec.make 2. 2.

let dot (a : Vec.t) (b : Vec.t) = (a.x *. b.x) +. (a.y *. b.y)

(** The nearest wall a ray meets. [Ray.cast] returns every wall the ray crosses;
    most tests only care about the closest one. *)
let nearest_hit world ~origin ~direction =
  Option.get (Ray.nearest (Ray.cast world ~origin ~direction))
