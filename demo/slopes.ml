(** {b Inclined floors and ceilings.} A floor is a {!Camlcast_core.Plane} — an
    elevation [z = ax + by + c] over the room — and so is a roof; both tilt,
    independently of each other.

    Two rooms, joined by a doorway. The hall's floor climbs and its roof climbs
    half again as fast. The room beyond is authored in its own coordinate frame,
    as every room is, and its floor is not written out by hand: it is the hall's
    floor put through the transform of the doorway between them, with
    {!Camlcast_core.Plane.through} — the same surface in the other room's terms.

    That makes the threshold seamless. Two planes written out separately differ
    by a few thousandths where they meet, which reads as a step; derive the
    second from the first and {!Camlcast_core.World.seam_gap} is zero by
    construction. A test asserts this for this world and for {!Level}'s. *)

open Camlcast

let height = 5.

(* The hall runs east and climbs as it goes. *)
let hall_sw = Vec.make 0. (-5.)
let hall_se = Vec.make 15. (-5.)
let hall_ne = Vec.make 15. 5.
let hall_nw = Vec.make 0. 5.
let up_sw = Vec.make 0. (-4.)
let up_se = Vec.make 9. (-4.)
let up_ne = Vec.make 9. 4.
let up_nw = Vec.make 0. 4.
let hall_floor = Plane.make ~a:0.11 ~b:0. ~c:0.
let hall_roof = Plane.make ~a:0.17 ~b:0. ~c:height
let width = 2.6

(* The way up, made once and cut into both rooms. Its two sides are the same
   opening, which is what the connection at the foot of this file says. *)
let onward = P.door ~width ~clearance:3.2 ()
let back = P.door ~width ~clearance:3.2 ()

(* The upper floor is not here, because it is not written any more: a room that
   gives its floor no plane takes its neighbour's through the connection between
   them, by the transform the opening implies.

   The roof still is. Plane.above would carry the floor's own slope up bodily
   for a ceiling of fixed headroom, and this one has a steeper slope of its own
   so that the way up gains headroom as it climbs. Nothing can derive an intent
   like that, so it is carried by hand — which is what P.through is left for. *)
let up_roof =
  P.through
    ~from:(P.opening ~width hall_se hall_ne)
    ~into:(P.opening ~width up_nw up_sw)
    hall_roof

let level =
  P.(
    world ~atmosphere:Surfaces.air
      [
        room ~height ~material:Surfaces.stone
          ~floor:(floor ~plane:hall_floor Surfaces.ground)
          ~ceiling:(roof ~plane:hall_roof Surfaces.soffit)
            (* The leg the way up is cut into is brick where the rest is
               stone, so the jambs either side of the opening are too: they are
               legs of this outline like any other. *)
          ~outline:
            [
              corner hall_sw;
              corner hall_se ~material:Surfaces.brick;
              corner hall_ne;
              corner hall_nw;
            ]
          [
            spawn (Vec.make 2. 0.);
            cut onward ~along:(hall_se, hall_ne);
            (* On the slope: a sprite's feet sit on the floor at whatever
               height it has reached. *)
            sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
              (Vec.make 6. (-2.));
            sprite ~key:"barrel" ~size:0.9 ~image:Pictures.barrel
              (Vec.make 11. 2.5);
          ];
        room ~height ~material:Surfaces.stone ~floor:(floor Surfaces.ground)
          ~ceiling:(roof ~plane:up_roof Surfaces.soffit)
          ~outline:
            [
              corner up_sw;
              corner up_se;
              corner up_ne;
              corner up_nw ~material:Surfaces.brick;
            ]
          [
            cut back ~along:(up_nw, up_sw);
            sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
              (Vec.make 6. 0.);
          ];
        connect onward back;
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
