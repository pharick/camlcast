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

(* Both surfaces are carried through the doorway rather than restated. Two
   rooms have no coordinates in common, so an upper floor written by hand will
   drift; derived, it cannot, and Check finds no step in the floor at the
   threshold. *)
let from = P.opening ~width hall_se hall_ne
let into = P.opening ~width up_nw up_sw
let up_floor = P.through ~from ~into hall_floor
let up_roof = P.through ~from ~into hall_roof

let level =
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:("hall", Vec.make 2. 0.)
      [
        room ~name:"hall"
          ~floor:(floor ~plane:hall_floor ~material:Surfaces.ground)
          ~ceiling:(roof ~plane:hall_roof ~material:Surfaces.soffit)
          [
            boundary ~closed:false ~height ~material:Surfaces.stone
              (corners [ hall_ne; hall_nw; hall_sw; hall_se ]);
            doorway ~name:"onward" ~width ~opening:3.2 ~height
              ~material:Surfaces.brick hall_se hall_ne;
            (* On the slope: a sprite's feet sit on the floor at whatever
               height it has reached. *)
            sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
              (Vec.make 6. (-2.));
            sprite ~key:"barrel" ~size:0.9 ~image:Pictures.barrel
              (Vec.make 11. 2.5);
          ];
        room ~name:"upper"
          ~floor:(floor ~plane:up_floor ~material:Surfaces.ground)
          ~ceiling:(roof ~plane:up_roof ~material:Surfaces.soffit)
          [
            boundary ~closed:false ~height ~material:Surfaces.stone
              (corners [ up_sw; up_se; up_ne; up_nw ]);
            doorway ~name:"back" ~width ~opening:3.2 ~height
              ~material:Surfaces.brick up_nw up_sw;
            sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
              (Vec.make 6. 0.);
          ];
        link ("hall", "onward") ("upper", "back");
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
