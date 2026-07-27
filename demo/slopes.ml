(** {b Inclined floors and ceilings.} A floor is a {!Raycaster.Plane} — an
    elevation [z = ax + by + c] over the room — and so is a roof, so both tilt,
    and independently of each other.

    Two rooms, joined by a doorway. The hall's floor climbs away from you and
    its roof climbs half again as fast, so the room opens up ahead as you walk
    into it. The room beyond is authored in its own coordinate frame, as every
    room is, and its floor is not written out by hand: it is the hall's floor
    put through the transform of the doorway between them, with
    {!Raycaster.Plane.through}. The same surface, in the other room's terms.

    That is what makes the threshold seamless. Two planes written out separately
    with the numbers that look right differ by a few thousandths where they
    meet, which reads as a step you walk into; derive the second from the first
    and {!Raycaster.World.seam_gap} is zero by construction. There is a test
    that says so, for this world and for {!Level}'s. *)

open Raycaster

let height = 5.

(* The transform a link will be given, exactly as {!Raycaster.World.make} is
   about to derive it, so a floor can be carried across a doorway before the
   world that joins the two rooms exists. *)
let across (a : Room.threshold) (b : Room.threshold) =
  Transform.between ~a1:a.Room.a ~a2:a.Room.b ~b1:b.Room.a ~b2:b.Room.b

let world =
  (* The hall runs east and climbs as it goes. *)
  let hall_sw = Vec.make 0. (-5.)
  and hall_se = Vec.make 15. (-5.)
  and hall_ne = Vec.make 15. 5.
  and hall_nw = Vec.make 0. 5. in
  let up_sw = Vec.make 0. (-4.)
  and up_se = Vec.make 9. (-4.)
  and up_ne = Vec.make 9. 4.
  and up_nw = Vec.make 0. 4. in
  let hall_jambs, hall_onward =
    Room.doorway ~name:"onward" ~width:2.6 ~opening:3.2 ~height
      ~material:Surfaces.brick hall_se hall_ne
  and up_jambs, up_back =
    Room.doorway ~name:"back" ~width:2.6 ~opening:3.2 ~height
      ~material:Surfaces.brick up_nw up_sw
  in
  let hall_floor = Plane.make ~a:0.11 ~b:0. ~c:0. in
  let hall_roof = Plane.make ~a:0.17 ~b:0. ~c:height in
  (* Both surfaces carried through the doorway rather than restated. *)
  let onward = across hall_onward up_back in
  let up_floor = Plane.through onward hall_floor
  and up_roof = Plane.through onward hall_roof in
  let stone a b = Room.wall ~height ~material:Surfaces.stone a b in
  let hall =
    Room.make ~thresholds:[ hall_onward ]
      ~floor:{ Room.plane = hall_floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof { Room.plane = hall_roof; material = Surfaces.soffit })
      ~sprites:
        [
          (* Standing on the slope: a sprite's feet are on the floor wherever
             the floor has got to. *)
          Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 6. (-2.));
          Room.sprite ~size:0.9 ~image:Pictures.barrel (Vec.make 11. 2.5);
        ]
      (hall_jambs
      @ [ stone hall_sw hall_se; stone hall_ne hall_nw; stone hall_nw hall_sw ]
      )
  and upper =
    Room.make ~thresholds:[ up_back ]
      ~floor:{ Room.plane = up_floor; material = Surfaces.ground }
      ~ceiling:(Room.Roof { Room.plane = up_roof; material = Surfaces.soffit })
      ~sprites:[ Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 6. 0.) ]
      (up_jambs @ [ stone up_sw up_se; stone up_se up_ne; stone up_ne up_nw ])
  in
  World.make
    ~rooms:[ ("hall", hall); ("upper", upper) ]
    ~links:[ (("hall", "onward"), ("upper", "back")) ]
    ~atmosphere:Surfaces.air
    ~spawn:("hall", Vec.make 2. 0.)

let run () = Engine.run world
