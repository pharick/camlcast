(** {b Materials.} One room, four walls, four materials.

    A pattern carries its own colours, so a wall can have more than one in it:
    the brick here is red and the mortar between it is grey, which is not a
    paler red. Look closely at a course line.

    One pattern still dresses many colours, because a {!Patterns} pattern takes
    them as arguments before its texel coordinates. The checker under your feet
    and the yellow tile behind you are the same function twice, a colour apart.
    Stand in the middle and turn round — nothing here varies but the material.

    The patterns themselves are pure functions of a texel coordinate, in
    {!Patterns}; the materials that fill their colours in are in {!Surfaces}. *)

open Raycaster

let height = 4.

let world =
  (* Counterclockwise, so that every wall's normal faces into the room. *)
  let sw = Vec.make (-6.) (-6.)
  and se = Vec.make 6. (-6.)
  and ne = Vec.make 6. 6.
  and nw = Vec.make (-6.) 6. in
  let wall material a b = Room.wall ~height ~material a b in
  let floor = Plane.horizontal 0. in
  let room =
    Room.make
      ~floor:{ Room.plane = floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.above floor height; material = Surfaces.soffit })
      (wall Surfaces.brick sw se :: wall Surfaces.panel se ne
     :: wall Surfaces.stone ne nw :: wall Surfaces.tile nw sw
      (* An oak post off to one side, so the four walls are also seen at a
         glancing angle, where the directional light falls differently. *)
      :: Room.regular_polygon ~center:(Vec.make 2.5 3.5) ~radius:0.7 ~sides:4
           ~rotation:0.4 ~height:2.6 ~material:Surfaces.oak)
  in
  (* Facing the green panel wall, with the red brick running away to the right
     and the blue stone to the left: three of the four are in shot at once. *)
  World.make ~rooms:[ ("room", room) ] ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("room", Vec.make (-4.5) 0.)

let run () = Engine.run world
