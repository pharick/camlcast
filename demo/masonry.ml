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

open Camlcast

let height = 4.
let flat = Plane.horizontal 0.

(* The four corners. Given to outline they would be four walls of one material;
   given one at a time they are four walls of four, which is what this one is
   about. They still bound the room, so they are written the way outline would
   have wound them. *)
let sw = Vec.make (-6.) (-6.)
let se = Vec.make 6. (-6.)
let ne = Vec.make 6. 6.
let nw = Vec.make (-6.) 6.

let level =
  P.(
    (* Facing the green panel wall, with the red brick running away to the right
       and the blue stone to the left: three of the four are in shot at once. *)
    world ~atmosphere:Surfaces.air
      ~spawn:("room", Vec.make (-4.5) 0.)
      [
        room ~name:"room"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
          [
            wall ~height ~material:Surfaces.brick sw se;
            wall ~height ~material:Surfaces.panel se ne;
            wall ~height ~material:Surfaces.stone ne nw;
            wall ~height ~material:Surfaces.tile nw sw;
            (* An oak post off to one side, so the four walls are also seen at a
               glancing angle, where the directional light falls differently. *)
            boundary ~height:2.6 ~material:Surfaces.oak
              (polygon ~center:(Vec.make 2.5 3.5) ~radius:0.7 ~sides:4
                 ~rotation:0.4);
          ];
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
