(** {b Materials.} One room, four walls, four materials.

    A pattern carries its own colours, so a wall can have more than one: the
    brick here is red and the mortar between it grey, not a paler red.

    One pattern still dresses many colours, because a {!Patterns} pattern takes
    them as arguments before its texel coordinates: the floor checker and the
    yellow tile are the same function with different colours. Nothing in the
    room varies but the material.

    The patterns themselves are pure functions of a texel coordinate, in
    {!Patterns}; the materials that fill their colours in are in {!Surfaces}. *)

open Camlcast

let height = 4.
let flat = Plane.horizontal 0.

(* The four corners. Given to outline they would be four walls of one
   material; given one at a time they are four walls of four. They still bound
   the room, so they are wound the way outline would wind them. *)
let sw = Vec.make (-6.) (-6.)
let se = Vec.make 6. (-6.)
let ne = Vec.make 6. 6.
let nw = Vec.make (-6.) 6.

let level =
  P.(
    (* Spawn faces the green panel wall, with the red brick to the right and
       the blue stone to the left: three of the four in shot at once. *)
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
            (* An oak post off to one side, so the walls are also seen at a
               glancing angle, where the directional light falls differently. *)
            boundary ~height:2.6 ~material:Surfaces.oak
              (polygon ~center:(Vec.make 2.5 3.5) ~radius:0.7 ~sides:4
                 ~rotation:0.4);
          ];
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
