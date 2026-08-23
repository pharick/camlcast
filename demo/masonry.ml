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

(* The four corners. A corner says how the wall {e leaving} it is made, so four
   of them carrying four materials is four walls of four — and it is still one
   outline, so it is still wound for us. *)
let sw = Vec.make (-6.) (-6.)
let se = Vec.make 6. (-6.)
let ne = Vec.make 6. 6.
let nw = Vec.make (-6.) 6.

let level =
  P.(
    (* Spawn faces the green panel wall, with the red brick to the right and
       the blue stone to the left: three of the four in shot at once. *)
    world ~atmosphere:Surfaces.air
      [
        room ~height ~material:Surfaces.stone
          ~floor:(floor ~plane:flat Surfaces.ground)
          ~ceiling:(roof ~plane:(Plane.above flat height) Surfaces.soffit)
          ~outline:
            [
              corner sw ~material:Surfaces.brick;
              corner se ~material:Surfaces.panel;
              corner ne ~material:Surfaces.stone;
              corner nw ~material:Surfaces.tile;
            ]
          [
            spawn (Vec.make (-4.5) 0.);
            (* An oak post off to one side, so the walls are also seen at a
               glancing angle, where the directional light falls differently. *)
            block ~height:2.6 ~material:Surfaces.oak
              (polygon ~center:(Vec.make 2.5 3.5) ~radius:0.7 ~sides:4
                 ~rotation:0.4);
          ];
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
