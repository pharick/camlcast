(** {b Decals and sprites.} Two ways of putting a picture in a room.

    A {b decal} is fixed to a wall: it has a position along that wall, a height
    up it, and a size, and is drawn as part of the wall in the same pass, so it
    foreshortens with the wall when viewed side-on.

    A {b sprite} stands in the room: it has a floor position and a size, always
    faces the camera, and is composited by depth after the walls, so it goes
    behind what is in front of it and in front of what is not.

    Both carry an {!Camlcast_core.Image} — colour and alpha at whatever shape it
    was drawn — unlike a {!Camlcast_core.Texture}, which is square and tiles a
    world cell because it is part of a surface rather than a thing in its own
    right. The sprites here are cut out against {!Camlcast_core.Image.clear}, so
    only the figure is drawn and not the box around it. *)

open Camlcast

let height = 4.
let flat = Plane.horizontal 0.
let sw = Vec.make (-7.) (-5.)
let se = Vec.make 7. (-5.)
let ne = Vec.make 7. 5.
let nw = Vec.make (-7.) 5.

let level =
  P.(
    (* Spawn faces the hung wall down the length of the room, sprites in
       between. *)
    world ~atmosphere:Surfaces.air
      [
        room ~height ~material:Surfaces.brick
          ~floor:(floor ~plane:flat Surfaces.ground)
          ~ceiling:(roof ~plane:(Plane.above flat height) Surfaces.soffit)
          ~outline:
            [
              corner sw;
              (* The hung wall with its decals. [along] is measured from the
                 wall's first endpoint and [z] up from the floor, so a decal is
                 placed in the wall's own terms and stays put if the room
                 around it moves. *)
              corner se ~material:Surfaces.panel
                ~decals:
                  [
                    decal ~along:3. ~z:1.7 ~half_width:1. ~half_height:1.
                      Pictures.painting;
                    decal ~along:6.5 ~z:1.7 ~half_width:0.8 ~half_height:1.
                      Pictures.poster;
                  ];
              corner ne;
              corner nw;
            ]
          [
            spawn (Vec.make (-5.) 0.);
            (* Two figures one behind the other: the near one hides the far
               one, and neither is ever seen edge-on. *)
            sprite ~key:"near" ~size:1.8 ~image:Pictures.figure
              (Vec.make 0. 1.8);
            sprite ~key:"far" ~size:1.8 ~image:Pictures.figure
              (Vec.make 2.5 (-1.2));
            sprite ~key:"barrel" ~size:0.9 ~image:Pictures.barrel
              (Vec.make (-2.) (-2.5));
          ];
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
