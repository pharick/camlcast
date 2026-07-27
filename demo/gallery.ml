(** {b Decals and sprites.} Two ways of putting a picture in a room, and the
    difference between them.

    A {b decal} is fixed to a wall: it has a position along that wall, a height
    up it, and a size, and it is drawn as part of the wall in the same pass —
    turn side-on to it and it foreshortens with the masonry it is painted over.

    A {b sprite} stands in the room instead of on a wall. It has a position on
    the floor and a size, it always faces you, and it is composited by depth
    after the walls are done, so it goes behind what is in front of it and in
    front of what is not.

    Both carry an {!Raycaster.Image}, which has its own colours and its own
    alpha — unlike a {!Raycaster.Texture}, which is greyscale and takes its
    colour from the material wearing it. The sprites here are cut out against
    {!Raycaster.Image.clear}, so only the figure is drawn and not the box it
    came in. *)

open Raycaster

let height = 4.

let world =
  let sw = Vec.make (-7.) (-5.)
  and se = Vec.make 7. (-5.)
  and ne = Vec.make 7. 5.
  and nw = Vec.make (-7.) 5. in
  let plain a b = Room.wall ~height ~material:Surfaces.brick a b in
  let floor = Plane.horizontal 0. in
  (* The hung wall. [along] is measured from the wall's first endpoint and [z]
     up from the floor, so a decal is placed in the wall's own terms and stays
     put if the room around it moves. *)
  let hung =
    Room.wall ~height ~material:Surfaces.panel se ne
      ~decals:
        [
          {
            Room.along = 3.;
            z = 1.7;
            half_width = 1.;
            half_height = 1.;
            image = Pictures.painting;
          };
          {
            Room.along = 6.5;
            z = 1.7;
            half_width = 0.8;
            half_height = 1.;
            image = Pictures.poster;
          };
        ]
  in
  let room =
    Room.make
      ~floor:{ Room.plane = floor; material = Surfaces.ground }
      ~ceiling:
        (Room.Roof
           { Room.plane = Plane.above floor height; material = Surfaces.soffit })
      ~sprites:
        [
          (* Two of them one behind the other, to be walked around: the near one
             hides the far one and neither is ever seen edge-on. *)
          Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 0. 1.8);
          Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 2.5 (-1.2));
          Room.sprite ~size:0.9 ~image:Pictures.barrel (Vec.make (-2.) (-2.5));
        ]
      [ plain sw se; hung; plain ne nw; plain nw sw ]
  in
  (* Facing the hung wall down the length of the room, with the sprites between
     you and it. *)
  World.make ~rooms:[ ("room", room) ] ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("room", Vec.make (-5.) 0.)

let run () = Engine.run world
