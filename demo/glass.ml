(** {b See-through walls.} A steel grille and a leaded window, each with someone
    standing behind it.

    A wall is see-through when the pattern its material wears carries an alpha —
    {!Patterns.bars} and {!Patterns.glass} are built with
    {!Camlcast_core.Texture.generate_masked}, and nothing else here is. The
    renderer draws the opaque walls first and then composites the translucent
    ones back to front along with the sprites, which is why the figure behind
    the grille is seen through it and the barrel in front of it is not.

    Walk up to the grille: the bars are opaque and the gaps are not, so one wall
    is doing both at once. Then walk through the gap between the two halves and
    look back through them from the other side. *)

open Camlcast

let height = 4.
let flat = Plane.horizontal 0.

let level =
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:("room", Vec.make (-5.5) 0.)
      [
        room ~name:"room"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
          [
            (* A plain box boundary: four corners say all of it. *)
            outline ~height ~material:Surfaces.stone
              [
                Vec.make (-8.) (-6.);
                Vec.make 8. (-6.);
                Vec.make 8. 6.;
                Vec.make (-8.) 6.;
              ];
            (* A screen across the room in two halves, bars on one side and
               leaded glass on the other, with a gap between them to walk
               through. It stands square across the way you are facing when you
               arrive. *)
            wall ~height:2.6 ~material:Surfaces.grille (Vec.make 0. (-6.))
              (Vec.make 0. (-1.6));
            wall ~height:2.6 ~material:Surfaces.window (Vec.make 0. 1.6)
              (Vec.make 0. 6.);
            (* One behind each half of the screen, and one in front of it: the
               three are seen through bars, through glass, and plainly. *)
            sprite ~key:"barred" ~size:1.8 ~image:Pictures.figure
              (Vec.make 4. (-3.5));
            sprite ~key:"glazed" ~size:1.8 ~image:Pictures.figure
              (Vec.make 4. 3.5);
            sprite ~key:"plain" ~size:0.9 ~image:Pictures.barrel
              (Vec.make (-3.) (-2.5));
          ];
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
