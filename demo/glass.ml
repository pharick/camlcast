(** {b See-through walls.} A steel grille and a leaded window, each with a
    figure standing behind it.

    A wall is see-through when the pattern its material wears carries an alpha:
    {!Patterns.bars} and {!Patterns.glass} are built with
    {!Camlcast_core.Texture.generate_masked}, and nothing else here is. The
    renderer draws the opaque walls first, then composites the translucent ones
    back to front along with the sprites — which is why the figure behind the
    grille is seen through it and the barrel in front of it is not. In the
    grille, the bars are opaque and the gaps are not, so one wall does both at
    once. *)

open Camlcast

let height = 4.
let flat = Plane.horizontal 0.

let level =
  P.(
    world ~atmosphere:Surfaces.air
      [
        room ~height ~material:Surfaces.stone
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
            (* A plain box outline: four corners say all of it. *)
          ~outline:
            (corners
               [
                 Vec.make (-8.) (-6.);
                 Vec.make 8. (-6.);
                 Vec.make 8. 6.;
                 Vec.make (-8.) 6.;
               ])
          [
            spawn (Vec.make (-5.5) 0.);
            (* A screen across the room in two halves, bars on one side and
               leaded glass on the other, with a walkable gap between them. It
               stands square across the spawn's facing direction. *)
            wall ~height:2.6 ~material:Surfaces.grille (Vec.make 0. (-6.))
              (Vec.make 0. (-1.6));
            wall ~height:2.6 ~material:Surfaces.window (Vec.make 0. 1.6)
              (Vec.make 0. 6.);
            (* One sprite behind each half of the screen and one in front: seen
               through bars, through glass, and directly. *)
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
