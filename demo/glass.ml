(** {b See-through walls.} A steel grille and a leaded window, each with
    someone standing behind it.

    A wall is see-through when the pattern its material wears carries an alpha —
    {!Patterns.bars} and {!Patterns.glass} are built with
    {!Raycaster.Texture.generate_masked}, and nothing else here is. The renderer
    draws the opaque walls first and then composites the translucent ones back
    to front along with the sprites, which is why the figure behind the grille
    is seen through it and the barrel in front of it is not.

    Walk up to the grille: the bars are opaque and the gaps are not, so one wall
    is doing both at once. Then walk through the gap between the two halves and
    look back through them from the other side. *)

open Raycaster

let height = 4.

let world =
  let sw = Vec.make (-8.) (-6.)
  and se = Vec.make 8. (-6.)
  and ne = Vec.make 8. 6.
  and nw = Vec.make (-8.) 6. in
  let solid a b = Room.wall ~height ~material:Surfaces.stone a b in
  let floor = Plane.horizontal 0. in
  (* A screen across the room in two halves, bars on one side and leaded glass
     on the other, with a gap between them to walk through. It stands square
     across the way you are facing when you arrive. *)
  let screen =
    [
      Room.wall ~height:2.6 ~material:Surfaces.grille (Vec.make 0. (-6.))
        (Vec.make 0. (-1.6));
      Room.wall ~height:2.6 ~material:Surfaces.window (Vec.make 0. 1.6)
        (Vec.make 0. 6.);
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
          (* One behind each half of the screen, and one in front of it: the
             three are seen through bars, through glass, and plainly. *)
          { Room.pos = Vec.make 4. (-3.5); size = 1.8; image = Pictures.figure };
          { Room.pos = Vec.make 4. 3.5; size = 1.8; image = Pictures.figure };
          { Room.pos = Vec.make (-3.) (-2.5); size = 0.9; image = Pictures.barrel };
        ]
      ([ solid sw se; solid se ne; solid ne nw; solid nw sw ] @ screen)
  in
  World.make ~rooms:[ ("room", room) ] ~links:[] ~atmosphere:Surfaces.air
    ~spawn:("room", Vec.make (-5.5) 0.)

let run () = Engine.run world
