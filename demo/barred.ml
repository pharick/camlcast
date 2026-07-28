(** {b Doors you can see through.} A steel grille across one doorway, a leaded
    transom over another, and the room behind each of them.

    A leaf hung in a doorway is drawn from its own material, and a material is
    see-through when the pattern it wears carries an alpha — exactly as for a
    wall. So a door can be shut and transparent at once, and the two halves of
    that are answered separately: the renderer draws the room beyond behind the
    leaf and composites the leaf over it, while {!Camlcast.Room.shut} goes on
    refusing the step. Walk into either of these and you stop against a door you
    are looking through.

    - The {b left} doorway has a grille hung in it. The bars are solid and the
      gaps between them are not, so the chamber behind — and the figure standing
      in it — is there to be seen through a door that is shut.
    - The {b right} doorway has a solid oak door with a {b transom} over it: a
      strip of leaded glass in the wall above the opening. The door hides its
      chamber and the glass does not, and that chamber is open to the sky.

    The transom is why the wall above an opening carries a material of its own.
    {!Camlcast.Room.doorway} cuts a wall and gives the jambs and the strip it
    leaves standing overhead the same one, which is the common case and not this
    one, so {!cut} below does the cutting instead. *)

open Camlcast

let height = 4.5
let opening = 2.6
let width = 2.4
let grille = Door.make Surfaces.grille
let oak = Door.make Surfaces.oak

(** A doorway with a leaf in it and a lintel of its own material — everything
    {!Camlcast.Room.doorway} does, except that the strip above the opening is
    not made of the wall it was cut into. The wall is split about its middle so
    that both jambs keep the boundary's winding, which is what the link between
    two rooms is derived from. *)
let cut name ~door ~transom a b =
  let edge = Vec.sub b a in
  let half = Vec.scale edge (width /. (2. *. Vec.length edge)) in
  let middle = Vec.scale (Vec.add a b) 0.5 in
  let p = Vec.sub middle half and q = Vec.add middle half in
  ( [
      Room.wall ~height ~material:Surfaces.brick a p;
      Room.wall ~height ~material:Surfaces.brick q b;
    ],
    Room.threshold ~name ~door ~height:opening
      ~lintel:{ Room.top = height; material = transom }
      p q )

(* The hall you arrive in, its east wall cut twice: the grille to the south, the
   glazed door to the north. Cut south to north so the wall's winding is
   unbroken. *)
let hall =
  let sw = Vec.make (-12.) (-7.)
  and se = Vec.make 0. (-7.)
  and ne = Vec.make 0. 7.
  and nw = Vec.make (-12.) 7. in
  let barred_jambs, barred =
    cut "barred" ~door:grille ~transom:Surfaces.brick se (Vec.make 0. 0.)
  and glazed_jambs, glazed =
    cut "glazed" ~door:oak ~transom:Surfaces.window (Vec.make 0. 0.) ne
  in
  let wall a b = Room.wall ~height ~material:Surfaces.stone a b in
  let floor = Plane.horizontal 0. in
  Room.make ~thresholds:[ barred; glazed ]
    ~floor:{ Room.plane = floor; material = Surfaces.ground }
    ~ceiling:
      (Room.Roof
         { Room.plane = Plane.above floor height; material = Surfaces.soffit })
    (List.concat
       [ barred_jambs; glazed_jambs; [ wall sw se; wall ne nw; wall nw sw ] ])

(* What is behind each of them. Both sides of a link must agree about the door,
   so each chamber hangs its own copy of whatever the hall hangs — and wears the
   same transom, so that the view back out is the view in. *)
let chamber ~door ~transom ~ceiling ~sprites =
  let sw = Vec.make 0. (-3.5)
  and se = Vec.make 6. (-3.5)
  and ne = Vec.make 6. 3.5
  and nw = Vec.make 0. 3.5 in
  let jambs, back = cut "back" ~door ~transom nw sw in
  let wall a b = Room.wall ~height ~material:Surfaces.brick a b in
  let floor = Plane.horizontal 0. in
  Room.make ~thresholds:[ back ]
    ~floor:{ Room.plane = floor; material = Surfaces.ground }
    ~ceiling ~sprites
    (jambs @ [ wall sw se; wall se ne; wall ne nw ])

let roofed =
  Room.Roof
    {
      Room.plane = Plane.above (Plane.horizontal 0.) height;
      material = Surfaces.soffit;
    }

let world =
  World.make
    ~rooms:
      [
        ("hall", hall);
        ( "behind-the-bars",
          chamber ~door:grille ~transom:Surfaces.brick ~ceiling:roofed
            ~sprites:
              (* Close to the bars, and so out of the fog: the point of this
                 room is that you can see what is standing in it. *)
              [ Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 2. 0.) ]
        );
        (* Open to the sky, which is what the glass over the door shows and the
           door itself does not. *)
        ( "behind-the-glass",
          chamber ~door:oak ~transom:Surfaces.window
            ~ceiling:(Room.Open Surfaces.day) ~sprites:[] );
      ]
    ~links:
      [
        (("hall", "barred"), ("behind-the-bars", "back"));
        (("hall", "glazed"), ("behind-the-glass", "back"));
      ]
    ~atmosphere:Surfaces.air
    ~spawn:("hall", Vec.make (-7.) 0.)

let run () = Engine.enter world
