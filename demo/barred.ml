(** {b Doors you can see through.} A steel grille across one doorway, a leaded
    transom over another, and the room behind each of them.

    A leaf hung in a doorway is drawn from its own material, and a material is
    see-through when the pattern it wears carries an alpha — exactly as for a
    wall. So a door can be shut and transparent at once, and the two halves of
    that are answered separately: the renderer draws the room beyond behind the
    leaf and composites the leaf over it, while {!Camlcast_core.Room.shut} goes
    on refusing the step. Walk into either of these and you stop against a door
    you are looking through.

    - The {b left} doorway has a grille hung in it. The bars are solid and the
      gaps between them are not, so the chamber behind — and the figure standing
      in it — is there to be seen through a door that is shut.
    - The {b right} doorway has a solid oak door with a {b transom} over it: a
      strip of leaded glass in the wall above the opening. The door hides its
      chamber and the glass does not, and that chamber is open to the sky.

    The transom is why the wall above an opening carries a material of its own.
    {!Camlcast_core.Room.doorway} cuts a wall and gives the jambs and the strip
    it leaves standing overhead the same one, which is the common case and not
    this one, so {!cut} below does the cutting instead. *)

open Camlcast

let height = 4.5

(* Not `opening`: a local open of P puts P.opening in scope, and a wall's
   clearance and the arithmetic that places one are two different things. *)
let clearance = 2.6
let width = 2.4
let grille = Door.make Surfaces.grille
let oak = Door.make Surfaces.oak
let flat = Plane.horizontal 0.

(** A doorway with a leaf in it and a lintel of its own material — everything
    {!Camlcast.P.doorway} does, except that the strip above the opening is not
    made of the wall it was cut into. Which is the one thing doorway will not
    do, and the reason {!Camlcast.P.threshold} exists.

    {!Camlcast.P.opening} works out where the two ends land, so the arithmetic
    that places an opening is written once, in the engine, and read back here
    rather than restated. The wall is split about its middle so that both jambs
    keep the boundary's winding, which is what the link between two rooms is
    derived from. *)
let cut ~name ~door ~transom a b =
  let p, q = P.opening ~width a b in
  P.
    [
      wall ~height ~material:Surfaces.brick a p;
      wall ~height ~material:Surfaces.brick q b;
      threshold ~name ~door ~height:clearance
        ~lintel:{ top = height; material = transom }
        p q;
    ]

let hall_sw = Vec.make (-12.) (-7.)
let hall_se = Vec.make 0. (-7.)
let hall_ne = Vec.make 0. 7.
let hall_nw = Vec.make (-12.) 7.
let middle = Vec.make 0. 0.

(* What is behind each of them. Both sides of a link must agree about the door,
   so each chamber hangs its own copy of whatever the hall hangs — and wears the
   same transom, so that the view back out is the view in. *)
let chamber =
  Element.declare ~name:"chamber"
  @@ fun (name, door, transom, ceiling, sprites) ->
  let sw = Vec.make 0. (-3.5)
  and se = Vec.make 6. (-3.5)
  and ne = Vec.make 6. 3.5
  and nw = Vec.make 0. 3.5 in
  P.(
    room ~name
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling
      (cut ~name:"back" ~door ~transom nw sw
      @ [
          wall ~height ~material:Surfaces.brick sw se;
          wall ~height ~material:Surfaces.brick se ne;
          wall ~height ~material:Surfaces.brick ne nw;
        ]
      @ sprites))

let roofed = P.roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit

let level =
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:("hall", Vec.make (-7.) 0.)
      [
        (* The hall you arrive in, its east wall cut twice: the grille to the
           south, the glazed door to the north. Cut south to north so the wall's
           winding is unbroken. *)
        room ~name:"hall"
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
          (cut ~name:"barred" ~door:grille ~transom:Surfaces.brick hall_se
             middle
          @ cut ~name:"glazed" ~door:oak ~transom:Surfaces.window middle hall_ne
          @ [
              wall ~height ~material:Surfaces.stone hall_sw hall_se;
              wall ~height ~material:Surfaces.stone hall_ne hall_nw;
              wall ~height ~material:Surfaces.stone hall_nw hall_sw;
            ]);
        chamber ~key:"bars"
          ( "behind-the-bars",
            grille,
            Surfaces.brick,
            roofed,
            (* Close to the bars, and so out of the fog: the point of this room
               is that you can see what is standing in it. *)
            [ sprite ~size:1.8 ~image:Pictures.figure (Vec.make 2. 0.) ] );
        (* Open to the sky, which is what the glass over the door shows and the
           door itself does not. *)
        chamber ~key:"glass"
          ("behind-the-glass", oak, Surfaces.window, open_sky Surfaces.day, []);
        link ("hall", "barred") ("behind-the-bars", "back");
        link ("hall", "glazed") ("behind-the-glass", "back");
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
