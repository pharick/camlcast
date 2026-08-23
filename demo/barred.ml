(** {b See-through doors.} A steel grille across one doorway, a leaded transom
    over another, and a room behind each.

    A leaf hung in a doorway is drawn from its own material, and a material is
    see-through when its pattern carries an alpha — exactly as for a wall. A
    door can therefore be shut and transparent at once, and the two halves are
    answered separately: the renderer draws the room beyond behind the leaf and
    composites the leaf over it, while {!Camlcast_core.Room.shut} goes on
    refusing the step — walking into either door stops against a door being
    looked through.

    - The {b left} doorway hangs a grille: the bars are solid and the gaps are
      not, so the chamber behind, and the figure in it, is visible through a
      shut door.
    - The {b right} doorway hangs a solid oak door with a {b transom}: a strip
      of leaded glass in the wall above the opening. The door hides its chamber
      and the glass does not; that chamber is open to the sky.

    The transom is why the wall above an opening carries a material of its own.
    {!Camlcast_core.Room.doorway} cuts a wall and gives the jambs and the strip
    left overhead the same material — the common case, not this one — so {!cut}
    below does the cutting instead. *)

open Camlcast

let height = 4.5
let clearance = 2.6
let width = 2.4
let grille = Door.make Surfaces.grille
let oak = Door.make Surfaces.oak
let flat = Plane.horizontal 0.

(** The two sides of one opening: a leaf hanging in it and a lintel of its own
    material over it, which is the whole of what this demo used to build by hand
    out of two walls and a threshold.

    Two doors and not one, because a door belongs to the room it is cut into and
    there are two rooms. They carry the same leaf and the same transom, which is
    what makes the view back out match the view in; the connection at the foot
    of this file is what makes them one opening. *)
let pair ~leaf ~transom =
  let side () =
    P.door ~leaf
      ~lintel:{ top = height; material = transom }
      ~width ~clearance ()
  in
  (side (), side ())

let hall_bars, back_bars = pair ~leaf:grille ~transom:Surfaces.brick
let hall_glass, back_glass = pair ~leaf:oak ~transom:Surfaces.window
let hall_sw = Vec.make (-12.) (-7.)
let hall_se = Vec.make 0. (-7.)
let hall_ne = Vec.make 0. 7.
let hall_nw = Vec.make (-12.) 7.
let middle = Vec.make 0. 0.

(* What is behind each doorway. The door is the one thing about a chamber that
   is its own, so it is what the two instances are told apart by. *)
let chamber =
  Element.declare ~name:"chamber" @@ fun (back, ceiling, sprites) ->
  let sw = Vec.make 0. (-3.5)
  and se = Vec.make 6. (-3.5)
  and ne = Vec.make 6. 3.5
  and nw = Vec.make 0. 3.5 in
  P.(
    room ~height ~material:Surfaces.brick
      ~floor:(floor ~plane:flat ~material:Surfaces.ground)
      ~ceiling
      ~outline:(corners [ sw; se; ne; nw ])
      (cut back ~along:(nw, sw) :: sprites))

let roofed = P.roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit

let level =
  P.(
    world ~atmosphere:Surfaces.air
      [
        (* The arrival hall. Its east side is two legs of the outline rather
           than one, because two doors are cut into it: the grille to the
           south, the glazed door to the north. Both legs are brick where the
           rest is stone, so the jambs either side of each opening are. *)
        room ~height ~material:Surfaces.stone
          ~floor:(floor ~plane:flat ~material:Surfaces.ground)
          ~ceiling:
            (roof ~plane:(Plane.above flat height) ~material:Surfaces.soffit)
          ~outline:
            [
              corner hall_sw;
              corner hall_se ~material:Surfaces.brick;
              corner middle ~material:Surfaces.brick;
              corner hall_ne;
              corner hall_nw;
            ]
          [
            spawn (Vec.make (-7.) 0.);
            cut hall_bars ~along:(hall_se, middle);
            cut hall_glass ~along:(middle, hall_ne);
          ];
        chamber ~key:"bars"
          ( back_bars,
            roofed,
            (* Close to the bars, and so out of the fog: the room exists to
               make its occupant visible. *)
            [ sprite ~size:1.8 ~image:Pictures.figure (Vec.make 2. 0.) ] );
        (* Open to the sky, which is what the glass over the door shows and the
           door itself does not. *)
        chamber ~key:"glass" (back_glass, open_sky Surfaces.day, []);
        connect hall_bars back_bars;
        connect hall_glass back_glass;
      ])

let world = (Mount.build level).Scene.world
let run window = Run.on window level
