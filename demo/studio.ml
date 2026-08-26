(** {b The dev overlay, on a world small enough to read in a sitting.} Two rooms
    and a doorway between them, written out rather than worked out, so that
    every wall in it is a literal and every literal can be dragged.

    The point is not the world. The point is that the thing looking at it can
    say which line of [demo/studio.ml] describes each piece of it, and write a
    change back into that line.

    {b F5} the plan, {b F6} the graph, {b F7} the tree, {b F8} closes it; {b Tab}
    moves the plan to the next room. On the plan, drag a corner and the
    coordinate is rewritten in this file. {b \[} and {b \]} walk the fields of
    whatever is picked, {b -} and {b =} move the chosen one, {b u} takes the last
    change back, and {b x} moves what is picked into a component file of its own.

    The overlay reads those keys itself; nothing here binds them. What this file
    does is place two things — a pointer in the world and a panel in the hud —
    and hand {!Camlcast.Run.on} the session's {!Camlcast.Watch}.

    {1 What to look at}

    The lamp is the interesting one. Its height is written
    [if lit then 1.6 else 1.2] — worked out from state rather than written down —
    so the plan draws it and will not drag it, while the bench beside it, whose
    height is a number, drags. That is the whole predicate, visible in one room:
    editable is what was written as a literal.

    Two doorways lead nowhere on purpose, one in each room, and the graph panel
    says so in red. They are the same width and clearance, so clicking each in
    turn joins them and writes the [connect] into this file — which is the editor
    doing the one thing that is not moving a number.

    Everything here is what a game would write. Nothing in it is special to the
    editor except the two places the overlay is put and the {!Camlcast.Watch}
    handed to the run. *)

open Camlcast

(* {1 The overlay} *)

(* At the top level, because a session is what holds the panels' state between
   frames: which one is up, what is picked, what is not yet written. One made
   inside [run] would forget all of it every time the launcher came back here. *)
let studio = Camlcast_edit.Session.create ()

(* {1 The world} *)

(* Made once, at the top level, because a door carries the identity a connection
   joins by, and one made inside a render is a different door every frame. *)
let plaza_east = P.door ~name:"east" ~width:2. ~clearance:2.4 ()
let hall_west = P.door ~name:"west" ~width:2. ~clearance:2.4 ()

(* The two that lead nowhere. The same width and clearance as each other, so
   that joining them leaves a world World.make will build -- two linked
   thresholds differing in either is one of the things it refuses. *)
let plaza_north = P.door ~name:"north" ~width:1.6 ~clearance:2.2 ()
let hall_south = P.door ~name:"south" ~width:1.6 ~clearance:2.2 ()

(* State, so the tree panel has something to show that is not geometry, and so
   that one height in the room is computed rather than written. *)
let lamp =
  Element.declare ~name:"lamp" @@ fun (at : Vec.t) ->
  let lit, set_lit = Hook.use_state ~show:string_of_bool false in
  P.wall ~key:"lamp"
    ~height:(if lit then 1.6 else 1.2)
    ~material:Surfaces.brick ~on_gaze:set_lit at
    (Vec.add at (Vec.make 1.4 0.))

(* The rooms alone, shared by the world the catalogue carries and the
   description [run] plays. The overlay is deliberately not in here: [--list]
   forces every world in the catalogue, and one with the panel in it would need
   a font read off the disk before it could be listed. *)
let rooms =
  P.
    [
      room ~name:"plaza" ~height:4. ~material:Surfaces.stone
        ~floor:(floor ~plane:(Plane.horizontal 0.) Surfaces.ground)
        ~ceiling:(roof Surfaces.soffit)
        ~outline:
          (corners
             [
               Vec.make (-6.) (-6.);
               Vec.make 6. (-6.);
               Vec.make 6. 6.;
               Vec.make (-6.) 6.;
             ])
        [
          spawn (Vec.make (-3.) 0.);
          cut plaza_east ~along:(Vec.make 6. (-6.), Vec.make 6. 6.);
          cut plaza_north ~along:(Vec.make 6. 6., Vec.make (-6.) 6.);
          wall ~key:"bench" ~height:0.6 ~material:Surfaces.brick
            (Vec.make (-2.) 3.) (Vec.make 2. 3.);
          lamp (Vec.make 1. (-4.));
        ];
      room ~name:"hall" ~height:3. ~material:Surfaces.brick
        ~floor:(floor ~plane:(Plane.horizontal 0.) Surfaces.ground)
        ~ceiling:(roof Surfaces.soffit)
        ~outline:
          (corners
             [
               Vec.make (-4.) (-3.);
               Vec.make 4. (-3.);
               Vec.make 4. 3.;
               Vec.make (-4.) 3.;
             ])
        [
          cut hall_west ~along:(Vec.make (-4.) 3., Vec.make (-4.) (-3.));
          cut hall_south ~along:(Vec.make (-4.) (-3.), Vec.make 4. (-3.));
        ];
      connect plaza_east hall_west;
    ]

let level = P.world ~atmosphere:Surfaces.air rooms
let world = (Mount.build level).Scene.world

let run window =
  Camlcast_edit.Session.open_ studio;
  Run.on window ~controls:Bindings.escapable
    ~watch:(Camlcast_edit.Session.watch studio)
    P.(
      world ~atmosphere:Surfaces.air
        (rooms
        @ [
            (* A child of the world rather than of the hud: under it the mouse
               is loose, and Run.aiming is already false, so dragging a corner
               cannot work the door the panel is drawn over. *)
            Camlcast_edit.Session.pointer studio;
            hud
              [
                crosshair ();
                Camlcast_edit.Session.overlay studio
                  ~font:(Lazy.force Typeface.font);
              ];
          ]))
