(** A world with the overlay watching it.

    Two rooms and a doorway between them, written out rather than worked out,
    so that every wall in it is a literal and every literal can be dragged. The
    point is not the world; the point is that the thing looking at it can say
    which line of this file describes each piece of it.

    {b Run it with} [dune exec studio/studio.exe].

    - {b F1} the plan, {b F2} the graph, {b F3} the tree, {b F4} closes it.
    - {b Tab} moves the plan to the next room.
    - On the plan, drag a corner. {b \[} and {b \]} walk the fields of what is
      picked, {b -} and {b =} move the chosen one, {b u} takes it back.
    - {b W A S D} and the mouse to walk, {b Esc} to leave.

    The overlay reads those keys itself; nothing here binds them. What this
    file does is place two things and hand over the description.

    {1 What to look at}

    The lamp is the interesting one. Its height is written
    [if lit then 1.6 else 1.2] — worked out from state rather than written down
    — so the plan draws it and will not drag it, while the wall beside it,
    whose height is a number, drags. That is the whole predicate, visible in
    one room: editable is what was written as a literal.

    Everything here is what a game would write. Nothing in it is special to the
    editor except the two lines that turn the overlay on. *)

open Camlcast

(* {1 Content} *)

let checker ~color ~u ~v =
  Color.level color (if ((u / 8) + (v / 8)) land 1 = 0 then 235 else 175)

let material color = Material.make ~pattern:(Texture.generate (checker ~color))
let stone = material (Color.rgb 150 150 160)
let brick = material (Color.rgb 190 90 80)
let ground = material (Color.rgb 116 110 98)
let soffit = material (Color.rgb 90 92 104)
let flat = Plane.horizontal 0.

(* Read once, and a failure here stops the program: everything this draws over
   the frame needs it, and there is nothing sensible to do without one. *)
let font =
  match Image.of_asset "assets/font.png" with
  | Ok atlas ->
      Font.make ~fallback:'\127' ~atlas ~width:6 ~height:10 ~first:32 ()
  | Error (`Msg message) ->
      prerr_endline ("studio: could not read assets/font.png: " ^ message);
      exit 1

(* {1 The overlay} *)

let studio = Camlcast_edit.Session.create ()

(* {1 The world} *)

(* Made once, at the top level, because a door carries the identity a
   connection joins by and one made inside a render is a different door every
   frame. *)
let plaza_east = P.door ~name:"east" ~width:2. ~clearance:2.4 ()
let hall_west = P.door ~name:"west" ~width:2. ~clearance:2.4 ()
let plaza_north = P.door ~name:"north" ~width:1.6 ~clearance:2.2 ()

(* State, so the tree panel has something to show that is not geometry, and so
   that one height in the room is computed rather than written. *)
let lamp =
  Element.declare ~name:"lamp" @@ fun (at : Vec.t) ->
  let lit, set_lit = Hook.use_state ~show:string_of_bool false in
  P.wall ~key:"lamp"
    ~height:(if lit then 1.6 else 1.2)
    ~material:brick ~on_gaze:set_lit at
    (Vec.add at (Vec.make 1.4 0.))

let level =
  P.(
    world ~atmosphere:Atmosphere.default
      [
        room ~name:"plaza" ~height:4. ~material:stone
          ~floor:(floor ~plane:flat ground) ~ceiling:(roof soffit)
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
            wall ~key:"bench" ~height:0.6 ~material:brick (Vec.make (-2.) 3.)
              (Vec.make 2. 3.);
            lamp (Vec.make 1. (-4.));
          ];
        room ~name:"hall" ~height:3. ~material:brick ~floor:(floor ground)
          ~ceiling:(roof soffit)
          ~outline:
            (corners
               [
                 Vec.make (-4.) (-3.);
                 Vec.make 4. (-3.);
                 Vec.make 4. 3.;
                 Vec.make (-4.) 3.;
               ])
          [ cut hall_west ~along:(Vec.make (-4.) (-3.), Vec.make (-4.) 3.) ];
        connect plaza_east hall_west;
        (* A child of the world rather than of the hud: under it the mouse is
           loose, and Run.aiming is already false, so dragging a corner cannot
           work the door the panel is drawn over. *)
        Camlcast_edit.Session.pointer studio;
        hud [ crosshair (); Camlcast_edit.Session.overlay studio ~font ];
      ])

let () =
  Camlcast_edit.Session.open_ studio;
  match Camlcast_edit.Session.play ~title:"camlcast studio" studio level with
  | Ok _ -> ()
  | Error (`Msg message) ->
      prerr_endline ("studio: " ^ message);
      exit 1
