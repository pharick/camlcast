(** A demonstration world of five rooms, built to exercise the whole engine at
    once — every kind of wall, both kinds of threshold, both a roof and the open
    {!Camlcast_core.Sky}, and, unlike every other demo here, all of it at the
    same time:

    - {b plaza}, open to an afternoon sky: a twelve-sided ring of tall walls
      around the spawn, six pillars of differing heights and materials, a
      gallery wall with a painting and a poster on the front and a lit sign on
      the back, a steel grille and a leaded window to look through, and standing
      sprites. Its three doorways are cut into three different sides of the
      ring, so none is axis aligned — the transforms to the neighbours are
      genuine rotations, not just translations.
    - {b hall}, roofed: a rectangle with a bench, a corner pillar and a barrel,
      open to the plaza on one side and joined to the cellar by a doorway with
      an oak door. Its roof climbs faster than its floor, so headroom grows
      across it.
    - {b nook}, roofed and low: a triangle closed off but for its one doorway.
    - {b garden}, open to a sky of its own — a later one than the plaza's, which
      is the whole of what two rooms under two skies takes: a winding low wall
      to look over, a tall monolith, two sprites held clear of the ground, and a
      grille gate with a brick transom over it.
    - {b cellar}, roofed and low: a small dark room with a figure and turning
      dust, reached only through the hall's door.

    Every room's floor is the same gently tilted surface seen from its own
    frame, derived with {!Camlcast_core.Plane.through}, so
    {!Camlcast_core.World.seam_gap} is zero at every doorway by construction
    rather than by arithmetic luck.

    {1 A world, and then a game}

    {!default} is the world at rest and all the tests need: a value, built once
    with {!Camlcast.Mount.build}, nothing shut and nothing moving. {!run} adds
    the part a world cannot hold by itself — {b E} works the door under the
    crosshair, the cellar's dust is re-made every frame, and a crosshair reports
    what is aimed at. That split is the layer's own: a description is a value
    either way, and mounting one gives the components in it somewhere to keep
    what they remember. This level is small enough to show the difference and
    large enough to need it. *)

open Camlcast

(** A floor or ceiling of the level's usual materials. *)
let ground plane = P.floor ~plane Surfaces.ground

(** The same floor for a room that is given no plane of its own: it is carried
    through the connection the room is reached by. *)
let ground_of () = P.floor Surfaces.ground

let roofed plane = P.roof ~plane Surfaces.soffit

(** The two leaves in the level, both hung open at rest.

    The {b E} key works whichever leaf is under the crosshair, and a shut leaf
    is drawn and collides like any other wall. Starting them open keeps the
    level walkable end to end before anything is touched — shutting an area off
    should take a deliberate act.

    The oak one is between the hall and the cellar. The garden's is a steel
    grille, so shutting it leaves the plaza in plain sight through the bars and
    entirely out of reach: {!Camlcast.Material} decides what can be seen through
    and {!Camlcast.Door} decides what can be walked through, and this is the one
    place in the level where those two answers differ.

    Both sides of a link ask this with the same name, so they cannot disagree —
    the thing World.set_door has to keep in step when the sides are indices. *)
let leaf material ~shut =
  Door.make ~state:(if shut then Door.Closed else Door.Open) material

let cellar_leaf = leaf Surfaces.oak
let garden_leaf = leaf Surfaces.grille

(* The plaza is a twelve-sided ring; its gates are cut into sides 0, 3 and 6. *)
let plaza_corner k =
  let angle = float_of_int k *. Float.pi /. 6. in
  Vec.make (11. *. cos angle) (11. *. sin angle)

let gate_width = 2.4
let door_width = 1.6

(* The four ways through, two doors each: a door belongs to the room it is cut
   into, and the connection at the foot of the description is what makes each
   pair one opening. Made here rather than in the description, which is rebuilt
   every frame. *)
let gate name = P.door ~name ~width:gate_width ~clearance:2.6 ()
let doorstep name = P.door ~name ~width:door_width ~clearance:2.2 ()

(* Named, though nothing in the description needs them to be: a name is for
   diagnostics now, and "the cellar door" reads better than the path of the
   element that made it in whatever Check has to say about it. Names are per
   room, so the plaza's west gate and the hall's west gate are both "west". *)
let plaza_east, hall_west = (gate "east", gate "west")
let plaza_north, nook_south = (gate "north", gate "south")
let plaza_west, garden_east = (gate "west", gate "east")
let hall_cellar, cellar_up = (doorstep "cellar", doorstep "up")

(* Only the plaza's floor is written. The hall's is written too, because its
   ceiling is derived from it and a room cannot name a plane nothing wrote; the
   nook's, the garden's and the cellar's are carried through the connections
   they are reached by. *)
let plaza_floor = Plane.make ~a:0.06 ~b:0.03 ~c:0.

let hall_floor =
  P.through
    ~from:(P.opening ~width:gate_width (plaza_corner 0) (plaza_corner 1))
    ~into:(P.opening ~width:gate_width (Vec.make 0. 5.) (Vec.make 0. (-5.)))
    plaza_floor

let motes_count = 18
let motes_period = 7.
let fraction step k = Float.rem (float_of_int k *. step) 1.

let mote ~t k =
  let frames = Array.length Pictures.motes in
  let fall = 1. -. Float.rem ((t /. motes_period) +. fraction 0.4142135624 k) 1.
  and spot =
    Vec.make
      (2.5 +. (2. *. ((fraction 0.7548776662 k *. 2.) -. 1.)))
      (2. *. ((fraction 0.5698402910 k *. 2.) -. 1.))
  in
  let drift =
    0.4 *. (1.3 -. fall)
    *. sin (((t /. motes_period) +. fraction 0.6180339887 k) *. 6.3)
  in
  P.sprite ~key:(string_of_int k) ~base:(fall *. 2.2)
    ~size:(0.3 +. (0.35 *. fraction 0.7320508076 k))
    ~image:Pictures.motes.((k + int_of_float (t *. 7.)) mod frames)
    (Vec.make (spot.Vec.x +. drift) (spot.Vec.y +. (drift *. 0.6)))

(** {1 The level} *)

(** A crosshair that says what it is on, and nothing else. The colours are the
    {!Targets} demo's, so a reader of that one learns no second vocabulary: a
    doorway reads blue, a hung picture violet, a plain wall amber, a sprite
    green, and the open sky white. *)
let tint ~refused (aim : Aim.spot option) =
  match aim with
  | _ when refused > 0. -> Color.rgb 235 80 70
  | Some { Aim.where = Aim.On_doorway; _ } -> Color.rgb 120 170 240
  | Some { Aim.where = Aim.On_wall { decal = Some _; _ }; _ } ->
      Color.rgb 215 130 235
  | Some { Aim.where = Aim.On_sprite; _ } -> Color.rgb 120 230 130
  | Some _ -> Color.rgb 235 195 100
  | None -> Color.rgb 245 245 245

let at ~shut ~t ~aim ~refused ~work ~watch =
  let door name material =
    let is_shut = List.mem name shut in
    ( material ~shut:is_shut,
      (fun here -> watch (if here then Some name else None)),
      fun _ -> work name )
  in
  let cellar_door, cellar_gaze, cellar_use = door "cellar" cellar_leaf in
  let garden_door, garden_gaze, garden_use = door "gate" garden_leaf in
  P.(
    world ~atmosphere:Surfaces.air
      [
        room ~height:7. ~material:Surfaces.stone ~floor:(ground plaza_floor)
          ~ceiling:(open_sky Surfaces.day)
          ~outline:(corners (List.init 12 plaza_corner))
          ([
             spawn (Vec.make 0. 0.);
             cut plaza_east ~along:(plaza_corner 0, plaza_corner 1);
             cut plaza_north ~along:(plaza_corner 3, plaza_corner 4);
             cut plaza_west ~leaf:garden_door ~on_gaze:garden_gaze
               ~on_use:garden_use
               ~along:(plaza_corner 6, plaza_corner 7);
           ]
          (* Six square pillars ringed around the spawn, each a different
             height and material; the low ones can be seen over. *)
          @ List.init 6 (fun k ->
              let angle = float_of_int k *. Float.pi /. 3. in
              block
                ~height:[| 3.5; 0.6; 2.2; 4.5; 1.3; 2.8 |].(k)
                ~material:
                  [|
                    Surfaces.brick;
                    Surfaces.panel;
                    Surfaces.stone;
                    Surfaces.tile;
                  |].(k mod 4)
                (polygon
                   ~center:(Vec.make (6. *. cos angle) (6. *. sin angle))
                   ~radius:0.6 ~sides:4 ~rotation:0.6))
          @ [
              (* A brick wall hung on both sides: a painting and a poster
                 facing the spawn, a lit sign on the back.

                 The sign is the only decal in the level that is neither Front
                 nor unlit. Back hangs it on the far face, found by walking
                 round the wall; glow lifts it clear of the light the rest of
                 the wall is under. *)
              wall ~height:3.2 ~material:Surfaces.brick (Vec.make (-3.) (-4.))
                (Vec.make 3. (-4.))
                ~decals:
                  [
                    decal ~along:2. ~z:1.6 ~half_width:0.9 ~half_height:0.9
                      Pictures.painting;
                    decal ~along:4. ~z:1.6 ~half_width:0.7 ~half_height:0.9
                      Pictures.poster;
                    decal ~facing:Back ~glow:0.8 ~along:3. ~z:1.7
                      ~half_width:0.8 ~half_height:0.5 Pictures.poster;
                  ];
              (* A steel grille and a leaded window, each with something behind
                 it. *)
              wall ~height:2. ~material:Surfaces.grille (Vec.make (-4.) 4.)
                (Vec.make 1. 4.);
              wall ~height:2.6 ~material:Surfaces.window (Vec.make 3. 3.)
                (Vec.make 6. 3.);
              (* Kept off the bearings of the three doorways, so that from the
                 spawn each opening is seen through rather than blocked by
                 something standing in front of it. *)
              sprite ~key:"barrel-a" ~size:0.9 ~image:Pictures.barrel
                (Vec.make 2.2 (-1.8));
              sprite ~key:"figure-a" ~size:1.8 ~image:Pictures.figure
                (Vec.make 2.6 (-0.8));
              sprite ~key:"figure-b" ~size:1.8 ~image:Pictures.figure
                (Vec.make (-3.5) 4.);
              sprite ~key:"barrel-b" ~size:0.9 ~image:Pictures.barrel
                (Vec.make 4.5 4.3);
            ]);
        room ~height:4.5 ~material:Surfaces.brick
          ~floor:(ground hall_floor)
            (* Not Plane.above, which carries the floor's own slope up bodily
             for a ceiling of fixed headroom. This one has a steeper slope of
             its own, so the two diverge: the hall is 4 cells high at the
             entrance and rather more by the far wall. Nothing else in the
             level does this. *)
          ~ceiling:
            (roofed
               (Plane.make
                  ~a:(Plane.gradient hall_floor (Vec.make 1. 0.) +. 0.05)
                  ~b:(Plane.gradient hall_floor (Vec.make 0. 1.))
                  ~c:(Plane.elevation hall_floor (Vec.make 0. 0.) +. 4.)))
          ~outline:
            (corners
               [
                 Vec.make 0. 5.;
                 Vec.make 0. (-5.);
                 Vec.make 6. (-5.);
                 Vec.make 6. 5.;
               ])
          [
            cut hall_west ~along:(Vec.make 0. 5., Vec.make 0. (-5.));
            cut hall_cellar ~leaf:cellar_door ~on_gaze:cellar_gaze
              ~on_use:cellar_use
              ~along:(Vec.make 6. (-5.), Vec.make 6. 5.);
            (* A low bench, seen over. *)
            wall ~height:0.5 ~material:Surfaces.panel (Vec.make 3. (-4.))
              (Vec.make 5. (-4.));
            block ~height:4.5 ~material:Surfaces.tile
              (polygon ~center:(Vec.make 4. 3.) ~radius:0.7 ~sides:4
                 ~rotation:0.3);
            sprite ~key:"barrel" ~size:0.9 ~image:Pictures.barrel
              (Vec.make 3. (-2.));
          ];
        room ~height:3.2 ~material:Surfaces.tile ~floor:(ground_of ())
          ~ceiling:(roof ~headroom:2.9 Surfaces.soffit)
          ~outline:
            (corners [ Vec.make 3. 0.; Vec.make 0. 5.; Vec.make (-3.) 0. ])
          [ cut nook_south ~along:(Vec.make (-3.) 0., Vec.make 3. 0.) ];
        room ~height:7. ~material:Surfaces.stone
          ~floor:(ground_of ())
            (* A sky of its own, and the only thing about the garden that is not
             the plaza's. A Sky belongs to the room it roofs, so two rooms under
             two skies costs a second value and nothing else; the light on the
             walls does not follow, because that is the world's one Atmosphere
             and it lights both. *)
          ~ceiling:(open_sky Surfaces.dusk)
          ~outline:
            (corners
               [
                 Vec.make 0. (-5.);
                 Vec.make 0. 5.;
                 Vec.make (-8.) 5.;
                 Vec.make (-8.) (-5.);
               ])
          [
            (* The strip over this opening is brick where the wall either side
               is stone, which makes it read as a transom rather than more
               wall. The plaza's side says nothing about a lintel and so takes
               its own wall's, and the two rooms disagree about what is over the
               opening — which they are allowed to, because a lintel is how one
               room presents an opening rather than part of the opening. *)
            cut garden_east ~leaf:garden_door ~on_gaze:garden_gaze
              ~on_use:garden_use
              ~lintel:{ top = 7.; material = Surfaces.brick }
              ~along:(Vec.make 0. (-5.), Vec.make 0. 5.);
            (* A lone tall monolith. *)
            wall ~height:6. ~material:Surfaces.brick (Vec.make (-6.) (-3.5))
              (Vec.make (-4.5) (-4.5));
            (* A winding low wall, seen over into the sky beyond. Four walls
               and not one run: it stands in the room rather than bounding it,
               so there is no inside for a winding to face and nothing for the
               engine to work out. The one place in the level where writing the
               segments out is the whole of what is meant. *)
            wall ~height:0.5 ~material:Surfaces.panel (Vec.make (-7.) (-3.))
              (Vec.make (-2.) (-2.));
            wall ~height:0.5 ~material:Surfaces.panel (Vec.make (-2.) (-2.))
              (Vec.make (-4.) 1.);
            wall ~height:0.5 ~material:Surfaces.panel (Vec.make (-4.) 1.)
              (Vec.make (-1.) 3.);
            wall ~height:0.5 ~material:Surfaces.panel (Vec.make (-1.) 3.)
              (Vec.make (-6.) 4.);
            (* Held clear of the floor, which nothing else in the level is: a
               sprite's base is where its feet are, and a barrel with its feet
               at 1.6 is a barrel sitting on nothing. *)
            sprite ~key:"floating" ~base:1.6 ~size:0.9 ~image:Pictures.barrel
              (Vec.make (-3.) 0.5);
            (* Wider than it is tall — the one picture here that is — so it also
               says that a sprite is sized by its height and takes its width
               from the image. *)
            sprite ~key:"mote" ~base:0.9 ~size:0.5 ~image:Pictures.motes.(0)
              (Vec.make (-5.) 2.);
          ];
        room ~height:2.8 ~material:Surfaces.stone ~floor:(ground_of ())
          ~ceiling:(roof ~headroom:2.5 Surfaces.soffit)
          ~outline:
            (corners
               [
                 Vec.make 0. 3.;
                 Vec.make 0. (-3.);
                 Vec.make 5. (-3.);
                 Vec.make 5. 3.;
               ])
          ([
             cut cellar_up ~leaf:cellar_door ~on_gaze:cellar_gaze
               ~on_use:cellar_use
               ~along:(Vec.make 0. 3., Vec.make 0. (-3.));
             sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
               (Vec.make 2.5 0.);
           ]
          (* The dust, written beside the cellar's own sprites rather than
             merged into them. Room.with_sprites replaces the array rather than
             adding to it, so reaching for it here with only the motes would
             take the figure away; a description says both and needs neither. *)
          @ List.init motes_count (mote ~t));
        connect plaza_east hall_west;
        connect plaza_north nook_south;
        connect plaza_west garden_east;
        connect hall_cellar cellar_up;
        hud [ crosshair ~color:(tint ~refused aim) () ];
      ])

let default =
  (Mount.build
     (at ~shut:[] ~t:0. ~aim:None ~refused:0.
        ~work:(fun _ -> ())
        ~watch:(fun _ -> ())))
    .Scene.world

(** {1 The game around it} *)

let showcase =
  Element.declare ~name:"showcase" @@ fun () ->
  let shut, set_shut = Hook.use_state [] in
  let elapsed, set_elapsed = Hook.use_state 0. in
  let refused, set_refused = Hook.use_state 0. in
  (* Which door the crosshair is on, if it is on one. A doorway is told when it
     is worked, so this is not what opens anything; it is kept only so that
     pressing E at nothing can say so. *)
  let at_door, set_at_door = Hook.use_state None in
  let aim = Events.use_aim () in
  Events.use_frame (fun ~dt ->
      set_elapsed (Float.rem (elapsed +. dt) motes_period);
      if refused > 0. then set_refused (Float.max 0. (refused -. dt)));
  Events.use_pressed (Input.Key Key.e) (fun () ->
      if at_door = None then set_refused 0.9);
  at ~shut ~t:elapsed ~aim ~refused
    ~work:(fun name ->
      set_shut
        (if List.mem name shut then
           List.filter (fun other -> other <> name) shut
         else name :: shut))
    ~watch:set_at_door

let run window = Run.on window ~controls:Bindings.escapable (showcase ())
