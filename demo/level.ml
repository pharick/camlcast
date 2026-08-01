(** A demonstration world of five rooms, built to exercise the whole engine at
    once — every kind of wall, both kinds of threshold, both a roof and the open
    {!Camlcast_core.Sky}, and, unlike every other demo here, all of it at the
    same time:

    - {b plaza}, open to an afternoon sky: a twelve-sided ring of tall walls
      around the spawn, six pillars of differing heights and materials, a
      gallery wall hung with a painting and a poster on the front and a lit sign
      on the back, a steel grille and a leaded window to look through, and
      sprites standing about. Three doorways lead out of it, cut into three
      different sides of the ring so none of them is axis aligned — the
      transforms between the plaza and its neighbours are genuine rotations, not
      just translations.
    - {b hall}, roofed: a rectangle with a bench, a corner pillar and a barrel,
      open to the plaza on one side and joined to the cellar by a doorway with
      an oak door in it. Its roof climbs faster than its floor does, so the
      headroom grows as you cross it.
    - {b nook}, roofed and low: a triangle closed off but for its one doorway.
    - {b garden}, open to a sky of its own — a later one than the plaza's, which
      is the whole of what two rooms under two skies takes: a winding low wall
      you look over, a tall monolith, two sprites held clear of the ground, and
      a grille gate with a brick transom over it.
    - {b cellar}, roofed and low: a small dark room with a figure in it and dust
      turning in the air, reached only through the hall's door.

    Every room's floor is the same gently tilted surface seen from its own
    frame, derived with {!Camlcast_core.Plane.through} so that
    {!Camlcast_core.World.seam_gap} is zero at every doorway by construction
    rather than by arithmetic luck.

    {1 A world, and then a game}

    {!default} is the world at rest and is all the tests need: a value, built
    once with {!Camlcast.Mount.build}, with nothing shut and nothing moving.
    What {!run} adds is the part a world cannot hold by itself — {b E} works the
    door you are nearest, the dust in the cellar is re-made every frame, and a
    crosshair says what you are looking at. That split is the layer's own: a
    description is a value either way, and mounting one is what gives the
    components in it somewhere to keep what they remember. This level is small
    enough to show the difference and large enough to need it. *)

open Camlcast

(** A floor or ceiling of the level's usual materials. *)
let ground plane = P.floor ~plane ~material:Surfaces.ground

let roofed plane = P.roof ~plane ~material:Surfaces.soffit

(** The two leaves in the level, both hung open at rest.

    Open is what they are at rest, not what they are for: the {b E} key works
    whichever you are looking at, and a leaf that is shut is drawn and walked
    into like any other wall. Starting them open is what keeps the level
    walkable end to end before anyone has touched anything — shutting yourself
    out of somewhere is a thing you should have to do on purpose.

    The oak one is between the hall and the cellar. The garden's is a steel
    grille, so shutting it leaves the plaza in plain sight through the bars and
    entirely out of reach: {!Camlcast.Material} decides what you can see through
    and {!Camlcast.Door} decides what you can walk through, and this is the one
    place in the level where those two answers differ.

    Both sides of a link ask this with the same name, so they cannot disagree —
    which is what World.set_door used to have to keep in step and now nobody
    does. *)
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

(* Every opening's two ends, worked out once. A floor is carried from one room
   to its neighbour through the doorway they share, so both sides of every join
   have to be nameable — and the garden's jambs are written by hand, which needs
   them too. *)
let plaza_east = P.opening ~width:gate_width (plaza_corner 0) (plaza_corner 1)
let plaza_north = P.opening ~width:gate_width (plaza_corner 3) (plaza_corner 4)
let plaza_west = P.opening ~width:gate_width (plaza_corner 6) (plaza_corner 7)
let hall_west = P.opening ~width:gate_width (Vec.make 0. 5.) (Vec.make 0. (-5.))

let hall_cellar =
  P.opening ~width:door_width (Vec.make 6. (-5.)) (Vec.make 6. 5.)

let nook_south =
  P.opening ~width:gate_width (Vec.make (-3.) 0.) (Vec.make 3. 0.)

let garden_east =
  P.opening ~width:gate_width (Vec.make 0. (-5.)) (Vec.make 0. 5.)

let cellar_up = P.opening ~width:door_width (Vec.make 0. 3.) (Vec.make 0. (-3.))

(* Each neighbour's floor is carried across the doorway rather than restated, so
   no two rooms can disagree about where the ground is at a threshold they
   share. Check reports a step in the floor at a doorway, and a plane carried
   through one never has one. *)
let plaza_floor = Plane.make ~a:0.06 ~b:0.03 ~c:0.
let hall_floor = P.through ~from:plaza_east ~into:hall_west plaza_floor
let nook_floor = P.through ~from:plaza_north ~into:nook_south plaza_floor
let garden_floor = P.through ~from:plaza_west ~into:garden_east plaza_floor
let cellar_floor = P.through ~from:hall_cellar ~into:cellar_up hall_floor
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
    {!Targets} demo's, because a reader who has seen that one should not have to
    learn a second vocabulary: a doorway reads blue, a hung picture violet, a
    plain wall amber, a sprite green, and the open sky white. *)
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
  (* The garden's own ends, not the plaza's. Two rooms have no coordinates in
     common, which is the whole point of a world of linked rooms — and using the
     wrong pair here put a three-quarter-cell step in the floor at the gate,
     which is what the seam check is for and what it found. *)
  let garden_p, garden_q = garden_east in
  P.(
    world ~atmosphere:Surfaces.air
      ~spawn:("plaza", Vec.make 0. 0.)
      [
        room ~name:"plaza" ~floor:(ground plaza_floor)
          ~ceiling:(open_sky Surfaces.day)
          ([
             doorway ~name:"east" ~width:gate_width ~opening:2.6 ~height:7.
               ~material:Surfaces.stone (plaza_corner 0) (plaza_corner 1);
             doorway ~name:"north" ~width:gate_width ~opening:2.6 ~height:7.
               ~material:Surfaces.stone (plaza_corner 3) (plaza_corner 4);
             doorway ~name:"west" ~door:garden_door ~on_gaze:garden_gaze
               ~on_use:garden_use ~width:gate_width ~opening:2.6 ~height:7.
               ~material:Surfaces.stone (plaza_corner 6) (plaza_corner 7);
           ]
          @ List.map
              (fun k ->
                wall ~height:7. ~material:Surfaces.stone (plaza_corner k)
                  (plaza_corner ((k + 1) mod 12)))
              [ 1; 2; 4; 5; 7; 8; 9; 10; 11 ]
          (* Six square pillars ringed around the spawn, each a different height
             and material, so you weave between them and see over the low
             ones. *)
          @ List.init 6 (fun k ->
              let angle = float_of_int k *. Float.pi /. 3. in
              polygon
                ~center:(Vec.make (6. *. cos angle) (6. *. sin angle))
                ~radius:0.6 ~sides:4 ~rotation:0.6
                ~height:[| 3.5; 0.6; 2.2; 4.5; 1.3; 2.8 |].(k)
                ~material:
                  [|
                    Surfaces.brick;
                    Surfaces.panel;
                    Surfaces.stone;
                    Surfaces.tile;
                  |].(k mod 4))
          @ [
              (* A brick wall hung on both sides: a painting and a poster facing
                 the spawn, and on the back of it a lit sign.

                 The sign is the only decal in the level that is neither Front
                 nor unlit. Back hangs it on the far face, so walking round the
                 wall is what finds it; glow lifts it clear of the light the
                 rest of the wall is under. *)
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
        room ~name:"hall"
          ~floor:(ground hall_floor)
            (* Not Plane.above, which is the floor's own slope carried up bodily
             and so a ceiling of fixed headroom. This one has a slope of its
             own, steeper than the floor's, so the two diverge: the hall is 4
             cells high where you come in and rather more of that by the far
             wall. Nothing else in the level does this. *)
          ~ceiling:
            (roofed
               (Plane.make
                  ~a:(Plane.gradient hall_floor (Vec.make 1. 0.) +. 0.05)
                  ~b:(Plane.gradient hall_floor (Vec.make 0. 1.))
                  ~c:(Plane.elevation hall_floor (Vec.make 0. 0.) +. 4.)))
          [
            doorway ~name:"west" ~width:gate_width ~opening:2.6 ~height:4.5
              ~material:Surfaces.brick (Vec.make 0. 5.) (Vec.make 0. (-5.));
            doorway ~name:"cellar" ~door:cellar_door ~on_gaze:cellar_gaze
              ~on_use:cellar_use ~width:door_width ~opening:2.2 ~height:4.5
              ~material:Surfaces.brick (Vec.make 6. (-5.)) (Vec.make 6. 5.);
            wall ~height:4.5 ~material:Surfaces.brick (Vec.make 0. (-5.))
              (Vec.make 6. (-5.));
            wall ~height:4.5 ~material:Surfaces.brick (Vec.make 6. 5.)
              (Vec.make 0. 5.);
            (* A low bench you see over. *)
            wall ~height:0.5 ~material:Surfaces.panel (Vec.make 3. (-4.))
              (Vec.make 5. (-4.));
            polygon ~center:(Vec.make 4. 3.) ~radius:0.7 ~sides:4 ~rotation:0.3
              ~height:4.5 ~material:Surfaces.tile;
            sprite ~key:"barrel" ~size:0.9 ~image:Pictures.barrel
              (Vec.make 3. (-2.));
          ];
        room ~name:"nook" ~floor:(ground nook_floor)
          ~ceiling:(roofed (Plane.above nook_floor 2.9))
          [
            doorway ~name:"south" ~width:gate_width ~opening:2.6 ~height:3.2
              ~material:Surfaces.tile (Vec.make (-3.) 0.) (Vec.make 3. 0.);
            path ~height:3.2 ~material:Surfaces.tile
              [ Vec.make 3. 0.; Vec.make 0. 5.; Vec.make (-3.) 0. ];
          ];
        room ~name:"garden"
          ~floor:(ground garden_floor)
            (* A sky of its own, and the only thing about the garden that is not
             the plaza's. A Sky belongs to the room it roofs, so two rooms under
             two skies costs a second value and nothing else; the light on the
             walls does not follow, because that is the world's one Atmosphere
             and it lights both. *)
          ~ceiling:(open_sky Surfaces.dusk)
          [
            (* The garden's side of the gate is written by hand rather than cut,
               for the one thing P.doorway will not do: a lintel of a material
               other than the wall's. The strip left standing over this opening
               is brick where the wall either side of it is stone, which is what
               makes it read as a transom rather than as more wall. The plaza's
               side is a plain doorway, so the two rooms disagree about what is
               over the opening — as two rooms built by different hands would. *)
            wall ~height:7. ~material:Surfaces.stone (Vec.make 0. (-5.))
              garden_p;
            wall ~height:7. ~material:Surfaces.stone garden_q (Vec.make 0. 5.);
            threshold ~name:"east" ~door:garden_door ~on_gaze:garden_gaze
              ~on_use:garden_use ~height:2.6
              ~lintel:{ top = 7.; material = Surfaces.brick }
              garden_p garden_q;
            wall ~height:7. ~material:Surfaces.stone (Vec.make 0. 5.)
              (Vec.make (-8.) 5.);
            wall ~height:7. ~material:Surfaces.stone (Vec.make (-8.) 5.)
              (Vec.make (-8.) (-5.));
            wall ~height:7. ~material:Surfaces.stone (Vec.make (-8.) (-5.))
              (Vec.make 0. (-5.));
            (* A lone tall monolith. *)
            wall ~height:6. ~material:Surfaces.brick (Vec.make (-6.) (-3.5))
              (Vec.make (-4.5) (-4.5));
            (* A winding low wall you look over into the sky beyond. *)
            path ~height:0.5 ~material:Surfaces.panel
              [
                Vec.make (-7.) (-3.);
                Vec.make (-2.) (-2.);
                Vec.make (-4.) 1.;
                Vec.make (-1.) 3.;
                Vec.make (-6.) 4.;
              ];
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
        room ~name:"cellar" ~floor:(ground cellar_floor)
          ~ceiling:(roofed (Plane.above cellar_floor 2.5))
          ([
             doorway ~name:"up" ~door:cellar_door ~on_gaze:cellar_gaze
               ~on_use:cellar_use ~width:door_width ~opening:2.2 ~height:2.8
               ~material:Surfaces.stone (Vec.make 0. 3.) (Vec.make 0. (-3.));
             wall ~height:2.8 ~material:Surfaces.stone (Vec.make 0. (-3.))
               (Vec.make 5. (-3.));
             wall ~height:2.8 ~material:Surfaces.stone (Vec.make 5. (-3.))
               (Vec.make 5. 3.);
             wall ~height:2.8 ~material:Surfaces.stone (Vec.make 5. 3.)
               (Vec.make 0. 3.);
             sprite ~key:"figure" ~size:1.8 ~image:Pictures.figure
               (Vec.make 2.5 0.);
           ]
          (* The dust. Where the old version had to keep the cellar's own
             sprites and hand them back with the motes — with_sprites replaces
             the array rather than adding to it, and handing it only the motes
             made the figure disappear — a description simply says both. *)
          @ List.init motes_count (mote ~t));
        link ("plaza", "east") ("hall", "west");
        link ("plaza", "north") ("nook", "south");
        link ("plaza", "west") ("garden", "east");
        link ("hall", "cellar") ("cellar", "up");
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
  (* Which door the crosshair is on, if it is on one. The old version found the
     nearest opening with a leaf in it and worked that; a doorway is told now,
     and this is only kept so that pressing E at nothing can say so. *)
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
