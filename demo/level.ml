(** A demonstration world of five rooms, built to exercise the whole engine at
    once — every kind of wall, both kinds of threshold, and both a roof and the
    open {!Raycaster.Sky}:

    - {b plaza}, open to the sky: a twelve-sided ring of tall walls around the
      spawn, six pillars of differing heights and materials, a gallery wall hung
      with a painting and a poster, a steel grille and a leaded window to look
      through, and sprites standing about. Three doorways lead out of it, cut
      into three different sides of the ring so none of them is axis aligned —
      the transforms between the plaza and its neighbours are genuine rotations,
      not just translations.
    - {b hall}, roofed: a rectangle with a bench, a corner pillar and a barrel,
      open to the plaza on one side and joined to the cellar by a doorway with
      an oak door standing open in it.
    - {b nook}, roofed and low: a triangle closed off but for its one doorway.
    - {b garden}, open to the sky: a winding low wall you look over and a tall
      monolith, walled round.
    - {b cellar}, roofed and low: a small room with a figure, reached only
      through the hall's door.

    Every room's floor is the same gently tilted surface seen from its own
    frame, derived with {!Raycaster.Plane.through} so that
    {!Raycaster.World.seam_gap} is zero at every doorway by construction rather
    than by arithmetic luck. *)

open Raycaster

(** A floor or ceiling of the level's usual materials. *)
let ground plane = { Room.plane; material = Surfaces.ground }

let roof plane = Room.Roof { Room.plane; material = Surfaces.soffit }

(** The oak leaf between the hall and the cellar, standing open so that the
    whole level stays walkable. A closed one would render as a leaf and seal the
    cellar off, which is the {!Doors} demo's subject rather than this one's. *)
let cellar_door = Door.make ~state:Door.Open Surfaces.oak

let default =
  (* Doorways are cut with {!Raycaster.Room.doorway}, which splits the wall and
     returns the jambs alongside the threshold, so an opening and the wall it is
     cut into can never drift apart. *)
  let plaza_corner k =
    let angle = float_of_int k *. Float.pi /. 6. in
    Vec.make (11. *. cos angle) (11. *. sin angle)
  in
  let plaza_side k =
    Room.wall ~height:7. ~material:Surfaces.stone (plaza_corner k)
      (plaza_corner ((k + 1) mod 12))
  in
  let plaza_gate name k =
    Room.doorway ~name ~width:2.4 ~opening:2.6 ~height:7.
      ~material:Surfaces.stone (plaza_corner k)
      (plaza_corner ((k + 1) mod 12))
  in
  let east_jambs, plaza_east = plaza_gate "east" 0
  and north_jambs, plaza_north = plaza_gate "north" 3
  and west_jambs, plaza_west = plaza_gate "west" 6 in
  let hall_jambs, hall_west =
    Room.doorway ~name:"west" ~width:2.4 ~opening:2.6 ~height:4.5
      ~material:Surfaces.brick (Vec.make 0. 5.) (Vec.make 0. (-5.))
  and hall_door_jambs, hall_cellar =
    Room.doorway ~name:"cellar" ~door:cellar_door ~width:1.6 ~opening:2.2
      ~height:4.5 ~material:Surfaces.brick (Vec.make 6. (-5.)) (Vec.make 6. 5.)
  in
  let nook_jambs, nook_south =
    Room.doorway ~name:"south" ~width:2.4 ~opening:2.6 ~height:3.2
      ~material:Surfaces.tile (Vec.make (-3.) 0.) (Vec.make 3. 0.)
  in
  let garden_jambs, garden_east =
    Room.doorway ~name:"east" ~width:2.4 ~opening:2.6 ~height:7.
      ~material:Surfaces.stone (Vec.make 0. (-5.)) (Vec.make 0. 5.)
  in
  let cellar_jambs, cellar_up =
    Room.doorway ~name:"up" ~door:cellar_door ~width:1.6 ~opening:2.2
      ~height:2.8 ~material:Surfaces.stone (Vec.make 0. 3.) (Vec.make 0. (-3.))
  in
  (* The transform of a link, exactly as {!Raycaster.World.make} will derive it,
     so a neighbour's floor can be built to meet this one across the doorway. *)
  let link (a : Room.threshold) (b : Room.threshold) =
    Transform.between ~a1:a.a ~a2:a.b ~b1:b.a ~b2:b.b
  in
  let plaza_floor = Plane.make ~a:0.06 ~b:0.03 ~c:0. in
  let hall_floor = Plane.through (link plaza_east hall_west) plaza_floor in
  let nook_floor = Plane.through (link plaza_north nook_south) plaza_floor in
  let garden_floor = Plane.through (link plaza_west garden_east) plaza_floor in
  let cellar_floor = Plane.through (link hall_cellar cellar_up) hall_floor in
  let plaza =
    let pillars =
      (* Six square pillars ringed around the spawn, each a different height and
         material, so you weave between them and see over the low ones. *)
      let coats =
        [| Surfaces.brick; Surfaces.panel; Surfaces.stone; Surfaces.tile |]
      in
      List.concat
        (List.init 6 (fun k ->
             let angle = float_of_int k *. Float.pi /. 3. in
             let center = Vec.make (6. *. cos angle) (6. *. sin angle) in
             let height = [| 3.5; 0.6; 2.2; 4.5; 1.3; 2.8 |].(k) in
             Room.regular_polygon ~center ~radius:0.6 ~sides:4 ~rotation:0.6
               ~height ~material:coats.(k mod 4)))
    and gallery =
      (* A brick wall hung with a painting and a poster. *)
      [
        Room.wall ~height:3.2 ~material:Surfaces.brick (Vec.make (-3.) (-4.))
          (Vec.make 3. (-4.))
          ~decals:
            [
              Room.decal ~along:2. ~z:1.6 ~half_width:0.9 ~half_height:0.9
                Pictures.painting;
              Room.decal ~along:4. ~z:1.6 ~half_width:0.7 ~half_height:0.9
                Pictures.poster;
            ];
      ]
    and see_through =
      (* A steel grille and a leaded window, each with something behind it. *)
      [
        Room.wall ~height:2. ~material:Surfaces.grille (Vec.make (-4.) 4.)
          (Vec.make 1. 4.);
        Room.wall ~height:2.6 ~material:Surfaces.window (Vec.make 3. 3.)
          (Vec.make 6. 3.);
      ]
    in
    Room.make
      ~thresholds:[ plaza_east; plaza_north; plaza_west ]
      ~floor:(ground plaza_floor)
      ~ceiling:(Room.Open Surfaces.day)
        (* Kept off the bearings of the three doorways (15, 105 and 195
           degrees), so that from the spawn each opening is seen through rather
           than blocked by something standing in front of it. *)
      ~sprites:
        [
          Room.sprite ~size:0.9 ~image:Pictures.barrel (Vec.make 2.2 (-1.8));
          Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 2.6 (-0.8));
          Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make (-3.5) 4.);
          Room.sprite ~size:0.9 ~image:Pictures.barrel (Vec.make 4.5 4.3);
        ]
      (List.concat
         [
           east_jambs;
           north_jambs;
           west_jambs;
           List.map plaza_side [ 1; 2; 4; 5; 7; 8; 9; 10; 11 ];
           pillars;
           gallery;
           see_through;
         ])
  and hall =
    Room.make
      ~thresholds:[ hall_west; hall_cellar ]
      ~floor:(ground hall_floor)
      ~ceiling:(roof (Plane.above hall_floor 4.))
      ~sprites:
        [ Room.sprite ~size:0.9 ~image:Pictures.barrel (Vec.make 3. (-2.)) ]
      (List.concat
         [
           hall_jambs;
           hall_door_jambs;
           [
             Room.wall ~height:4.5 ~material:Surfaces.brick (Vec.make 0. (-5.))
               (Vec.make 6. (-5.));
             Room.wall ~height:4.5 ~material:Surfaces.brick (Vec.make 6. 5.)
               (Vec.make 0. 5.);
             (* A low bench you see over. *)
             Room.wall ~height:0.5 ~material:Surfaces.panel (Vec.make 3. (-4.))
               (Vec.make 5. (-4.));
           ];
           Room.regular_polygon ~center:(Vec.make 4. 3.) ~radius:0.7 ~sides:4
             ~rotation:0.3 ~height:4.5 ~material:Surfaces.tile;
         ])
  and nook =
    Room.make ~thresholds:[ nook_south ]
      ~floor:(ground nook_floor)
      ~ceiling:(roof (Plane.above nook_floor 2.9))
      (nook_jambs
      @ Room.path ~height:3.2 ~material:Surfaces.tile
          [ Vec.make 3. 0.; Vec.make 0. 5.; Vec.make (-3.) 0. ])
  and garden =
    Room.make ~thresholds:[ garden_east ]
      ~floor:(ground garden_floor)
      ~ceiling:(Room.Open Surfaces.day)
      (List.concat
         [
           garden_jambs;
           [
             Room.wall ~height:7. ~material:Surfaces.stone (Vec.make 0. 5.)
               (Vec.make (-8.) 5.);
             Room.wall ~height:7. ~material:Surfaces.stone (Vec.make (-8.) 5.)
               (Vec.make (-8.) (-5.));
             Room.wall ~height:7. ~material:Surfaces.stone (Vec.make (-8.) (-5.))
               (Vec.make 0. (-5.));
             (* A lone tall monolith. *)
             Room.wall ~height:6. ~material:Surfaces.brick (Vec.make (-6.) (-3.5))
               (Vec.make (-4.5) (-4.5));
           ];
           (* A winding low wall you look over into the sky beyond. *)
           Room.path ~height:0.5 ~material:Surfaces.panel
             [
               Vec.make (-7.) (-3.);
               Vec.make (-2.) (-2.);
               Vec.make (-4.) 1.;
               Vec.make (-1.) 3.;
               Vec.make (-6.) 4.;
             ];
         ])
  and cellar =
    Room.make ~thresholds:[ cellar_up ]
      ~floor:(ground cellar_floor)
      ~ceiling:(roof (Plane.above cellar_floor 2.5))
      ~sprites:
        [ Room.sprite ~size:1.8 ~image:Pictures.figure (Vec.make 2.5 0.) ]
      (cellar_jambs
      @ [
          Room.wall ~height:2.8 ~material:Surfaces.stone (Vec.make 0. (-3.))
            (Vec.make 5. (-3.));
          Room.wall ~height:2.8 ~material:Surfaces.stone (Vec.make 5. (-3.))
            (Vec.make 5. 3.);
          Room.wall ~height:2.8 ~material:Surfaces.stone (Vec.make 5. 3.)
            (Vec.make 0. 3.);
        ])
  in
  World.make
    ~rooms:
      [
        ("plaza", plaza);
        ("hall", hall);
        ("nook", nook);
        ("garden", garden);
        ("cellar", cellar);
      ]
    ~links:
      [
        (("plaza", "east"), ("hall", "west"));
        (("plaza", "north"), ("nook", "south"));
        (("plaza", "west"), ("garden", "east"));
        (("hall", "cellar"), ("cellar", "up"));
      ]
    ~atmosphere:Surfaces.air
    ~spawn:("plaza", Vec.make 0. 0.)
