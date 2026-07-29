(** A demonstration world of five rooms, built to exercise the whole engine at
    once — every kind of wall, both kinds of threshold, both a roof and the open
    {!Camlcast.Sky}, and, unlike every other demo here, all of it at the same
    time:

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
    frame, derived with {!Camlcast.Plane.through} so that
    {!Camlcast.World.seam_gap} is zero at every doorway by construction rather
    than by arithmetic luck.

    {1 A world, and then a game}

    {!default} is the world at rest and is all the tests need: a value, built
    once, with nothing shut and nothing moving. What {!run} adds is the part a
    world cannot hold by itself — {b E} works the door you are nearest, the dust
    in the cellar is re-made every frame, and a crosshair says what you are
    looking at. That split is the engine's own: {!Camlcast.Engine.run_world}
    runs a world, {!Camlcast.Engine.run} runs a state that has a world in it,
    and this is small enough to show the difference and large enough to need it.
*)

open Camlcast
open Result_ext

(** A floor or ceiling of the level's usual materials. *)
let ground plane = { Room.plane; material = Surfaces.ground }

let roof plane = Room.Roof { Room.plane; material = Surfaces.soffit }

(** The two leaves in the level, both hung open.

    Open is what they are at rest, not what they are for: {!update} works them
    with the {b E} key, and a leaf that is shut is drawn and walked into like
    any other wall. Starting them open is what keeps the level walkable end to
    end before anyone has touched anything — shutting yourself out of somewhere
    is a thing you should have to do on purpose.

    The oak one is between the hall and the cellar. The garden's is a steel
    grille, so shutting it leaves the plaza in plain sight through the bars and
    entirely out of reach: {!Camlcast.Material} decides what you can see through
    and {!Camlcast.Door} decides what you can walk through, and this is the one
    place in the level where those two answers differ.

    Each is one value hung in {e both} thresholds of its link, because
    {!Camlcast.World.set_door} changes a door on both sides at once and
    {!Camlcast.World.check} refuses a world whose two halves disagree. Note that
    {!Camlcast.Door.make} defaults to shut, so open is asked for. *)
let cellar_door = Door.make ~state:Door.Open Surfaces.oak

let garden_gate = Door.make ~state:Door.Open Surfaces.grille

let default =
  (* Doorways are cut with {!Camlcast.Room.doorway}, which splits the wall and
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
  let plaza_gate ?door name k =
    Room.doorway ?door ~name ~width:2.4 ~opening:2.6 ~height:7.
      ~material:Surfaces.stone (plaza_corner k)
      (plaza_corner ((k + 1) mod 12))
  in
  let east_jambs, plaza_east = plaza_gate "east" 0
  and north_jambs, plaza_north = plaza_gate "north" 3
  and west_jambs, plaza_west = plaza_gate ~door:garden_gate "west" 6 in
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
  (* The garden's side of the gate is cut by hand rather than with
     {!Camlcast.Room.doorway}, for the one thing that does not do: a lintel of a
     material other than the wall's. The strip left standing over this opening is
     brick where the wall either side of it is stone, which is what makes it read
     as a transom rather than as more wall. Splitting about the middle keeps both
     jambs wound the way the boundary is, which is what the link is derived from.
     The plaza's side is a plain doorway, so the two rooms disagree about what is
     over the opening — as two rooms built by different hands would. *)
  let garden_jambs, garden_east =
    let a = Vec.make 0. (-5.) and b = Vec.make 0. 5. in
    let half = Vec.scale (Vec.sub b a) (2.4 /. (2. *. Vec.length (Vec.sub b a)))
    and middle = Vec.scale (Vec.add a b) 0.5 in
    let p = Vec.sub middle half and q = Vec.add middle half in
    ( [
        Room.wall ~height:7. ~material:Surfaces.stone a p;
        Room.wall ~height:7. ~material:Surfaces.stone q b;
      ],
      Room.threshold ~name:"east" ~door:garden_gate ~height:2.6
        ~lintel:{ Room.top = 7.; material = Surfaces.brick }
        p q )
  in
  let cellar_jambs, cellar_up =
    Room.doorway ~name:"up" ~door:cellar_door ~width:1.6 ~opening:2.2
      ~height:2.8 ~material:Surfaces.stone (Vec.make 0. 3.) (Vec.make 0. (-3.))
  in
  (* The transform of a link, exactly as {!Camlcast.World.make} will derive it,
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
               ~height
               ~material:coats.(k mod 4)))
    and gallery =
      (* A brick wall hung on both sides: a painting and a poster facing the
         spawn, and on the back of it a lit sign.

         The sign is the only decal in the level that is neither [Front] nor
         unlit. [Back] hangs it on the far face, so walking round the wall is
         what finds it; [glow] lifts it clear of the light the rest of the wall
         is under, which is how a thing that is meant to be its own light source
         reads in a room lit from somewhere else. *)
      [
        Room.wall ~height:3.2 ~material:Surfaces.brick (Vec.make (-3.) (-4.))
          (Vec.make 3. (-4.))
          ~decals:
            [
              Room.decal ~along:2. ~z:1.6 ~half_width:0.9 ~half_height:0.9
                Pictures.painting;
              Room.decal ~along:4. ~z:1.6 ~half_width:0.7 ~half_height:0.9
                Pictures.poster;
              Room.decal ~facing:Room.Back ~glow:0.8 ~along:3. ~z:1.7
                ~half_width:0.8 ~half_height:0.5 Pictures.poster;
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
    Room.make ~thresholds:[ hall_west; hall_cellar ]
      ~floor:(ground hall_floor)
        (* Not {!Camlcast.Plane.above}, which is the floor's own slope carried up
         bodily and so a ceiling of fixed headroom. This one has a slope of its
         own, steeper than the floor's, so the two diverge: the hall is 4 cells
         high where you come in and rather more of that by the far wall, and you
         can watch the roof climb away from you as you cross it. Nothing else in
         the level does this — every other ceiling here is parallel to what it
         is over. *)
      ~ceiling:
        (roof
           (Plane.make
              ~a:(Plane.gradient hall_floor (Vec.make 1. 0.) +. 0.05)
              ~b:(Plane.gradient hall_floor (Vec.make 0. 1.))
              ~c:(Plane.elevation hall_floor (Vec.make 0. 0.) +. 4.)))
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
    Room.make ~thresholds:[ nook_south ] ~floor:(ground nook_floor)
      ~ceiling:(roof (Plane.above nook_floor 2.9))
      (nook_jambs
      @ Room.path ~height:3.2 ~material:Surfaces.tile
          [ Vec.make 3. 0.; Vec.make 0. 5.; Vec.make (-3.) 0. ])
  and garden =
    Room.make ~thresholds:[ garden_east ]
      ~floor:(ground garden_floor)
        (* A sky of its own, and the only thing about the garden that is not the
         plaza's. A {!Camlcast.Sky} belongs to the room it roofs, so two rooms
         under two skies costs a second value and nothing else; the light on the
         walls does not follow, because that is the world's one
         {!Camlcast.Atmosphere} and it lights both. *)
      ~ceiling:(Room.Open Surfaces.dusk)
      ~sprites:
        [
          (* Held clear of the floor, which nothing else in the level is: a
             sprite's [base] is where its feet are, and a barrel with its feet
             at 1.6 is a barrel sitting on nothing. *)
          Room.sprite ~base:1.6 ~size:0.9 ~image:Pictures.barrel
            (Vec.make (-3.) 0.5);
          (* Wider than it is tall — the one picture here that is — so it also
             says that a sprite is sized by its height and takes its width from
             the image. *)
          Room.sprite ~base:0.9 ~size:0.5 ~image:Pictures.motes.(0)
            (Vec.make (-5.) 2.);
        ]
      (List.concat
         [
           garden_jambs;
           [
             Room.wall ~height:7. ~material:Surfaces.stone (Vec.make 0. 5.)
               (Vec.make (-8.) 5.);
             Room.wall ~height:7. ~material:Surfaces.stone (Vec.make (-8.) 5.)
               (Vec.make (-8.) (-5.));
             Room.wall ~height:7. ~material:Surfaces.stone
               (Vec.make (-8.) (-5.)) (Vec.make 0. (-5.));
             (* A lone tall monolith. *)
             Room.wall ~height:6. ~material:Surfaces.brick
               (Vec.make (-6.) (-3.5)) (Vec.make (-4.5) (-4.5));
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
    Room.make ~thresholds:[ cellar_up ] ~floor:(ground cellar_floor)
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

(** {1 The game around it} *)

(** The cellar's index in {!default}, which the dust is put into by number
    because {!Camlcast.World.replace_room} asks for one. Taken from the world
    rather than written down, so re-ordering the rooms above cannot leave this
    pointing at the plaza. *)
let cellar_room =
  match Array.find_index (String.equal "cellar") default.World.names with
  | Some i -> i
  | None -> invalid_arg "Level: no cellar to put the dust in"

(** {2 Dust}

    The same idea as the {!Dust} demo and a quarter of the arithmetic, because
    the cellar is small and dim and a dozen motes read where seventy would be
    soup. Each one's place, size and fall come from its index alone, so the same
    dust comes back on every run and there is nothing to carry between frames:
    the state is a clock, and the motes are a function of it. The constants are
    irrational and pairwise unrelated for the reason {!Pictures.mote} gives —
    two that add to one put every mote on a diagonal.

    A mote that reaches the floor reappears at the ceiling, which is what makes
    the fall endless with nothing remembering how far round it has been. *)

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
  Room.sprite ~base:(fall *. 2.2)
    ~size:(0.3 +. (0.35 *. fraction 0.7320508076 k))
    ~image:Pictures.motes.((k + int_of_float (t *. 7.)) mod frames)
    (Vec.make (spot.Vec.x +. drift) (spot.Vec.y +. (drift *. 0.6)))

(** What the cellar was authored with, kept because
    {!Camlcast.Room.with_sprites} {e replaces} the array rather than adding to
    it: hand it only the motes and the figure standing down there disappears,
    which looks like a fault in the renderer rather than a fault here. The
    {!Dust} demo never meets this, its chamber having nothing else in it. *)
let cellar_sprites = Array.to_list (World.room default cellar_room).Room.sprites

let dusty world ~t =
  World.replace_room world ~room:cellar_room
    ~replacement:
      (Room.with_sprites
         (World.room world cellar_room)
         (cellar_sprites @ List.init motes_count (mote ~t)))

type t = {
  world : World.t;
  player : Player.t;
  elapsed : float;
  refused : float;  (** seconds left of the "nothing within reach" flash *)
}
(** {2 The state}

    A world of its own rather than {!default}, because working a door rebuilds
    one — {!Camlcast.World.set_door} hands back a new world and the old one is
    still the level at rest, which is what the tests read. *)

let start =
  { world = default; player = Player.spawn default; elapsed = 0.; refused = 0. }

(** How far from the middle of an opening you have to be to work its leaf. Long
    enough that you need not stand in the doorway — which matters, because a
    door that shuts on the square you are standing in leaves you inside it. *)
let reach = 3.2

(** The doorway with a leaf in it that the player is nearest to, if they are
    near enough to reach one. The {!Doors} demo does the same thing and says
    more about why it is nearest-wins rather than what-you-are-looking-at. *)
let nearest (world : World.t) (player : Player.t) =
  let room = World.room world player.Player.room in
  Array.to_list room.Room.thresholds
  |> List.mapi (fun i (t : Room.threshold) -> (i, t))
  |> List.filter_map (fun (i, (t : Room.threshold)) ->
      if t.Room.door = None then None
      else
        let middle = Vec.scale (Vec.add t.Room.a t.Room.b) 0.5 in
        let away = Vec.length (Vec.sub middle player.Player.pos) in
        if away <= reach then Some (away, i) else None)
  |> List.sort (fun (a, _) (b, _) -> Float.compare a b)
  |> function
  | [] -> None
  | (_, i) :: _ -> Some i

let update state ~dt ~motion ~actions =
  let player = Engine.step state.world state.player motion in
  let elapsed = Float.rem (state.elapsed +. dt) motes_period
  and fade = Float.max 0. (state.refused -. dt) in
  match
    (Input.pressed actions (Input.Key Key.e), nearest state.world player)
  with
  | false, _ -> { state with player; elapsed; refused = fade }
  | true, None -> { state with player; elapsed; refused = 0.9 }
  | true, Some threshold ->
      let next =
        match
          (World.room state.world player.Player.room).Room.thresholds.(threshold)
            .Room.door
        with
        | Some { Door.state = Door.Closed; _ } -> Door.Open
        | _ -> Door.Closed
      in
      {
        world =
          World.set_door state.world ~room:player.Player.room ~threshold next;
        player;
        elapsed;
        refused = fade;
      }

(** The dust is put in here rather than in {!update} because it is not state:
    nothing about the motes survives a frame, and a world rebuilt in [view] is a
    world the renderer sees and nothing else keeps. *)
let view state = (dusty state.world ~t:state.elapsed, state.player)

(** A crosshair that says what it is on, and nothing else. The colours are the
    {!Targets} demo's, because a reader who has seen that one should not have to
    learn a second vocabulary: a doorway reads blue, a hung picture violet, a
    plain wall amber, a sprite green, and the open sky white. *)
let overlay fb state =
  let r, g, b =
    match Sight.look (fst (view state)) state.player with
    | _ when state.refused > 0. -> (235, 80, 70)
    | Some { Sight.kind = Sight.Doorway _; _ } -> (120, 170, 240)
    | Some { Sight.kind = Sight.Wall { decal = Some _; _ }; _ } ->
        (215, 130, 235)
    | Some { Sight.kind = Sight.Sprite _; _ } -> (120, 230, 130)
    | Some _ -> (235, 195, 100)
    | None -> (245, 245, 245)
  in
  Paint.crosshair fb ~r ~g ~b

let run window =
  let+ _, ending =
    Engine.run window ~bindings:Bindings.escapable ~update ~view ~overlay start
  in
  ending
