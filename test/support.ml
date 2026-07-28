(** Testables and fixtures shared by the per-module suites. *)

open Camlcast

let case name body = Alcotest.test_case name `Quick body

(** Floating point comparisons need slack: the maths goes through [cos], [tan]
    and a few divisions, none of which are exact. *)
let close = Alcotest.float 1e-9

let vec =
  Alcotest.testable
    (fun ppf (v : Vec.t) -> Format.fprintf ppf "(%g, %g)" v.x v.y)
    (fun (a : Vec.t) (b : Vec.t) ->
      Float.abs (a.x -. b.x) <= 1e-9 && Float.abs (a.y -. b.y) <= 1e-9)

let color =
  Alcotest.testable
    (fun ppf (c : Color.t) -> Format.fprintf ppf "#%02x%02x%02x" c.r c.g c.b)
    ( = )

(** Does [haystack] contain [needle]? For the suites that assert an error
    message is {e useful} — that it names the file, the shape or the directories
    it looked in — rather than asserting its exact wording, which would make
    rephrasing one a test failure. *)
let mentions haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.sub haystack i n = needle || at (i + 1))
  in
  n = 0 || at 0

(** {1 Fixtures} *)

(* Two materials for the fixtures to wear. They exist only to be told apart —
   the geometry suites never care what a wall looks like, only that a hit
   reports the wall it was actually cast at — so they are the simplest thing
   that is distinguishable: one flat bright, one flat dim. *)

let material brightness =
  Material.make
    ~pattern:
      (Texture.generate (fun ~u:_ ~v:_ ->
           Color.level (Color.rgb 200 200 200) brightness))

let pale = material 230
let dim = material 90

(* A material you see through, for the one test that needs the renderer's
   translucent routing to have something to route. *)
let mesh =
  Material.make
    ~pattern:
      (Texture.generate_masked (fun ~u ~v ->
           if u mod 8 < 3 || v mod 8 < 3 then
             (Color.level (Color.rgb 120 120 130) 180, 255)
           else (Color.rgb 0 0 0, 0)))

let air =
  Atmosphere.make ~haze:(Color.rgb 20 20 28) ~fog_distance:12.
    ~min_brightness:0.25 ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.6
    ~directional:0.4

(* Something to hang on a wall. Four quadrants, so a test can tell which corner
   of it a sample came from, and a clear border so the cut-out path is covered
   too. *)
let poster =
  Image.make 8 (fun ~u ~v ->
      if u = 0 || v = 0 || u = 7 || v = 7 then Image.clear
      else
        ( Color.rgb (if u < 4 then 200 else 40) (if v < 4 then 200 else 40) 0,
          255 ))

let flat_floor = { Room.plane = Plane.horizontal 0.; material = pale }

let flat_ceiling =
  Room.Roof { Room.plane = Plane.horizontal 3.; material = dim }

(** A cloudless sky, for the fixture room that is open to one. *)
let open_sky =
  Room.Open
    {
      Sky.horizon = Color.rgb 176 196 222;
      zenith = Color.rgb 40 62 126;
      sun = Color.rgb 255 246 216;
      sun_azimuth = -0.9;
      sun_height = 0.5;
      sun_radius = 0.55;
      gradient = 2.2;
    }

(** A square room, 4 x 4, its four walls given counter-clockwise so [along]
    grows predictably. Small enough that every expected distance is obvious:
    from the centre each wall is exactly 2 cells away. *)
let room =
  Room.make ~floor:flat_floor ~ceiling:flat_ceiling
    [
      Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.);
      Room.wall ~height:3. ~material:pale (Vec.make 4. 0.) (Vec.make 4. 4.);
      Room.wall ~height:3. ~material:pale (Vec.make 4. 4.) (Vec.make 0. 4.);
      Room.wall ~height:3. ~material:pale (Vec.make 0. 4.) (Vec.make 0. 0.);
    ]

(** The same room with a short free-standing dim wall, one cell east of the
    centre, spanning the ray fired east from it. Used to check that a ray keeps
    the walls behind a near one. *)
let room_with_pillar =
  Room.make ~floor:flat_floor ~ceiling:flat_ceiling
    (Array.to_list room.Room.walls
    @ [ Room.wall ~height:1. ~material:dim (Vec.make 3. 1.5) (Vec.make 3. 2.5) ]
    )

(** The same room as the only room of a world, for the suites that need a
    {!World.t} but nothing to do with doorways. *)
let world =
  World.make
    ~rooms:[ ("room", room) ]
    ~links:[] ~atmosphere:air
    ~spawn:("room", Vec.make 2. 2.)

(** Two 4 x 4 rooms joined through a doorway one cell wide, each authored in its
    own coordinates so both rooms occupy [0..4] squared and the link's transform
    has real work to do. The first is roofed and the second open to the sky, so
    a test can tell which room a thing came from.

    Both boundaries are wound counter-clockwise and both doorways are cut with
    {!Room.doorway}, which is the winding rule {!Transform.between} relies on;
    the gaps land at [y = 1.5 .. 2.5] of the first room's east wall and of the
    second's west wall, and the transform between them is a translation by
    [(-4, 0)].

    The second room has a short wall standing just inside its doorway. The two
    rooms' jambs are collinear — they are the same opening — so that wall is the
    only thing in either room that the {e other} one cannot see, which is what
    makes it possible to test that collision consults the neighbour at all.

    [door] hangs a leaf in the opening. It goes to both sides at once because
    {!World.make} refuses a link whose two thresholds disagree about one.

    [lintel] is what the strip of wall over the opening is made of, for the
    suites that need a transom you can see through. {!Room.doorway} gives the
    jambs and the lintel one material, which is exactly what a transom is not,
    so it is put back afterwards rather than asked for. *)
let joined_rooms ?door ?lintel () =
  let glaze (t : Room.threshold) =
    match lintel with
    | None -> t
    | Some material -> { t with Room.lintel = Some { Room.top = 3.; material } }
  in
  let first_jambs, east =
    Room.doorway ~name:"east" ?door ~width:1. ~opening:2. ~height:3.
      ~material:pale (Vec.make 4. 0.) (Vec.make 4. 4.)
  and second_jambs, west =
    Room.doorway ~name:"west" ?door ~width:1. ~opening:2. ~height:3.
      ~material:dim (Vec.make 0. 4.) (Vec.make 0. 0.)
  in
  let east = glaze east and west = glaze west in
  let first =
    Room.make ~thresholds:[ east ] ~floor:flat_floor ~ceiling:flat_ceiling
      (first_jambs
      @ [
          Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~material:pale (Vec.make 4. 4.) (Vec.make 0. 4.);
          Room.wall ~height:3. ~material:pale (Vec.make 0. 4.) (Vec.make 0. 0.);
        ])
  and second =
    Room.make ~thresholds:[ west ] ~floor:flat_floor ~ceiling:open_sky
      (second_jambs
      @ [
          Room.wall ~height:3. ~material:dim (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~material:dim (Vec.make 4. 0.) (Vec.make 4. 4.);
          Room.wall ~height:3. ~material:dim (Vec.make 4. 4.) (Vec.make 0. 4.);
          (* Just inside the doorway, and invisible to the first room. *)
          Room.wall ~height:1. ~material:dim (Vec.make 0.25 2.45)
            (Vec.make 1.2 2.45);
        ])
  in
  World.make
    ~rooms:[ ("first", first); ("second", second) ]
    ~links:[ (("first", "east"), ("second", "west")) ]
    ~atmosphere:air
    ~spawn:("first", Vec.make 2. 2.)

(** The pair with a bare opening between them, no door at all. *)
let two_rooms = joined_rooms ()

(** The same pair with a leaf hung in the opening, in a given state. A door goes
    to both sides at once because {!World.make} refuses a link whose two
    thresholds disagree about one — which is the invariant {!World.set_door}
    exists to keep once the world is built. *)
let two_rooms_with_a_door state = joined_rooms ~door:(Door.make ~state dim) ()

(** Shut, which is what "a door" means unless something has opened it. This is
    the fixture that puts anything down the door path of {!World.can_step} or
    the renderer. *)
let two_rooms_closed = two_rooms_with_a_door Door.Closed

(** The same pair again, with a leaf of {!mesh} shut across the opening: a door
    you cannot walk through and can see through, which is the pair of claims the
    renderer and {!Camlcast.Sight} have to keep apart. *)
let two_rooms_barred = joined_rooms ~door:(Door.make mesh) ()

(** The portal behind a threshold that is certainly linked. A world may hold
    doorways that lead nowhere yet, so [World.portals] hands back options; the
    fixtures here are all finished worlds. *)
let portal world ~room ~index = Option.get (World.portals world room).(index)

(** Two rooms joined twice over, into a loop a single step can go all the way
    round. Room a's east doorway leads into b; b's north doorway leads back into
    a's south one. Both rooms are the same 0..4 square in their own coordinates,
    so the loop closes on a geometry that could not exist — which is the point:
    nothing checks it, and a route home is the crossings and not the arithmetic.

    Shared, because a frame that goes out of a room and back into it in one step
    is the case two suites need: the player's, for the crossings it reports, and
    the engine's, for the growth hook that would otherwise never hear about it.
*)
let loop =
  let a_east_jambs, a_east =
    Room.doorway ~name:"east" ~width:1. ~opening:2. ~height:3. ~material:pale
      (Vec.make 4. 0.) (Vec.make 4. 4.)
  and a_south_jambs, a_south =
    Room.doorway ~name:"south" ~width:1. ~opening:2. ~height:3. ~material:pale
      (Vec.make 0. 0.) (Vec.make 4. 0.)
  and b_west_jambs, b_west =
    Room.doorway ~name:"west" ~width:1. ~opening:2. ~height:3. ~material:dim
      (Vec.make 0. 4.) (Vec.make 0. 0.)
  and b_north_jambs, b_north =
    Room.doorway ~name:"north" ~width:1. ~opening:2. ~height:3. ~material:dim
      (Vec.make 4. 4.) (Vec.make 0. 4.)
  in
  let a =
    Room.make ~thresholds:[ a_east; a_south ] ~floor:flat_floor
      ~ceiling:flat_ceiling
      (a_east_jambs @ a_south_jambs
      @ [
          Room.wall ~height:3. ~material:pale (Vec.make 4. 4.) (Vec.make 0. 4.);
          Room.wall ~height:3. ~material:pale (Vec.make 0. 4.) (Vec.make 0. 0.);
        ])
  and b =
    Room.make ~thresholds:[ b_west; b_north ] ~floor:flat_floor
      ~ceiling:flat_ceiling
      (b_west_jambs @ b_north_jambs
      @ [
          Room.wall ~height:3. ~material:dim (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~material:dim (Vec.make 4. 0.) (Vec.make 4. 4.);
        ])
  in
  World.make
    ~rooms:[ ("a", a); ("b", b) ]
    ~links:[ (("a", "east"), ("b", "west")); (("b", "north"), ("a", "south")) ]
    ~atmosphere:air
    ~spawn:("a", Vec.make 2. 2.)

(** Centre of the room, 2 cells from every wall. *)
let centre = Vec.make 2. 2.

let dot (a : Vec.t) (b : Vec.t) = (a.x *. b.x) +. (a.y *. b.y)

(** The nearest wall a ray meets. [Ray.cast] returns every wall the ray crosses;
    most tests only care about the closest one. *)
let nearest_hit world ~origin ~direction =
  Option.get (Ray.nearest (Ray.cast world ~origin ~direction))
