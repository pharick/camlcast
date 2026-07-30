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

(* A grille: solid bars three texels wide every eight, in both directions, with
   clear five-by-five holes between them. Two things want it. The renderer's
   translucent routing needs something to route, and anything about picking
   {e through} a surface needs one whose alpha depends on where you look — the
   bars stop a ray and the holes do not, so a test that moves the crosshair
   across it can tell the two rules apart. Note that texel 0 of {e u} is bar, so
   a hit landing squarely on a cell boundary is on a bar and not in a hole.

   The bars across are offset by five texels, which is the one arbitrary number
   here and is chosen rather than left at zero. Every sighting test crosses this
   material level, at [Config.eye_height] over a flat floor, and
   {!Texture.row_of_height} turns that half a cell into row 32 of 64 — a
   multiple of the period, so unoffset it would be bar whatever [u] did, and the
   fixture would answer the same for every aim across it. Five puts row 32 in
   the middle of a hole, three rows clear of the bars either side, so those
   tests turn on [u] — the thing they actually vary — and no rounding at the
   edge of a texel can flip the answer. *)
let mesh =
  Material.make
    ~pattern:
      (Texture.generate_masked (fun ~u ~v ->
           if u mod 8 < 3 || (v + 5) mod 8 < 3 then
             (Color.level (Color.rgb 120 120 130) 180, 255)
           else (Color.rgb 0 0 0, 0)))

(* And a pane of glass: partly transparent at every texel and fully solid at
   none. What that buys is a see-through material with no pattern to aim at, so
   a test about a see-through {e material} — a glazed door, a transom — says
   what it means wherever the crosshair happens to land, instead of quietly
   turning into a test about which texel it landed on. *)
let glass =
  Material.make
    ~pattern:
      (Texture.generate_masked (fun ~u:_ ~v:_ ->
           (Color.level (Color.rgb 150 170 190) 210, 120)))

let air =
  Atmosphere.make ~haze:(Color.rgb 20 20 28) ~fog_distance:12.
    ~min_brightness:0.25 ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.6
    ~directional:0.4 ()

(* Something to hang on a wall. Four quadrants, so a test can tell which corner
   of it a sample came from, and a clear border so the cut-out path is covered
   too. *)
let poster =
  Image.make ~width:8 (fun ~u ~v ->
      if u = 0 || v = 0 || u = 7 || v = 7 then Image.clear
      else
        ( Color.rgb (if u < 4 then 200 else 40) (if v < 4 then 200 else 40) 0,
          255 ))

let flat_floor = { Room.plane = Plane.horizontal 0.; material = pale }

let flat_ceiling =
  Room.Roof { Room.plane = Plane.horizontal 3.; material = dim }

(** A cloudless sky, for the fixture room that is open to one. *)
let open_sky = Room.open_sky Sky.default

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
    (List.init (Room.wall_count room) (Room.wall_at room)
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
    so it is put back afterwards rather than asked for.

    [bare] takes the strip away instead of re-materialling it, leaving a
    threshold with no {!Room.type-lintel} at all — the shape {!Room.doorway}
    never produces and only a hand-built one has. It overrides [lintel], a
    transom being a lintel like any other. *)
let joined_rooms ?door ?lintel ?(bare = false) () =
  let over (t : Room.threshold) =
    if bare then Room.with_lintel t None
    else
      match lintel with
      | None -> t
      | Some material -> Room.with_lintel t (Some { Room.top = 3.; material })
  in
  let first_jambs, east =
    Room.doorway ~name:"east" ?door ~width:1. ~opening:2. ~height:3.
      ~material:pale (Vec.make 4. 0.) (Vec.make 4. 4.)
  and second_jambs, west =
    Room.doorway ~name:"west" ?door ~width:1. ~opening:2. ~height:3.
      ~material:dim (Vec.make 0. 4.) (Vec.make 0. 0.)
  in
  let east = over east and west = over west in
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
    the fixture that puts anything down the door path of {!World.passable} or
    the renderer. *)
let two_rooms_closed = two_rooms_with_a_door Door.Closed

(** The same pair again, with a leaf of {!mesh} shut across the opening: a door
    you cannot walk through and can see through, which is the pair of claims the
    renderer and {!Camlcast.Sight} have to keep apart. *)
let two_rooms_barred = joined_rooms ~door:(Door.make mesh) ()

(** Two rooms joined the same way, but with the second room's doorway set in the
    back of a blind recess — a room that folds back on itself.

    The first room is the 4 x 4 square of {!joined_rooms}, its east wall cut at
    [y = 1.5 .. 2.5]. The second is a 4 x 4 square with a slot
    [x = 1.5 .. 2, y = 1 .. 3] standing blind in the middle of it, and the
    doorway is cut into the slot's far side, the segment [(2, 3) .. (2, 1)]. The
    transform between the two is the translation [(-2, 0)], chosen so that every
    distance a test wants is checkable on paper.

    That puts the slot's back wall at [x = 1.5] of the second room's frame,
    which the link carries to [x = 3.5] of the first — half a cell
    {e inside the room the player is standing in}. Which is legal, and is the
    whole of what {!Camlcast.World} means by two rooms occupying the same
    coordinates and still being separate places. It is also the one wall of the
    second room that must never reach the first: not down a ray through the
    doorway, where it stands nearer than the doorway itself, and not down a step
    towards it, which never goes far enough to reach the room it belongs to. A
    convex neighbour cannot produce it, which is why the rest of the fixtures
    here do not.

    [blind:false] takes that one wall away and changes nothing else, so a test
    can assert the two worlds are drawn identically — the strongest form of "not
    seen through the doorway" there is.

    The low wall at [(2.2, 2.45) .. (3.2, 2.45)] is the other direction: it
    stands genuinely beyond the doorway, in the room's own body, and has to go
    on stopping a step the way {!joined_rooms}' does. *)
let recessed ?(blind = true) () =
  let first_jambs, east =
    Room.doorway ~name:"east" ~width:1. ~opening:2. ~height:3. ~material:pale
      (Vec.make 4. 0.) (Vec.make 4. 4.)
  and slot_jambs, west =
    Room.doorway ~name:"west" ~width:1. ~opening:2. ~height:3. ~material:dim
      (Vec.make 2. 3.) (Vec.make 2. 1.)
  in
  (* The slot's sides and, if it has one, its back. Wound so that every normal
     points out of the slot and into the room around it. *)
  let slot =
    [
      Room.wall ~height:3. ~material:dim (Vec.make 1.5 3.) (Vec.make 2. 3.);
      Room.wall ~height:3. ~material:dim (Vec.make 2. 1.) (Vec.make 1.5 1.);
    ]
    @
    if blind then
      [ Room.wall ~height:3. ~material:dim (Vec.make 1.5 1.) (Vec.make 1.5 3.) ]
    else []
  in
  let first =
    Room.make ~thresholds:[ east ] ~floor:flat_floor ~ceiling:flat_ceiling
      (first_jambs
      @ [
          Room.wall ~height:3. ~material:pale (Vec.make 0. 0.) (Vec.make 4. 0.);
          Room.wall ~height:3. ~material:pale (Vec.make 4. 4.) (Vec.make 0. 4.);
          Room.wall ~height:3. ~material:pale (Vec.make 0. 4.) (Vec.make 0. 0.);
        ])
  and second =
    Room.make ~thresholds:[ west ] ~floor:flat_floor ~ceiling:flat_ceiling
      (slot_jambs @ slot
      @ Room.path ~closed:true ~height:3. ~material:dim
          [ Vec.make 0. 0.; Vec.make 4. 0.; Vec.make 4. 4.; Vec.make 0. 4. ]
      @ [
          Room.wall ~height:1. ~material:dim (Vec.make 2.2 2.45)
            (Vec.make 3.2 2.45);
        ])
  in
  World.make
    ~rooms:[ ("first", first); ("second", second) ]
    ~links:[ (("first", "east"), ("second", "west")) ]
    ~atmosphere:air
    ~spawn:("first", Vec.make 2. 2.)

(** The portal behind a threshold that is certainly linked. A world may hold
    doorways that lead nowhere yet, so [World.portal] hands back an option; the
    fixtures here are all finished worlds. *)
let portal world ~room ~index =
  Option.get (World.portal world ~room ~threshold:index)

(** Every room of a world, in index order. [World.room] answers about one room
    at a time, and a suite that wants to say something about all of them — that
    each is walled all round, that some one of them is open to the sky — wants
    them as a list. Pair it with [List.iteri] where the index is wanted too. *)
let rooms world = List.init (World.room_count world) (World.room world)

(** Every doorway of every room, as [(room, threshold, portal option)]: the
    whole of a world's linkage, flattened. [World.portal] answers about one
    doorway at a time, and a suite that wants to say something about all of them
    — how many lead nowhere, that none leads out of range — would otherwise
    write the same fold over two ranges each time. *)
let doorways world =
  List.concat_map
    (fun room ->
      List.init (World.doorway_count world ~room) (fun threshold ->
          (room, threshold, World.portal world ~room ~threshold)))
    (List.init (World.room_count world) Fun.id)

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
