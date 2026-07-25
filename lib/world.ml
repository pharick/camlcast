(** A world of independently-authored {!Room}s joined at their doorways.

    A {!Room} is a level in its own right, written in its own coordinates and
    knowing nothing of its neighbours. A world names a set of them and says
    which {!Room.type-threshold} of one is which threshold of another; everything
    else follows. From each such link {!make} derives the {!Transform} laying
    one room's frame onto the other's, and it is that transform — applied to the
    camera when the player walks through, and to the ray when the renderer looks
    through — that does all the work.

    Because every room has its own frame, two of them may occupy the same
    coordinates and still be separate places: a world can fold back on itself,
    and rooms may form a cycle. Nothing here assumes otherwise, which is why the
    renderer's portal recursion is bounded by {!Config.max_portal_depth} rather
    than by the shape of the graph.

    There is deliberately no world-wide compass, no global position and no
    single floor. A location is a room and a point {e in that room}, and a
    height only means anything within one room — which is why {!seam_gap}
    exists, to report where two rooms disagree about the floor at a doorway
    they share. *)

type portal = {
  threshold : Room.threshold;  (** the doorway, in this room's own frame *)
  to_room : int;  (** the room on the other side *)
  onto : Transform.t;  (** this room's frame onto that one's *)
}
(** One side of a link: what a room sees when it looks at one of its own
    doorways. A link makes two of these, each the other's inverse. *)

type location = { room : int; pos : Vec.t }
(** A point in a named room. Only meaningful together — the same [pos] in
    another room is somewhere else entirely. *)

type t = {
  rooms : Room.t array;
  names : string array;  (** [names.(i)] is what [rooms.(i)] was authored as *)
  portals : portal array array;
      (** [portals.(i)] runs parallel to [rooms.(i).thresholds], so a ray that
          reports a threshold index can look up its portal directly *)
  spawn : location;
}

let room t index = t.rooms.(index)
let portals t index = t.portals.(index)

(** Tolerance for the authoring checks in {!make}. Lengths and heights are
    written by hand, so they should agree exactly; this only absorbs the last
    bit or two of a decimal literal. *)
let epsilon = 1e-6

(** Assemble a world from named rooms and the links between their named
    thresholds — a link reads [(("plaza", "east"), ("hall", "west"))] — and the
    room and point to start in.

    Each link becomes two {!portal}s, one in each room, carrying
    {!Transform.between} and its inverse. Everything that would make a link
    meaningless is refused here as [Invalid_argument], because each is an
    authoring mistake with no sensible run-time behaviour: a room or threshold
    name that does not exist, two thresholds of one room sharing a name (no link
    could tell them apart), a threshold with no length (its transform would
    collapse the world to a point), a threshold linked more than once, a
    threshold nothing links to (a hole in the wall opening onto nowhere), and
    two linked thresholds differing in length or height — the opening would not
    line up, so the seam would be visible from both sides.

    What is {e not} refused is a floor mismatch across a doorway; see
    {!seam_gap}. *)
let make ~rooms ~links ~spawn =
  let names = Array.of_list (List.map fst rooms) in
  let values = Array.of_list (List.map snd rooms) in
  let describe room name = names.(room) ^ "." ^ name in
  let find_room name =
    match Array.find_index (String.equal name) names with
    | Some index -> index
    | None -> invalid_arg ("World.make: no room named " ^ name)
  in
  Array.iteri
    (fun room (r : Room.t) ->
      let seen = Hashtbl.create (Array.length r.thresholds) in
      Array.iter
        (fun (t : Room.threshold) ->
          if Hashtbl.mem seen t.name then
            invalid_arg
              ("World.make: two thresholds named " ^ describe room t.name);
          Hashtbl.add seen t.name ())
        r.thresholds)
    values;
  let find_threshold room name =
    let thresholds = values.(room).Room.thresholds in
    match
      Array.find_index
        (fun (t : Room.threshold) -> String.equal t.name name)
        thresholds
    with
    | Some index -> (index, thresholds.(index))
    | None -> invalid_arg ("World.make: no threshold " ^ describe room name)
  in
  (* One slot per threshold, filled as the links are read: a slot filled twice
     is a threshold linked twice, and one left empty is a threshold linked to
     nothing. *)
  let slots =
    Array.map
      (fun (r : Room.t) -> Array.make (Array.length r.thresholds) None)
      values
  in
  let fill room index name portal =
    if Option.is_some slots.(room).(index) then
      invalid_arg ("World.make: threshold linked twice: " ^ describe room name);
    slots.(room).(index) <- Some portal
  in
  List.iter
    (fun ((room_a, name_a), (room_b, name_b)) ->
      let ia = find_room room_a and ib = find_room room_b in
      let ja, a = find_threshold ia name_a
      and jb, b = find_threshold ib name_b in
      let check (t : Room.threshold) room name =
        if t.length <= epsilon then
          invalid_arg ("World.make: threshold has no length: " ^ describe room name)
      in
      check a ia name_a;
      check b ib name_b;
      let pair = describe ia name_a ^ " and " ^ describe ib name_b in
      if Float.abs (a.length -. b.length) > epsilon then
        invalid_arg ("World.make: linked thresholds differ in length: " ^ pair);
      if Float.abs (a.height -. b.height) > epsilon then
        invalid_arg ("World.make: linked thresholds differ in height: " ^ pair);
      let onto = Transform.between ~a1:a.a ~a2:a.b ~b1:b.a ~b2:b.b in
      fill ia ja name_a { threshold = a; to_room = ib; onto };
      fill ib jb name_b
        { threshold = b; to_room = ia; onto = Transform.inverse onto })
    links;
  let portals =
    Array.mapi
      (fun room ->
        Array.mapi (fun index -> function
          | Some portal -> portal
          | None ->
              let t = values.(room).Room.thresholds.(index) in
              invalid_arg
                ("World.make: nothing links threshold " ^ describe room t.name)))
      slots
  in
  let spawn_room, spawn_pos = spawn in
  {
    rooms = values;
    names;
    portals;
    spawn = { room = find_room spawn_room; pos = spawn_pos };
  }

(** May the player step from [from] to [dest], both in [room]'s frame?

    The room's own walls are the first answer ({!Room.can_step}), but they are
    not the whole one. A doorway is a gap in this room's boundary, so nothing of
    this room stops a step taken through it — while on the other side the
    neighbour's own jamb is right there. Straddling an open threshold, a step
    that this room finds clear can be flush against a wall of the next.

    So for every {e open} portal the swept step comes near, the step is carried
    into the neighbour's frame and asked again there. A doorway with a leaf is
    skipped: walking into a door is how you go through it, so it must not
    collide. *)
let can_step t ~room:index ~from ~dest =
  let near (portal : portal) =
    Room.distance_between_segments from dest portal.threshold.a
      portal.threshold.b
    < Config.collision_padding
  in
  Room.can_step t.rooms.(index) ~from ~dest
  && Array.for_all
       (fun (portal : portal) ->
         match portal.threshold.door with
         | Some _ -> true
         | None when near portal ->
             Room.can_step t.rooms.(portal.to_room)
               ~from:(Transform.point portal.onto from)
               ~dest:(Transform.point portal.onto dest)
         | None -> true)
       t.portals.(index)

(** The doorway a step from [from] to [dest] passes through, if it passes
    through one — the portal whose {!Player.through} the caller should then
    apply.

    A step could in principle cross two, so they are ranked by how far along the
    step each is met and the nearest wins. A step running {e along} an opening
    rather than through it has no such point at all; that case is ranked
    [infinity] so it can never displace a genuine crossing. *)
let crossing t ~room:index ~from ~dest =
  let step = Vec.sub dest from in
  let parameter (portal : portal) =
    let edge = portal.threshold.edge in
    let denom = Vec.cross step edge in
    if Float.abs denom < 1e-12 then infinity
    else Vec.cross (Vec.sub portal.threshold.a from) edge /. denom
  in
  Array.fold_left
    (fun best (portal : portal) ->
      if Room.segments_cross from dest portal.threshold.a portal.threshold.b
      then
        let here = parameter portal in
        match best with
        | Some (_, there) when there <= here -> best
        | _ -> Some (portal, here)
      else best)
    None
    t.portals.(index)
  |> Option.map fst

(** By how much the two rooms either side of [portal] disagree about the height
    of the floor at the doorway they share, measured at both of its endpoints.

    Each room has its own floor {!Plane} and nothing forces two of them to meet.
    Where they do not, the doorway has a step in it: the floor visible through
    the opening sits above or below the floor you are standing on, and walking
    through jolts the camera. That is an authoring mistake, but a harmless one —
    the world still renders and is still walkable — so it is reported here for a
    test to assert on rather than raised by {!make}. {!Plane.through} builds a
    neighbour's floor from its own so the gap is zero by construction. *)
let seam_gap t ~room:index portal =
  let here = t.rooms.(index) and there = t.rooms.(portal.to_room) in
  let difference p =
    Float.abs
      (Plane.elevation here.Room.floor p
      -. Plane.elevation there.Room.floor (Transform.point portal.onto p))
  in
  Float.max (difference portal.threshold.a) (difference portal.threshold.b)

(** A demonstration world of five rooms, built to exercise the whole engine at
    once — every kind of wall, both kinds of threshold, and both a roof and the
    open {!Sky}:

    - {b plaza}, open to the sky: a twelve-sided ring of tall walls around the
      spawn, six pillars of differing heights and textures, a gallery wall hung
      with a painting and a poster, a steel grille and a leaded window to look
      through, and sprites standing about. Three doorways lead out of it, cut
      into three different sides of the ring so none of them is axis aligned —
      the transforms between the plaza and its neighbours are genuine rotations,
      not just translations.
    - {b hall}, roofed: a rectangle with a bench, a corner pillar and a barrel,
      open to the plaza on one side and shut off from the cellar by a door.
    - {b nook}, roofed and low: a triangle closed off but for its one doorway.
    - {b garden}, open to the sky: a winding low wall you look over and a tall
      monolith, walled round.
    - {b cellar}, roofed and low: a small room with a figure, reached only
      through the hall's door.

    Every room's floor is the same gently tilted surface seen from its own
    frame, derived with {!Plane.through} so that {!seam_gap} is zero at every
    doorway by construction rather than by arithmetic luck. *)
let default =
  (* Doorways are cut with {!Room.doorway}, which splits the wall and returns
     the jambs alongside the threshold, so an opening and the wall it is cut
     into can never drift apart. *)
  let plaza_corner k =
    let angle = float_of_int k *. Float.pi /. 6. in
    Vec.make (11. *. cos angle) (11. *. sin angle)
  in
  let plaza_side k =
    Room.wall ~height:7. ~texture:3 (plaza_corner k)
      (plaza_corner ((k + 1) mod 12))
  in
  let plaza_gate name k =
    Room.doorway ~name ~width:2.4 ~opening:2.6 ~height:7. ~texture:3
      (plaza_corner k)
      (plaza_corner ((k + 1) mod 12))
  in
  let east_jambs, plaza_east = plaza_gate "east" 0
  and north_jambs, plaza_north = plaza_gate "north" 3
  and west_jambs, plaza_west = plaza_gate "west" 6 in
  let hall_jambs, hall_west =
    Room.doorway ~name:"west" ~width:2.4 ~opening:2.6 ~height:4.5 ~texture:1
      (Vec.make 0. 5.) (Vec.make 0. (-5.))
  and hall_door_jambs, hall_cellar =
    Room.doorway ~name:"cellar" ~door:7 ~width:1.6 ~opening:2.2 ~height:4.5
      ~texture:1 (Vec.make 6. (-5.)) (Vec.make 6. 5.)
  in
  let nook_jambs, nook_south =
    Room.doorway ~name:"south" ~width:2.4 ~opening:2.6 ~height:3.2 ~texture:4
      (Vec.make (-3.) 0.) (Vec.make 3. 0.)
  in
  let garden_jambs, garden_east =
    Room.doorway ~name:"east" ~width:2.4 ~opening:2.6 ~height:7. ~texture:3
      (Vec.make 0. (-5.)) (Vec.make 0. 5.)
  in
  let cellar_jambs, cellar_up =
    Room.doorway ~name:"up" ~door:7 ~width:1.6 ~opening:2.2 ~height:2.8
      ~texture:3 (Vec.make 0. 3.) (Vec.make 0. (-3.))
  in
  (* The transform of a link, exactly as {!make} will derive it, so a
     neighbour's floor can be built to meet this one across the doorway. *)
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
         texture, so you weave between them and see over the low ones. *)
      List.concat
        (List.init 6 (fun k ->
             let angle = float_of_int k *. Float.pi /. 3. in
             let center = Vec.make (6. *. cos angle) (6. *. sin angle) in
             let height = [| 3.5; 0.6; 2.2; 4.5; 1.3; 2.8 |].(k) in
             Room.regular_polygon ~center ~radius:0.6 ~sides:4 ~rotation:0.6
               ~height ~texture:(1 + (k mod 4))))
    and gallery =
      (* A brick wall hung with a painting and a poster. *)
      [
        Room.wall ~height:3.2 ~texture:1 (Vec.make (-3.) (-4.))
          (Vec.make 3. (-4.))
          ~decals:
            [
              {
                Room.along = 2.;
                z = 1.6;
                half_width = 0.9;
                half_height = 0.9;
                image = Image.painting;
              };
              {
                Room.along = 4.;
                z = 1.6;
                half_width = 0.7;
                half_height = 0.9;
                image = Image.poster;
              };
            ];
      ]
    and see_through =
      (* A steel grille and a leaded window, each with something behind it. *)
      [
        Room.wall ~height:2. ~texture:5 (Vec.make (-4.) 4.) (Vec.make 1. 4.);
        Room.wall ~height:2.6 ~texture:6 (Vec.make 3. 3.) (Vec.make 6. 3.);
      ]
    in
    Room.make
      ~thresholds:[ plaza_east; plaza_north; plaza_west ]
      ~floor:plaza_floor ~ceiling:None
      ~sprites:
        [
          { Room.pos = Vec.make 2.6 0.7; size = 0.9; image = Image.barrel };
          { Room.pos = Vec.make 2.6 (-0.8); size = 1.8; image = Image.figure };
          { Room.pos = Vec.make (-1.5) 5.2; size = 1.8; image = Image.figure };
          { Room.pos = Vec.make 4.5 4.3; size = 0.9; image = Image.barrel };
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
      ~floor:hall_floor
      ~ceiling:(Some (Plane.above hall_floor 4.))
      ~sprites:[ { Room.pos = Vec.make 3. (-2.); size = 0.9; image = Image.barrel } ]
      (List.concat
         [
           hall_jambs;
           hall_door_jambs;
           [
             Room.wall ~height:4.5 ~texture:1 (Vec.make 0. (-5.))
               (Vec.make 6. (-5.));
             Room.wall ~height:4.5 ~texture:1 (Vec.make 6. 5.) (Vec.make 0. 5.);
             (* A low bench you see over. *)
             Room.wall ~height:0.5 ~texture:2 (Vec.make 3. (-4.))
               (Vec.make 5. (-4.));
           ];
           Room.regular_polygon ~center:(Vec.make 4. 3.) ~radius:0.7 ~sides:4
             ~rotation:0.3 ~height:4.5 ~texture:4;
         ])
  and nook =
    Room.make ~thresholds:[ nook_south ] ~floor:nook_floor
      ~ceiling:(Some (Plane.above nook_floor 2.9))
      (nook_jambs
      @ Room.path ~height:3.2 ~texture:4
          [ Vec.make 3. 0.; Vec.make 0. 5.; Vec.make (-3.) 0. ])
  and garden =
    Room.make ~thresholds:[ garden_east ] ~floor:garden_floor ~ceiling:None
      (List.concat
         [
           garden_jambs;
           [
             Room.wall ~height:7. ~texture:3 (Vec.make 0. 5.) (Vec.make (-8.) 5.);
             Room.wall ~height:7. ~texture:3 (Vec.make (-8.) 5.)
               (Vec.make (-8.) (-5.));
             Room.wall ~height:7. ~texture:3 (Vec.make (-8.) (-5.))
               (Vec.make 0. (-5.));
             (* A lone tall monolith. *)
             Room.wall ~height:6. ~texture:1 (Vec.make (-6.) (-3.5))
               (Vec.make (-4.5) (-4.5));
           ];
           (* A winding low wall you look over into the sky beyond. *)
           Room.path ~height:0.5 ~texture:2
             [
               Vec.make (-7.) (-3.);
               Vec.make (-2.) (-2.);
               Vec.make (-4.) 1.;
               Vec.make (-1.) 3.;
               Vec.make (-6.) 4.;
             ];
         ])
  and cellar =
    Room.make ~thresholds:[ cellar_up ] ~floor:cellar_floor
      ~ceiling:(Some (Plane.above cellar_floor 2.5))
      ~sprites:[ { Room.pos = Vec.make 2.5 0.; size = 1.8; image = Image.figure } ]
      (cellar_jambs
      @ [
          Room.wall ~height:2.8 ~texture:3 (Vec.make 0. (-3.))
            (Vec.make 5. (-3.));
          Room.wall ~height:2.8 ~texture:3 (Vec.make 5. (-3.)) (Vec.make 5. 3.);
          Room.wall ~height:2.8 ~texture:3 (Vec.make 5. 3.) (Vec.make 0. 3.);
        ])
  in
  make
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
    ~spawn:("plaza", Vec.make 0. 0.)
