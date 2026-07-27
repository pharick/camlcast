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
    they share.

    {1 Worlds that grow}

    A world need not be finished. A doorway whose portal is [None] is one that
    leads nowhere {e yet}: the renderer fills it with the world's
    {!Atmosphere.haze} and {!can_step} treats it as solid, so an unfinished
    world is a playable one rather than a crash waiting for the player to walk
    towards it. {!make} still refuses to produce such a world, because in a
    hand-authored level a doorway onto nowhere is a mistake; it is
    {!open_doorway}, {!add_room} and {!link} that leave one, and a generator
    that composes them can build the house ahead of the player forever.

    Everything that survives a frame refers to a room by a bare index —
    {!Player.t}'s [room], a portal's [to_room] and [twin]. Appending never
    disturbs an index, so those three primitives only ever append: a room goes
    on the end of [rooms], a threshold on the end of a room's, and its portal in
    the matching slot. Nothing is ever inserted or removed, and nothing is ever
    reclaimed. *)

type portal = {
  threshold : Room.threshold;  (** the doorway, in this room's own frame *)
  to_room : int;  (** the room on the other side *)
  twin : int;
      (** which of [to_room]'s own thresholds is this same doorway, described
          from that side *)
  onto : Transform.t;  (** this room's frame onto that one's *)
}
(** One side of a link: what a room sees when it looks at one of its own
    doorways. A link makes two of these, each the other's inverse.

    [twin] matters to anything that steps through. {!Transform.point} lands the
    camera {e behind} the neighbour's copy of the opening — that is what it means
    to be standing in the doorway looking in — so a ray cast from there crosses
    that opening again straight away. Whoever crosses has to know which threshold
    not to cross back through. *)

type location = { room : int; pos : Vec.t }
(** A point in a named room. Only meaningful together — the same [pos] in
    another room is somewhere else entirely. *)

type t = {
  rooms : Room.t array;
  names : string array;  (** [names.(i)] is what [rooms.(i)] was authored as *)
  portals : portal option array array;
      (** [portals.(i)] runs parallel to [rooms.(i).thresholds], so a ray that
          reports a threshold index can look up its portal directly. [None] is a
          doorway that leads nowhere yet — see {!open_doorway}. *)
  atmosphere : Atmosphere.t;  (** the air every room of it is seen through *)
  spawn : location;
}

let room t index = t.rooms.(index)
let portals t index = t.portals.(index)

(** Tolerance for the checks below. Lengths and heights are either written by
    hand or built from the same constants, so they should agree exactly; this
    only absorbs the last bit or two of a decimal literal. *)
let epsilon = 1e-6

(** What the two sides of a link have to agree about: whether a leaf hangs
    there, and what it is doing. [None] for a bare opening.

    Not what it is made of — see {!pair}. *)
let door_state (t : Room.threshold) =
  Option.map (fun (d : Door.t) -> d.Door.state) t.Room.door

(** Refuse two thresholds of one room sharing a name: no link could tell them
    apart, and {!link} resolves by name. *)
let check_names ~who ~room name (r : Room.t) =
  let seen = Hashtbl.create (Array.length r.Room.thresholds) in
  Array.iter
    (fun (t : Room.threshold) ->
      if Hashtbl.mem seen t.Room.name then
        invalid_arg
          (who ^ ": two thresholds named " ^ name ^ "." ^ t.Room.name);
      Hashtbl.add seen t.Room.name ())
    r.Room.thresholds;
  ignore room

(** Refuse two rooms of one world sharing a name, for the same reason as the
    thresholds above and with a sharper edge: a duplicate name does not collide,
    it {e shadows}. Rooms are resolved with [Array.find_index], which answers
    with the first, so a link — or a spawn — written for the second of two rooms
    named [hall] is silently made against the first. The world is built, it
    renders, it walks, and it is not the one that was written down. *)
let check_room_names ~who names =
  let seen = Hashtbl.create (Array.length names) in
  Array.iter
    (fun name ->
      if Hashtbl.mem seen name then invalid_arg (who ^ ": two rooms named " ^ name);
      Hashtbl.add seen name ())
    names

(** The two {!portal}s a link makes, each the other's inverse, after refusing
    everything that would make the link meaningless: a threshold with no length
    (its transform would collapse the world to a point), two thresholds
    differing in length or height (the opening would not line up, so the seam
    would be visible from both sides), and two that disagree about a door (a leaf
    on one side and an opening on the other would be a door you could see through
    from behind).

    What is compared is whether a leaf hangs there and what it is doing, not
    what it is made of: the renderer draws the near side's, so a door that is oak
    from the hall and stone from the cellar is a choice and not a mistake. The
    state is another matter — a door open from one side and closed from the other
    is one the player could walk through in only one direction, which is not a
    door. {!set_door} is how it is changed, and it changes both sides at once.

    Shared by {!make} and {!link} so a world that grew is held to exactly the
    same standard as one that was written down.

    {b Each measurement is refused by negating what would pass}, rather than by
    asserting what would fail. The two read the same for an ordinary number and
    differently for [nan], which answers false to every ordered comparison it is
    given: written the other way round a threshold of length [nan] would be
    neither long enough to reject nor different enough from its twin to reject,
    and the world would be built out of a transform that was [nan] throughout.
    {!Room.doorway} refuses the degenerate wall such a threshold comes from, and
    this is the backstop for one built any other way. *)
let pair ~who ~describe (ia, ja, (a : Room.threshold))
    (ib, jb, (b : Room.threshold)) =
  let length (t : Room.threshold) room =
    if not (t.Room.length > epsilon) then
      invalid_arg (who ^ ": threshold has no length: " ^ describe room t)
  in
  length a ia;
  length b ib;
  let both = describe ia a ^ " and " ^ describe ib b in
  if not (Float.abs (a.Room.length -. b.Room.length) <= epsilon) then
    invalid_arg (who ^ ": linked thresholds differ in length: " ^ both);
  if not (Float.abs (a.Room.height -. b.Room.height) <= epsilon) then
    invalid_arg (who ^ ": linked thresholds differ in height: " ^ both);
  if door_state a <> door_state b then
    invalid_arg (who ^ ": linked thresholds disagree about a door: " ^ both);
  let onto =
    Transform.between ~a1:a.Room.a ~a2:a.Room.b ~b1:b.Room.a ~b2:b.Room.b
  in
  ( { threshold = a; to_room = ib; twin = jb; onto },
    { threshold = b; to_room = ia; twin = ja; onto = Transform.inverse onto } )

(** Assemble a world from named rooms and the links between their named
    thresholds — a link reads [(("plaza", "east"), ("hall", "west"))] — and the
    room and point to start in.

    Each link becomes two {!portal}s, one in each room, carrying
    {!Transform.between} and its inverse. Everything that would make a link
    meaningless is refused here as [Invalid_argument], because each is an
    authoring mistake with no sensible run-time behaviour: a room or threshold
    name that does not exist, two rooms sharing a name (the second would be
    shadowed by the first, silently), two thresholds of one room sharing a name
    (no link could tell them apart), a threshold with no length (its transform would
    collapse the world to a point), a threshold linked more than once, a
    threshold nothing links to (a hole in the wall opening onto nowhere), two
    linked thresholds differing in length or height — the opening would not line
    up, so the seam would be visible from both sides — and two that disagree
    about a door.

    What is {e not} refused is a floor mismatch across a doorway; see
    {!seam_gap}. *)
let make ~rooms ~links ~atmosphere ~spawn =
  let names = Array.of_list (List.map fst rooms) in
  let values = Array.of_list (List.map snd rooms) in
  let describe room name = names.(room) ^ "." ^ name in
  let find_room name =
    match Array.find_index (String.equal name) names with
    | Some index -> index
    | None -> invalid_arg ("World.make: no room named " ^ name)
  in
  check_room_names ~who:"World.make" names;
  Array.iteri
    (fun room r -> check_names ~who:"World.make" ~room names.(room) r)
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
      let here, there =
        pair ~who:"World.make"
          ~describe:(fun room (t : Room.threshold) -> describe room t.Room.name)
          (ia, ja, a) (ib, jb, b)
      in
      fill ia ja name_a here;
      fill ib jb name_b there)
    links;
  Array.iteri
    (fun room ->
      Array.iteri (fun index -> function
        | Some _ -> ()
        | None ->
            let t = values.(room).Room.thresholds.(index) in
            invalid_arg
              ("World.make: nothing links threshold " ^ describe room t.name)))
    slots;
  let spawn_room, spawn_pos = spawn in
  {
    rooms = values;
    names;
    portals = slots;
    atmosphere;
    spawn = { room = find_room spawn_room; pos = spawn_pos };
  }

(** {1 Growing a world}

    Three primitives that a generator composes to build a house ahead of the
    player. Each appends and nothing else, so every index anything is holding
    stays valid; and each leaves a world that renders and walks, so a generator
    that stops halfway leaves the player facing a doorway onto black rather than
    an exception in the middle of a frame. *)

(** Do two thresholds describe the same opening? Not physical equality, because
    a generator that rebuilds a room from its parts hands back thresholds that
    are equal without being the same value; and not full structural equality
    either, because a door may be hung in an opening that is already there. What
    has to hold is that the {e opening} is unmoved, since that is what a
    [twin] index and a link's {!Transform} were derived from. *)
let same_opening (x : Room.threshold) (y : Room.threshold) =
  String.equal x.Room.name y.Room.name
  && x.Room.a = y.Room.a
  && x.Room.b = y.Room.b
  && x.Room.height = y.Room.height

(** Replace a room already in the world with the same room, one doorway further
    on. [opened] must be [rooms.(room)] rebuilt with exactly one threshold
    appended; its walls may be anything, since nothing outside the room refers
    to them, but its existing thresholds must still describe the same openings
    in the same order.

    Appended and not inserted: every {!portal}'s [twin] is a bare index into
    this array, and every one of them would mean a different doorway if anything
    shifted. The new threshold's portal starts [None], so between this call and
    the {!link} that fills it the doorway is solid and shows as haze — which is
    exactly what it is. *)
let open_doorway t ~room ~opened =
  let before = t.rooms.(room) in
  let n = Array.length before.Room.thresholds in
  let where = t.names.(room) in
  if Array.length opened.Room.thresholds <> n + 1 then
    invalid_arg
      (Printf.sprintf
         "World.open_doorway: %s must gain exactly one threshold, from %d to %d"
         where n
         (Array.length opened.Room.thresholds));
  Array.iteri
    (fun i (t : Room.threshold) ->
      if i < n && not (same_opening t before.Room.thresholds.(i)) then
        invalid_arg
          ("World.open_doorway: " ^ where ^ " moved its existing threshold "
         ^ t.Room.name))
    opened.Room.thresholds;
  check_names ~who:"World.open_doorway" ~room where opened;
  let rooms = Array.copy t.rooms and portals = Array.copy t.portals in
  rooms.(room) <- opened;
  portals.(room) <- Array.append t.portals.(room) [| None |];
  { t with rooms; portals }

(** Append a room, and return the index it landed at. Every doorway it brings
    with it starts unlinked, which is what makes it possible to add a room at
    all: it is not yet joined to anything, including whatever is about to join
    it.

    The name has to be one no room already has, for the reason
    {!check_room_names} gives: a generator that reused one would leave a world
    whose second [corridor] could never be named again. *)
let add_room t ~name room =
  if Array.exists (String.equal name) t.names then
    invalid_arg ("World.add_room: a room is already named " ^ name);
  check_names ~who:"World.add_room" ~room:(Array.length t.rooms) name room;
  ( {
      t with
      rooms = Array.append t.rooms [| room |];
      names = Array.append t.names [| name |];
      portals =
        Array.append t.portals
          [| Array.make (Array.length room.Room.thresholds) None |];
    },
    Array.length t.rooms )

(** Join two doorways that both exist and neither of which leads anywhere yet.
    The same checks as {!make}'s links, and the same two portals.

    Nothing requires the two rooms to be different, or to be near each other, or
    for the result to be consistent with any path that already runs between
    them. A link is derived from the two doorways and from nothing else, so
    joining a room to one four doorways behind it produces a corridor that
    returns you somewhere it could not possibly go. There is no global frame for
    that to contradict. *)
let link t (room_a, name_a) (room_b, name_b) =
  let find room name =
    let thresholds = t.rooms.(room).Room.thresholds in
    match
      Array.find_index
        (fun (x : Room.threshold) -> String.equal x.Room.name name)
        thresholds
    with
    | Some index -> (index, thresholds.(index))
    | None ->
        invalid_arg
          ("World.link: no threshold " ^ t.names.(room) ^ "." ^ name)
  in
  let describe room (x : Room.threshold) = t.names.(room) ^ "." ^ x.Room.name in
  let ja, a = find room_a name_a and jb, b = find room_b name_b in
  let free room j (x : Room.threshold) =
    if Option.is_some t.portals.(room).(j) then
      invalid_arg ("World.link: threshold linked twice: " ^ describe room x)
  in
  free room_a ja a;
  free room_b jb b;
  if room_a = room_b && ja = jb then
    invalid_arg
      ("World.link: a threshold cannot lead to itself: " ^ describe room_a a);
  let here, there = pair ~who:"World.link" ~describe (room_a, ja, a) (room_b, jb, b) in
  let portals = Array.copy t.portals in
  let fill room j portal =
    let row = Array.copy portals.(room) in
    row.(j) <- Some portal;
    portals.(room) <- row
  in
  fill room_a ja here;
  fill room_b jb there;
  { t with portals }

(** {1 Changing a room}

    The three primitives above append, and what they have appended stands. This
    is the other half of a world that changes: the same room, in the same place,
    made of something else. *)

(** Replace a room with another version of itself.

    [replacement] must describe the same openings as the room it stands in for —
    the same thresholds, in the same order, with the same names, endpoints and
    heights — and may differ in everything else: the walls, their decals, the
    floor and ceiling surfaces, the sprites, and whether a leaf hangs in a
    doorway.

    That division is not arbitrary. A threshold's endpoints are what a link's
    {!Transform} was derived from, and its position in this array is what a
    portal's [twin] is an index into, so moving or reordering one would leave
    every portal that points at it meaning a different opening — silently, and
    from the other side of the world. Nothing outside a room refers to its
    walls, its planes or its sprites, so those are free to become anything.

    This is how a sign animates, a chalk mark appears on a wall, and a room is
    lit differently on the way back than it was on the way out: build the room
    again from its parts with the one thing changed, and hand it over.

    Unlike the three primitives above, the names are not re-checked. They cannot
    have gone wrong: every threshold here has been compared by name against the
    one it replaces, so the names are exactly the ones the room already had, and
    those were checked when it was built.

    {b A door is only half here.} The [door] in an opening may change, since
    hanging a leaf where there is already a gap is one of the things this is
    for — but the two sides of a link have to agree about a door and this
    changes one room, so {!check} will object until the other side has been
    replaced too.

    {b A floor may open a seam.} Nothing measures whether a new floor plane
    still meets its neighbour's across a doorway, here or anywhere else;
    {!seam_gap} is what a generator's tests ask, and a floor that no longer
    meets is a step you walk into rather than an error. *)
let replace_room t ~room ~replacement =
  let before = t.rooms.(room) in
  let n = Array.length before.Room.thresholds in
  let where = t.names.(room) in
  if Array.length replacement.Room.thresholds <> n then
    invalid_arg
      (Printf.sprintf
         "World.replace_room: %s has %d thresholds and its replacement has %d"
         where n
         (Array.length replacement.Room.thresholds));
  Array.iteri
    (fun i (x : Room.threshold) ->
      if not (same_opening x before.Room.thresholds.(i)) then
        invalid_arg
          ("World.replace_room: " ^ where ^ " moved or reordered its threshold "
         ^ x.Room.name))
    replacement.Room.thresholds;
  let rooms = Array.copy t.rooms in
  rooms.(room) <- replacement;
  { t with rooms }

(** Open or close the door in a doorway — on both sides of it at once.

    Locking is not here. A locked door is a closed one a game will not open, and
    that rule is the game's; see {!Door}. What this guarantees is the part that
    is not a rule — that the two sides of one door never disagree about standing
    open.

    The two sides of a link have to agree about what a door is doing, so
    changing one and not the other leaves a world {!check} refuses and a door
    the player could walk through in one direction only. Every change goes
    through here for that reason: there is no way to hold the world in the
    state where the two halves differ, because this never returns one.

    A doorway that leads nowhere yet has only the one side, and gets it.

    What it does {e not} do is move anybody. A player standing in a doorway when
    it closes is left standing there, inside a leaf — the game decides whether
    that is possible, by deciding when a door may be shut. The engine's part is
    that nothing here touches the player at all. *)
let set_door t ~room ~threshold state =
  let hang world ~room ~threshold =
    let before = world.rooms.(room) in
    if threshold < 0 || threshold >= Array.length before.Room.thresholds then
      invalid_arg
        (Printf.sprintf "World.set_door: %s has no threshold %d"
           world.names.(room) threshold);
    let x = before.Room.thresholds.(threshold) in
    match x.Room.door with
    | None ->
        invalid_arg
          ("World.set_door: no door hangs in " ^ world.names.(room) ^ "."
         ^ x.Room.name)
    | Some door ->
        let thresholds = Array.copy before.Room.thresholds in
        thresholds.(threshold) <-
          { x with Room.door = Some (Door.set_state door state) };
        (* Through {!replace_room}, so that a door changed here is held to the
           same invariants as a room rebuilt for any other reason. *)
        replace_room world ~room ~replacement:{ before with Room.thresholds }
  in
  let after = hang t ~room ~threshold in
  match t.portals.(room).(threshold) with
  | None -> after
  | Some portal -> hang after ~room:portal.to_room ~threshold:portal.twin

(** Everything {!make} guarantees, asserted over a world that was grown instead:
    every room uniquely named, every room's thresholds uniquely named, every one
    of them linked, every portal's [twin] the same doorway seen from the other
    side, and the two sides of every link agreed about a door.

    That last one is asked of the rooms as they stand now and not of the
    [threshold] each {!portal} is carrying, which is the copy taken when the link
    was made. {!same_opening} lets a leaf be hung into an opening that is already
    linked, so the two can differ, and it is the room the renderer and
    {!can_step} read that has to be right.

    A generator's tests run this; nothing at run time needs to. *)
let check t =
  check_room_names ~who:"World.check" t.names;
  Array.iteri
    (fun room r -> check_names ~who:"World.check" ~room t.names.(room) r)
    t.rooms;
  Array.iteri
    (fun room row ->
      Array.iteri
        (fun index -> function
          | None ->
              let x = t.rooms.(room).Room.thresholds.(index) in
              invalid_arg
                ("World.check: nothing links threshold " ^ t.names.(room) ^ "."
               ^ x.Room.name)
          | Some portal ->
              let describe = t.names.(room) ^ "." ^ portal.threshold.Room.name in
              if Array.length t.portals.(portal.to_room) <= portal.twin then
                invalid_arg ("World.check: twin out of range: " ^ describe);
              (match t.portals.(portal.to_room).(portal.twin) with
              | Some back when back.to_room = room && back.twin = index -> ()
              | _ -> invalid_arg ("World.check: twin does not lead back: " ^ describe));
              if
                door_state t.rooms.(room).Room.thresholds.(index)
                <> door_state
                     t.rooms.(portal.to_room).Room.thresholds.(portal.twin)
              then
                invalid_arg
                  ("World.check: linked thresholds disagree about a door: "
                 ^ describe);
              if
                not
                  (same_opening portal.threshold
                     t.rooms.(room).Room.thresholds.(index))
              then
                invalid_arg
                  ("World.check: portal and threshold disagree: " ^ describe))
        row)
    t.portals

(** May the player step from [from] to [dest], both in [room]'s frame?

    The room's own walls are the first answer ({!Room.can_step}), but they are
    not the whole one. A doorway is a gap in this room's boundary, so nothing of
    this room stops a step taken through it — while on the other side the
    neighbour's own jamb is right there. Straddling an open threshold, a step
    that this room finds clear can be flush against a wall of the next.

    So for every portal the swept step comes near, the step is carried into the
    neighbour's frame and asked again there. Asking the neighbour cannot by
    itself make a step collide, because {!Room.can_step} measures against walls
    and a doorway is a {!Room.type-threshold}, never a wall. What the question
    does catch is the wall or fitting standing just beyond an opening, which is
    otherwise as invisible to collision as it is to the eye.

    Three things make an opening solid, and they are the same three that stop
    the renderer looking through it. A {b closed} leaf ({!Room.shut}): walking
    into a shut door is how you find out it is shut. A doorway that
    {b leads nowhere yet}: there is no room to carry the step into, and letting
    the player walk out through a hole in the boundary into a room that has not
    been built is the one failure the whole [option] exists to prevent. And a
    neighbour whose {b own walls} are in the way just beyond it.

    An open door is not one of them. It is a leaf swung aside, and it neither
    draws nor blocks — so opening a door never moves the player, which is a rule
    the game wants and gets for free from doing nothing here. *)
let can_step t ~room:index ~from ~dest =
  let here = t.rooms.(index) in
  let near (threshold : Room.threshold) =
    Room.distance_between_segments from dest threshold.Room.a threshold.Room.b
    < Config.collision_padding
  in
  let clear j (threshold : Room.threshold) =
    if Room.shut threshold then not (near threshold)
    else
      match t.portals.(index).(j) with
      | None -> not (near threshold)
      | Some portal ->
          (not (near threshold))
          || Room.can_step t.rooms.(portal.to_room)
               ~from:(Transform.point portal.onto from)
               ~dest:(Transform.point portal.onto dest)
  in
  let rec every j =
    j >= Array.length here.Room.thresholds
    || (clear j here.Room.thresholds.(j) && every (j + 1))
  in
  Room.can_step here ~from ~dest && every 0

(** The doorway a step from [from] to [dest] goes through, if it goes through
    one: which of this room's thresholds it was, the portal behind it — whose
    {!Player.through} the caller should then apply — and how far along the step
    the opening was met, as a fraction of it. [from] plus that fraction of the
    step is the point on the opening, which is where a caller meaning to walk a
    step doorway by doorway has to cut it.

    {1 What counts as going through}

    Which side of an opening a point is on is the cross product below, which is
    its distance from the line times the opening's length; a threshold is wound
    with its room's own boundary, so that is positive inside the room and
    negative outside it. A step goes through when it {e ends} outside, having
    started inside or on the opening itself. {!Room.segments_cross} keeps the
    question to the opening rather than to the whole line it lies on: a step
    passing the end of a doorway changes side without going through anything.

    Deliberately not a symmetric change of sign. Asking only that the two ends
    disagree would answer the same for an ordinary step and differently for the
    two that matter.

    A step that {e begins} on the line is the one a player standing in a doorway
    takes, and it goes through as much as any other; refusing it walks that
    player out of the room they are still called to be in.

    More sharply, it is what makes the {e remainder} of a step that has just
    crossed safe to ask again — which is the whole of how {!Player.slide} walks
    a leg. That remainder begins on the opening it came through and heads into
    the room beyond, so under a symmetric test the twin is refused by where the
    step {e begins}, and the point it begins at got there by being carried
    through a rotation. It is on the line to within a bit or two either way, and
    which way decides whether the player is thrown straight back through the
    doorway they just came out of. Here the twin is refused by where the step
    {e ends}, which is most of an opening's width from zero, and the coin toss
    does not arise.

    {1 Why the test is part of the ranking}

    A step can meet two openings, so they are ranked by how far along it each is
    met and the nearest wins — the others are not lost, because the caller is
    expected to ask again from there, standing in the room it has just reached.

    The test is applied while they are ranked and not to the winner afterwards,
    and that is the difference between {e the nearest opening this step goes
    through} and {e the nearest one its line touches}. Rank first and an opening
    the step merely begins on takes the top of the ranking, fails the test
    there, and takes the real crossing down with it.

    The fraction falls out of the same two numbers, so it cannot disagree with
    them: the step's own reach across the line is their difference, which the
    test has just made strictly greater than the first with both non-negative —
    so the division is never by zero and never lands outside [0, 1).

    A doorway that leads nowhere yet is not a crossing, because there is nowhere
    to cross to, and {!can_step} has already refused any step that would reach
    one. A shut one still is: this answers which opening a step is through, and
    {!can_step} is what says no. *)
let crossing t ~room ~from ~dest =
  let row = t.portals.(room) in
  let side (threshold : Room.threshold) p =
    Vec.cross threshold.Room.edge (Vec.sub p threshold.Room.a)
  in
  (* Walked by index rather than folded over, because which doorway it was is
     half the answer: an index is what {!replace_room} leaves valid and what a
     [twin] is already expressed in, where a copy of the threshold would go
     stale the moment a leaf was hung in it. *)
  let rec nearest slot best =
    if slot >= Array.length row then best
    else
      let best =
        match row.(slot) with
        | Some (portal : portal)
          when Room.segments_cross from dest portal.threshold.a
                 portal.threshold.b -> (
            let entering = side portal.threshold from
            and leaving = side portal.threshold dest in
            if entering < 0. || leaving >= 0. then best
            else
              let here = entering /. (entering -. leaving) in
              match best with
              | Some (_, _, there) when there <= here -> best
              | _ -> Some (slot, portal, here))
        | _ -> best
      in
      nearest (slot + 1) best
  in
  nearest 0 None

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
      (Plane.elevation here.Room.floor.Room.plane p
      -. Plane.elevation there.Room.floor.Room.plane
           (Transform.point portal.onto p))
  in
  Float.max (difference portal.threshold.a) (difference portal.threshold.b)
