(** A world of independently-authored {!Room}s joined at their doorways.

    A {!Room} is a level in its own right, written in its own coordinates and
    knowing nothing of its neighbours. A world names a set of them and says
    which {!Room.type-threshold} of one is which threshold of another;
    everything else follows. From each such link {!make} derives the
    {!Transform} laying one room's frame onto the other's, and it is that
    transform — applied to the camera when the player walks through, and to the
    ray when the renderer looks through — that does all the work.

    Because every room has its own frame, two of them may occupy the same
    coordinates and still be separate places: a world can fold back on itself,
    and rooms may form a cycle. Nothing here assumes otherwise, which is why the
    renderer's portal recursion is bounded by {!Config.max_portal_depth} rather
    than by the shape of the graph.

    There is deliberately no world-wide compass, no global position and no
    single floor. A location is a room and a point {e in that room}, and a
    height only means anything within one room — which is why {!seam_gap}
    exists, to report where two rooms disagree about the floor at a doorway they
    share.

    {1 Worlds that grow}

    A world need not be finished. A doorway whose portal is [None] is one that
    leads nowhere {e yet}: the renderer fills it with the world's
    {!Atmosphere.haze} and {!passable} treats it as solid, so an unfinished
    world is a playable one rather than a crash waiting for the player to walk
    towards it. {!make} still refuses to produce such a world, because in a
    hand-authored level a doorway onto nowhere is a mistake; it is
    {!open_doorway}, {!add_room} and {!link} that leave one, and a generator
    that composes them can build the level ahead of the player forever.

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
    camera {e behind} the neighbour's copy of the opening — that is what it
    means to be standing in the doorway looking in — so a ray cast from there
    crosses that opening again straight away. Whoever crosses has to know which
    threshold not to cross back through. *)

type location = { room : int; pos : Vec.t }
(** A point in a named room. Only meaningful together — the same [pos] in
    another room is somewhere else entirely. *)

type t
(** A world: its rooms, what each was authored as, how they are joined, the air
    they are seen through, and where the player starts.

    Abstract, where a {!Room.t} is merely private, and the difference between
    them is what each is for. A room is {e walked}: {!Renderer} runs down its
    walls and its thresholds once per screen column, so those have to stay field
    reads. A world is {e asked}: three lookups per column and one per doorway,
    and every one of them is named below.

    What being abstract buys is that the arrays underneath cannot leave. They
    must not: {!open_doorway}, {!link} and {!replace_room} copy one level and
    share the rest, so a write into a row a caller had been handed would reach
    not only this world but the world it was grown from and every world grown
    beside it. A game holding two generations of a level at once — and
    [demo/endless.ml] holds several — would find the older ones changing under
    it. The invariant that [portals] runs parallel to each room's own thresholds
    is the one this exists to keep.

    Build one with {!make} and change it with the functions below. *)

val room : t -> int -> Room.t
(** [room world i] is the room at that index. Indices come back from
    {!Sight.look}, from a {!type-portal}'s [to_room], and from {!add_room}. *)

val room_count : t -> int
(** How many rooms there are; the indices run [0] to [room_count world - 1].
    {!add_room} appends and never inserts, so an index a frame is holding stays
    the room it named. A count and named as one — the rooms themselves are read
    one at a time through {!val-room}. *)

val name : t -> int -> string
(** What the room at that index was authored as — the name it was given in
    {!make}'s [rooms], or in {!add_room}. *)

val named : t -> string -> int option
(** The room of that name, if the world has one. {!make} takes names and hands
    back a world indexed by number, and this is the road back: a game that
    authored a room as ["cellar"] and wants to say something about it later asks
    here rather than remembering which index it came out as. *)

val doorway_count : t -> room:int -> int
(** How many doorways that room has — the same count as its own thresholds,
    which is the invariant — and so the range to read {!val-portal} over. The
    index is labelled as {!val-portal}'s and {!set_door}'s is. *)

val portal : t -> room:int -> threshold:int -> portal option
(** The link behind one doorway, by the threshold index a ray reports. [None] is
    a doorway that leads nowhere yet — see {!open_doorway}.

    One doorway rather than the room's whole row, because one is what both
    callers in the engine already wanted — {!Renderer} per column, {!Sight} per
    opening — and it costs no allocation on that path, where handing back a row
    would either allocate a copy per column or hand out the world's own. *)

val atmosphere : t -> Atmosphere.t
(** The air every room of it is seen through. *)

val with_atmosphere : t -> Atmosphere.t -> t
(** The same world seen through other air, sharing every room with the one it
    came from. This is how a level changes its light without rebuilding its
    geometry: a lamp brightening is one call and no room is touched. Named as
    {!Room.with_sprites} is, and for the same reason. *)

val spawn : t -> location
(** The room and the point the player starts in. *)

val make :
  rooms:(string * Room.t) list ->
  links:((string * string) * (string * string)) list ->
  atmosphere:Atmosphere.t ->
  spawn:string * Vec.t ->
  t
(** Assemble a world from named rooms and the links between their named
    thresholds — a link reads [(("plaza", "east"), ("hall", "west"))] — and the
    room and point to start in.

    Each link becomes two {!type-portal}s, one in each room, carrying
    {!Transform.between} and its inverse. Everything that would make a link
    meaningless is refused here as [Invalid_argument], because each is an
    authoring mistake with no sensible run-time behaviour: a room or threshold
    name that does not exist, two rooms sharing a name (the second would be
    shadowed by the first, silently), two thresholds of one room sharing a name
    (no link could tell them apart), a threshold with no length (its transform
    would collapse the world to a point), a threshold linked more than once, a
    threshold nothing links to (a hole in the wall opening onto nowhere), two
    linked thresholds differing in length or height — the opening would not line
    up, so the seam would be visible from both sides — and two that disagree
    about a door.

    What is {e not} refused is a floor mismatch across a doorway; see
    {!seam_gap}. *)

(** {1 Growing a world}

    Three primitives that a generator composes to build a level ahead of the
    player. Each appends and nothing else, so every index anything is holding
    stays valid; and each leaves a world that renders and walks, so a generator
    that stops halfway leaves the player facing a doorway onto black rather than
    an exception in the middle of a frame. *)

val open_doorway : t -> room:int -> opened:Room.t -> t
(** Replace a room already in the world with the same room, one doorway further
    on. [opened] must be [rooms.(room)] rebuilt with exactly one threshold
    appended; its walls may be anything, since nothing outside the room refers
    to them, but its existing thresholds must still describe the same openings
    in the same order.

    Appended and not inserted: every {!type-portal}'s [twin] is a bare index
    into this array, and every one of them would mean a different doorway if
    anything shifted. The new threshold's portal starts [None], so between this
    call and the {!link} that fills it the doorway is solid and shows as haze —
    which is exactly what it is. *)

val add_room : t -> name:string -> Room.t -> t * int
(** Append a room, and return the index it landed at. Every doorway it brings
    with it starts unlinked, which is what makes it possible to add a room at
    all: it is not yet joined to anything, including whatever is about to join
    it.

    The name has to be one no room already has, for the same reason {!make}
    refuses a duplicate: a name is how a room is found, so a generator that
    reused one would leave a world whose second [corridor] could never be named
    again. *)

val link : t -> int * string -> int * string -> t
(** Join two doorways that both exist and neither of which leads anywhere yet.
    The same checks as {!make}'s links, and the same two portals.

    Nothing requires the two rooms to be different, or to be near each other, or
    for the result to be consistent with any path that already runs between
    them. A link is derived from the two doorways and from nothing else, so
    joining a room to one four doorways behind it produces a corridor that
    returns you somewhere it could not possibly go. There is no global frame for
    that to contradict. *)

(** {1 Changing a room}

    The three primitives above append, and what they have appended stands. This
    is the other half of a world that changes: the same room, in the same place,
    made of something else. *)

val replace_room : t -> room:int -> replacement:Room.t -> t
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
    hanging a leaf where there is already a gap is one of the things this is for
    — but the two sides of a link have to agree about a door and this changes
    one room, so {!check} will object until the other side has been replaced
    too.

    {b A floor may open a seam.} Nothing measures whether a new floor plane
    still meets its neighbour's across a doorway, here or anywhere else;
    {!seam_gap} is what a generator's tests ask, and a floor that no longer
    meets is a step you walk into rather than an error. *)

val set_door : t -> room:int -> threshold:int -> Door.state -> t
(** Open or close the door in a doorway — on both sides of it at once.

    Locking is not here. A locked door is a closed one a game will not open, and
    that rule is the game's; see {!Door}. What this guarantees is the part that
    is not a rule — that the two sides of one door never disagree about standing
    open.

    The two sides of a link have to agree about what a door is doing, so
    changing one and not the other leaves a world {!check} refuses and a door
    the player could walk through in one direction only. Every change goes
    through here for that reason: there is no way to hold the world in the state
    where the two halves differ, because this never returns one.

    A doorway that leads nowhere yet has only the one side, and gets it.

    What it does {e not} do is move anybody. A player standing in a doorway when
    it closes is left standing there, inside a leaf — the game decides whether
    that is possible, by deciding when a door may be shut. The engine's part is
    that nothing here touches the player at all. *)

val check : t -> unit
(** Everything {!make} guarantees, asserted over a world that was grown instead:
    every room uniquely named, every room's thresholds uniquely named, every one
    of them linked, every portal's [twin] the same doorway seen from the other
    side, and the two sides of every link agreed about a door.

    That last one is asked of the rooms as they stand now and not of the
    [threshold] each {!type-portal} is carrying, which is the copy taken when
    the link was made. {!replace_room} matches a doorway by where it is rather
    than by everything about it, so a leaf may be hung into an opening that is
    already linked and the two can differ — and it is the room the renderer and
    {!passable} read that has to be right.

    A generator's tests run this; nothing at run time needs to. *)

val passable : t -> room:int -> from:Vec.t -> dest:Vec.t -> bool
(** May the player step from [from] to [dest], both in [room]'s frame?

    The room's own walls are the first answer ({!Room.passable}), but they are
    not the whole one. A doorway is a gap in this room's boundary, so nothing of
    this room stops a step taken through it — while on the other side the
    neighbour's own jamb is right there. Straddling an open threshold, a step
    that this room finds clear can be flush against a wall of the next.

    So for every portal the swept step comes near, the step is carried into the
    neighbour's frame and asked again there. Asking the neighbour cannot by
    itself make a step collide, because {!Room.passable} measures against walls
    and a doorway is a {!Room.type-threshold}, never a wall. What the question
    does catch is the wall or fitting standing just beyond an opening, which is
    otherwise as invisible to collision as it is to the eye.

    Not the whole step, though: only the part of it that reaches the neighbour.
    A room's walls answer for the space that room encloses and for no other, and
    a room that folds back on itself has walls standing — in its own coordinates
    — on {e this} side of its own doorway, which is to say in the room you are
    walking through. Handed the whole step, such a wall refuses a clear stride
    towards an open doorway from across the room. So the step is cut where it
    crosses the threshold's plane before it is carried over, with
    {!Config.collision_padding} of slack: the player is a disc, and a disc whose
    centre is still this side of an opening can already be touching something
    through it.

    That leaves one honest gap. Both halves of the question are swept-{e disc}
    measurements, so a neighbour wall standing within twice the padding of the
    opening, on the wrong side of it, still answers for a step that never gets
    there. Closing it would take a test for whether a point is inside a room,
    and there is deliberately no such thing here — see {!Room.passable} for why.

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

val crossing :
  t -> room:int -> from:Vec.t -> dest:Vec.t -> (int * portal * float) option
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
    and that is the difference between
    {e the nearest opening this step goes through} and
    {e the nearest one its line touches}. Rank first and an opening the step
    merely begins on takes the top of the ranking, fails the test there, and
    takes the real crossing down with it.

    The fraction falls out of the same two numbers, so it cannot disagree with
    them: the step's own reach across the line is their difference, which the
    test has just made strictly greater than the first with both non-negative —
    so the division is never by zero, and the fraction it gives is at least zero
    and always less than one.

    A doorway that leads nowhere yet is not a crossing, because there is nowhere
    to cross to, and {!passable} has already refused any step that would reach
    one. A shut one still is: this answers which opening a step is through, and
    {!passable} is what says no. *)

val seam_gap : t -> room:int -> portal -> float
(** By how much the two rooms either side of [portal] disagree about the height
    of the floor at the doorway they share, measured at both of its endpoints.

    Each room has its own floor {!Plane} and nothing forces two of them to meet.
    Where they do not, the doorway has a step in it: the floor visible through
    the opening sits above or below the floor you are standing on, and walking
    through jolts the camera. That is an authoring mistake, but a harmless one —
    the world still renders and is still walkable — so it is reported here for a
    test to assert on rather than raised by {!make}. {!Plane.through} builds a
    neighbour's floor from its own so the gap is zero by construction. *)
