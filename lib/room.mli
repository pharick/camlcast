(** A room: a set of wall segments in its own flat coordinate frame, with a
    floor {!Plane} below them and either a roof or the open {!Sky} above.

    A grid raycaster can only place walls on cell edges, so every wall faces
    north, south, east or west. Here a wall is an arbitrary line {e segment}
    instead, so a room may have any number of walls facing any direction — an
    octagon, a triangle, a wedge. Each wall also carries its own height, the
    {!Material} it is made of (some see-through) and any {!type-decal}s hung on
    it; the floor is an inclined plane so it need not be horizontal; the ceiling
    is either an inclined plane of its own or the open sky; and {!type-sprite}s
    stand — or float — in the world as billboards facing the player.

    A room knows nothing of any other room, or of the {!World} it is in. Every
    coordinate here is its own, and the only thing that ever relates two rooms
    is a {!World} link between a {!type-threshold} of each. That is what lets
    rooms be authored — or generated — one at a time, in any order. *)

(** {1 Surfaces} *)

type surface = { plane : Plane.t; material : Material.t }
(** A floor or a ceiling: where it is, and what it is made of.

    Concrete and open, and deliberately so. There is no constructor to close it
    in favour of: it is two independent fields with nothing derived from either,
    and every one that exists is written down as a literal at the point of use —
    [{ Room.plane = floor; material = ground }] — so closing it would mean
    inventing a two-argument constructor to protect an invariant there isn't. *)

(** Which face of a wall something is on.

    A wall is a segment and has two of them. [Front] is the side its
    {!wall.normal} points to — and since {!Vec.perp} turns a direction a quarter
    turn to its left, that is the side to the left of [a -> b]. Every room here
    is wound counter-clockwise, so for a room's own boundary [Front] is the
    inside: the side you are standing on.

    {!side_of} is how anything works out which side a {e point} is on, and
    {!Sight} reports the one the crosshair was looking from. Between them a game
    never has to reason about the winding to put a mark on the face somebody is
    facing. *)
type side = Front | Back

(** {1 Decals} *)

type decal = private {
  along : float;
  z : float;
  half_width : float;
  half_height : float;
  image : Image.t;
  facing : side;
  glow : float;
}
(** A decoration flat on a wall — a painting, a poster, a chalk mark — drawn
    over the wall's own texture. It is placed by how far [along] the wall it
    sits and how high above the floor ([z]), and reaches [half_width] to each
    side and [half_height] up and down.

    [facing] is the side of the wall it is on, and it is not optional, because
    paint is not. A wall you can see through has two faces you can look at and a
    mark drawn on the near one has nothing on the far one but the back of the
    wall; a wall you cannot see through has one face you will ever look at, and
    saying which costs nothing.

    [glow] is how much of its own light it makes, from [0.] to [1.]. At [0.] it
    is paint: lit by the room and by nothing else, so it fades into the distance
    and into the dark exactly as the wall it is on does. At [1.] it is drawn at
    its own colours wherever it is and however dark the room has become. See
    {!decal_light}.

    Private: every field stays readable, and {!val-decal} is the only way to
    make one, so what it refuses is refused of every decal that exists. Both
    halves are divisors in {!decal_column} and {!decal_row}, and [glow] out of
    range would carry {!decal_light} past the colours the picture was drawn with
    — the one thing its docstring promises cannot happen. *)

val decal :
  ?facing:side ->
  ?glow:float ->
  along:float ->
  z:float ->
  half_width:float ->
  half_height:float ->
  Image.t ->
  decal
(** A decal on the [facing] side of a wall — {!Front} unless said otherwise,
    which for a room's own boundary is the inside — making [glow] of its own
    light, which is none unless said otherwise.

    @raise Invalid_argument
      if either half extent is not positive, or if [glow] is outside [0. .. 1.].
      Each test is the negation of the passing condition, so a [nan] fails it
      rather than slipping through to be divided by a frame later. *)

val decal_light : decal -> light:float -> float
(** The light a decal is drawn in, given the light its wall is drawn in.

    [light] is the room's answer — orientation and fog together — and [glow]
    lifts the decal off it towards its own full brightness. It is an
    interpolation and not an addition, so it can only ever brighten and never
    past the colours the picture was drawn with, at any [light] and any [glow]
    in range.

    {b Why this exists.} A game whose light fails by closing the {!Atmosphere}
    in dims everything drawn, including the marks the player left to find their
    way back — and those are the one thing that has to stay readable. Dimming
    the {!Material}s instead would spare them, since a decal never takes a
    wall's colour, but that is a different-looking dark and not every game wants
    it. So which of the two a decal is subject to becomes the decal's own to
    say: keep [glow] at zero and it is paint, raise it and it is phosphorescent,
    and a game can raise it as its light dies.

    {b It is asked twice.} The renderer applies this to the wall's light, and
    again to the wall's {e fog} — because a fog factor is a fraction of a
    surface's own colour as well, the fraction the haze does not replace (see
    {!Atmosphere.fog}). Lifting both is what makes a mark at [glow = 1.] the
    colours it was painted in at any distance: lifting only the light would
    leave it at full brightness under a coat of haze, which at the far end of a
    long room is the haze and not the mark. Both are "a fraction, raised towards
    [1.] by [glow]", which is all this function is; the argument is named for
    the commoner of the two. *)

val decal_column : decal -> seen_from:side -> along:float -> int option
(** Where along a decal's width a point [along] the wall falls, as a column of
    its image — or [None] where the decal does not reach, {e including} where it
    is on the face away from [seen_from].

    This and {!decal_row} are the whole of "is this point on that decal". They
    are split in two because the renderer needs them split: the horizontal
    answer is constant down a screen column and is worked out once, the vertical
    one changes every pixel. {!Sight} asks both at once. Between them they are
    the only statement of the rule, so what can be picked stays exactly what is
    drawn.

    The face test is here, in the once-per-column half, rather than anywhere its
    two callers could have written it differently. It is why a mark chalked on
    this side of a grille is not also on the other side of it, reversed. *)

val decal_row : decal -> above:float -> int option
(** Where down a decal's height a point [above] the wall's foot falls, as a row
    of its image. A decal hangs a height above the {e floor} under the wall and
    not at an absolute elevation, so on a sloped floor it rides with the wall
    instead of tilting across it. *)

(** {1 Walls} *)

type wall = {
  a : Vec.t;  (** one endpoint *)
  b : Vec.t;  (** the other *)
  height : float;  (** how far the wall rises above the floor, in cells *)
  material : Material.t;  (** what it is made of, and whether you see through *)
  decals : decal list;  (** decorations over the material *)
  edge : Vec.t;  (** [b - a], precomputed for intersection tests *)
  length : float;  (** [|b - a|] *)
  normal : Vec.t;  (** unit vector perpendicular to the wall, for shading *)
}
(** A straight run of wall between two points.

    {b The last three fields are derived from the first two, and nothing
       recomputes them.} [edge] is [b - a], [length] is [|edge|], and [normal]
    is [edge] turned a quarter turn to its left and normalised. {!val-wall}
    computes all three at once and is the only thing that should.
    [{ w with Room.a = ... }] moves an endpoint and leaves all three describing
    the wall it used to be — silently, and each of them is something else's
    measurement: {!Ray.cast} intersects against [edge], a decal is placed along
    a [length] that no longer matches, and {!Atmosphere.face_shading} and
    {!side_of} both work off a [normal] that has turned. A functional update of
    [height], [material] or [decals] touches nothing derived and is safe;
    {!add_decal} is exactly that.

    Concrete rather than private in spite of all that, because {!Renderer}
    builds one: a door's leaf and the strip of wall above an opening are both
    drawn as though they were walls, and it copies [edge], [length] and [normal]
    across from the threshold together, for precisely the reason above. *)

val side_of : wall -> Vec.t -> side
(** Which side of a wall a point is on. A point exactly on the line counts as
    {!Front}, which matters to nothing: a wall is solid there, and the only
    caller that can reach the line is an eye pressed flat against it.

    This is what turns "where the player is standing" into "which face they are
    looking at", so it is what a game placing a mark asks — usually through
    {!Sight}, which has already asked it. *)

(** {1 Sprites} *)

type sprite = private {
  pos : Vec.t;
  base : float;
  size : float;
  image : Image.t;
}
(** An object or character standing in the world, drawn as a billboard — a flat
    {!Image} that always faces the player. It stands at [pos] and is [size]
    cells tall.

    [base] is how far its foot floats above the floor under it: [0.] for
    something resting on the ground, and anything else for a mote of dust, a
    lamp, a bird. Like a {!type-decal}'s [z] it is measured from the floor and
    not from an absolute height, so on a sloped floor a sprite rides with the
    floor rather than staying put while the ground falls away beneath it.

    Its width is not [size]. A sprite is as wide as its picture says it is — see
    {!sprite_half_width} — so a wide, short mote is drawn wide and short.

    Private for the same reason a {!type-decal} is: [size] is a divisor in
    {!sprite_row} and in {!Viewport.sprite_box}, so {!val-sprite} is the only
    way to arrive at one and what it refuses stays refused. *)

val sprite : ?base:float -> size:float -> image:Image.t -> Vec.t -> sprite
(** A sprite at [pos], [size] cells tall, made of [image]; [base] cells above
    the floor if you say so, and standing on it if you do not.

    A constructor rather than a bare record so that a floating sprite is the
    only kind anyone has to write down, exactly as {!val-wall} lets a wall
    without decals stay silent about them.

    @raise Invalid_argument
      if [size] is not positive — negated, so a [nan] is refused with it. *)

val sprite_half_width : sprite -> float
(** Half a sprite's width, in cells.

    A billboard is as tall as its [size] and as wide as its picture's shape
    makes it: a 2:1 image is drawn twice as wide as it is tall. Taking the
    aspect from the picture rather than from a field of its own means the art
    cannot be stretched by an authoring mistake — there is nothing to disagree
    with — and a square picture comes out square, which is what every sprite was
    before this existed.

    This is the only place the aspect ratio appears. {!Viewport.sprite_box}
    scales the screen box by it and {!sprite_column} reads across it, so the
    rectangle a sprite is drawn in and the rectangle it is picked in are the
    same rectangle. *)

val sprite_foot : sprite -> floor_z:float -> float
(** The elevation of a sprite's foot, given the height of the floor under it. *)

val sprite_head : sprite -> floor_z:float -> float
(** The elevation of a sprite's top. *)

val sprite_column : sprite -> lateral:float -> int option
(** Where across a sprite's width a point [lateral] cells to one side of its
    centre falls, as a column of its image — or [None] where the sprite does not
    reach. [lateral] is measured along the viewer's [right], since a billboard
    faces the viewer and has no side of its own.

    This and {!sprite_row} are to a sprite what {!decal_column} and {!decal_row}
    are to a decal: between them the only statement of "is this point on that
    picture", so what can be picked stays exactly what is drawn. {!Sight} asks
    both at once; {!Renderer} inverts them once per sprite into a screen
    rectangle and interpolates across it, which is the same rule read from the
    other end. *)

val sprite_row : sprite -> floor_z:float -> z:float -> int option
(** Where down a sprite's height an elevation [z] falls, as a row of its image,
    over a floor at [floor_z] — or [None] above its head or below its foot. *)

(** {1 Doorways} *)

type lintel = { top : float; material : Material.t }
(** The wall a doorway is cut into, so the renderer can fill the strip left
    above the opening: [top] is how far that wall rises above the floor and
    [material] is what it is made of.

    Leaving it out is a claim rather than a shrug. It says the opening already
    reaches the top of the wall it was cut into, so nothing is left standing
    over it and there is nothing above it to draw. Say it of an opening that
    stops short of that and the rows over its head fall to the room's own
    ceiling, exactly as they do over a wall that stops below one — {!Renderer}
    caps a wall at the ceiling and paints the ceiling above it, and an opening
    with no lintel is no different. What it is not is a hole through to the
    neighbour. {!doorway} takes the wall's own height for its lintel and so
    never leaves the question open; only a threshold written by hand can.

    Open, like a {!type-surface} and for the same reason: two independent fields
    written as a literal wherever one is wanted. *)

type threshold = {
  name : string;  (** what a {!World} link refers to it by *)
  a : Vec.t;  (** one endpoint *)
  b : Vec.t;  (** the other *)
  height : float;  (** how tall the opening is above the floor *)
  door : Door.t option;
      (** the leaf hung across it, if any; [None] is a bare opening *)
  lintel : lintel option;  (** the wall above the opening, if any *)
  edge : Vec.t;  (** [b - a], precomputed exactly as on a {!type-wall} *)
  length : float;  (** [|b - a|] *)
  normal : Vec.t;  (** unit vector perpendicular to the opening *)
}
(** A doorway in the room boundary — the gap in the wall loop that a {!World}
    link joins to a doorway of another room.

    Its endpoints must be given in the
    {b same winding direction as the room's own boundary walls}, because
    {!Transform.between} pairs linked endpoints in reverse: the two rooms
    describe the same opening from opposite sides, so walking the boundary
    through it in one room runs the other way in the other.
    {!Transform.between}'s docstring carries the argument in full.

    An open threshold is a portal — the neighbour is drawn through it, and the
    player walks through. A closed leaf takes its place and the step is refused;
    whether the neighbour is still drawn behind it is its material's to say, the
    same question the renderer asks of a wall. A door standing [Open] is neither
    drawn nor felt, so it behaves exactly as an opening with no door in it.

    [edge], [length] and [normal] are derived from [a] and [b] exactly as on a
    {!type-wall}, and the same warning applies: {!val-threshold} and {!doorway}
    are what compute them, and moving an endpoint by hand leaves all three
    behind. [door] and [lintel] are derived from nothing, and updating either is
    what the engine itself does — {!World.set_door} hangs a state that way. *)

val leaf : threshold -> Material.t option
(** What is drawn across this opening, if anything — {!Door.leaf} of whatever
    hangs in it, and nothing at all where nothing does. *)

val shut : threshold -> bool
(** Does this opening stop a step? Exactly when there is a leaf across it. *)

(** {1 The room itself} *)

(** What is overhead: an inclined plane of some material, or nothing at all and
    the sky beyond it. A variant rather than a {!type-surface} option, so that
    an open room says which {!Sky} it is open to and cannot forget to. *)
type ceiling =
  | Roof of surface  (** an inclined plane overhead, of some material *)
  | Open of Sky.t  (** nothing overhead, and which {!Sky} shows instead *)

type t = private {
  walls : wall array;
  thresholds : threshold array;
  floor : surface;
  ceiling : ceiling;
  sprites : sprite array;
}
(** A whole room: its boundary, the doorways cut into it, what is underfoot and
    overhead, and whatever stands in it.

    Private, not abstract: {!Renderer} walks [walls] and [thresholds] per screen
    column, {!Sight} and {!World} index into both by the position a ray reports,
    and all of that has to stay a field read. What being private buys is that
    {!make} and the three functions under {e Changing a room} are the only ways
    to arrive at one, so a room's arrays are always the ones some constructor
    built — which is what {!World} relies on when it holds a portal row parallel
    to [thresholds].

    The arrays are not mutated. A room that changes is a room built again, and
    the functions below share everything they do not replace. *)

(** {1 Building a room} *)

val wall :
  height:float ->
  material:Material.t ->
  ?decals:decal list ->
  Vec.t ->
  Vec.t ->
  wall
(** Build a wall between two points, precomputing the quantities the renderer
    and the ray caster would otherwise recompute every frame. *)

val threshold :
  name:string ->
  height:float ->
  ?door:Door.t ->
  ?lintel:lintel ->
  Vec.t ->
  Vec.t ->
  threshold
(** Build a doorway between two points, precomputing the same quantities as
    {!type-wall} — a threshold is intersected by exactly the same ray test. *)

val make :
  ?thresholds:threshold list ->
  floor:surface ->
  ceiling:ceiling ->
  ?sprites:sprite list ->
  wall list ->
  t
(** Assemble a room from its parts: the [walls] of its boundary, the
    [thresholds] cut into that boundary, what is underfoot and overhead, and the
    [sprites] standing in it. Everything optional is empty if it is not given,
    so a plain box of a room says only what it is made of.

    Lists in and arrays out, because a room is written down once and then
    indexed by every frame after it. The order they are given in is the order
    they keep, and it is the order everything else refers to them by: a wall's
    index is what {!Sight} reports and what {!add_decal} takes, and a
    threshold's is what a {!World} portal runs parallel to.

    Nothing is checked here, and there is nothing here to check — a room is
    walls and planes, each already refused at its own constructor if it could
    not be built. What can only be wrong between {e two} rooms is
    {!World.make}'s to refuse. *)

(** {1 Changing a room}

    A room is immutable, so each of these hands back another one and shares
    everything it did not have to replace. {!World.replace_room} is what puts
    the result back into the world it came from. *)

val with_sprites : t -> sprite list -> t
(** The same room with other sprites in it.

    This is how something animates. A room is immutable, so a room that changes
    is a room that is built again — but a mote of dust drifting across it has
    not moved a wall, and rebuilding the walls to move the mote costs a
    {!val-wall} per wall per frame, each one normalizing a vector to arrive back
    at the number it already had. Here the walls, the thresholds and both planes
    are the ones that were already there; only the sprite array is new.

    {b The pictures must already exist.} Selecting among images made once at
    load is the whole of animating a sprite: hand back the same room with a
    different {!type-sprite} in it and give that to {!World.replace_room}. There
    is no frame counter and no timing here, because which frame it is at this
    moment is the game's to decide and there is nothing the engine could add to
    it — and because generating a picture inside a frame is the one thing this
    is meant to make unnecessary. *)

val with_thresholds : t -> threshold array -> t
(** The same room with other thresholds in it, everything else shared.

    An engine seam rather than a game's: {!World.set_door} hangs a new state on
    one threshold and needs the room back with that one replaced. An array
    rather than a list, unlike {!with_sprites}, because its caller has one in
    hand already. It must be the same length and the same order as the array it
    replaces: a {!World}'s portals run parallel to it, and nothing here can
    check that.

    The array is copied and not adopted. A room outlives the call that made it
    and the caller's array does not have to, so a room that kept it would be a
    room somebody else could still write into — and a threshold moved after the
    fact would slip past {!World.replace_room}, which matches a doorway by where
    it is. Being private stops a room being {e built} by hand; it cannot stop a
    constructor keeping what it was handed, so this one does not. One copy per
    door opening, and none per frame. *)

val add_decal : t -> wall:int -> decal -> t
(** The same room with one more {!type-decal} on one of its walls.

    This is how a mark gets onto a wall at run time — a chalk symbol, a scorch,
    a number somebody wrote. Hand it the wall's index, which is what {!Sight}
    reports and what survives {!World.replace_room}, and a decal placed at the
    [along] and [z] {!Sight} also reported: those are the wall's own
    coordinates, so a mark put where the crosshair was is a mark drawn where the
    crosshair was.

    It goes on the {e end} of that wall's list, which is what puts it on top —
    {!Sight} picks the last decal covering a point for the same reason. Only the
    wall array and the one wall in it are new; the rest of the room, including
    every other wall, is shared.

    There is no way to take one off again, and that is not an oversight: a room
    is immutable and this returns another one, so a game that wants a mark gone
    builds the room without it. What the engine has no opinion about is how many
    marks there may be, or what they mean, or whether the player has any left.
    All of that is the game's. *)

(** {1 Walking, and the geometry {!World} shares}

    The player is a disc of radius {!Config.collision_padding} rather than a
    point, so every question below is about distance and not about containment.
    The two segment measurements are here rather than in {!World} because a step
    across a doorway is tested against both rooms with the same arithmetic. *)

val distance_to_wall : wall -> Vec.t -> float
(** Shortest distance from a point to a wall, measured to the nearest point on
    the segment rather than to the infinite line it lies on — so a point past
    the end of a wall is as far away as that end is, which is what stops a
    corner from blocking a step taken around it. *)

val blocked : t -> Vec.t -> bool
(** Is [p] too close to any wall to stand there? The player is treated as a
    small disc of radius {!Config.collision_padding}, so it stops a little short
    of a wall rather than pressing its nose flat against it. *)

val segments_cross : Vec.t -> Vec.t -> Vec.t -> Vec.t -> bool
(** Do the two segments [a1..a2] and [b1..b2] cross? Solved with the same cross
    product as {!Ray.cast}: the crossing exists when both parameters land in
    [0, 1].

    That solution does not exist for parallel segments, but two of them can
    still lie on the same line and overlap — a step taken straight along a wall
    — which counts as a crossing just as much. Those are settled separately, by
    projecting [b1..b2] onto [a1..a2] and asking whether the two spans meet. *)

val distance_between_segments : Vec.t -> Vec.t -> Vec.t -> Vec.t -> float
(** Shortest distance between the segments [a1..a2] and [b1..b2]. Segments that
    cross are no distance apart at all; for two that miss, the closest pair of
    points must include an endpoint of one of them — slide along either segment
    away from an interior closest point and the distance would keep falling — so
    the four endpoint-to-segment distances cover every remaining case. *)

val passable : t -> from:Vec.t -> dest:Vec.t -> bool
(** May the player step from [from] to [dest]? The player is a disc of radius
    {!Config.collision_padding}, so the step sweeps that disc along the segment
    [from..dest] and is refused when the swept shape touches a wall — that is,
    when the step comes within the padding of a wall {e anywhere along the way}.

    Testing the whole path and not just the destination is what makes a step
    longer than the padding safe: it can neither tunnel through a thin wall nor
    clip past the end of one, both of which land clear of every wall and would
    pass a test taken at the destination alone. *)

(** {1 Shortcuts for authoring}

    Walls in the shapes rooms are usually made of. Each returns plain
    {!type-wall}s, so a room built this way is no different from one written out
    segment by segment. *)

val path :
  ?closed:bool -> height:float -> material:Material.t -> Vec.t list -> wall list
(** Walls following a run of points; [closed] joins the last point back to the
    first, turning a polyline into a polygon. *)

val doorway :
  name:string ->
  ?door:Door.t ->
  width:float ->
  opening:float ->
  height:float ->
  material:Material.t ->
  Vec.t ->
  Vec.t ->
  wall list * threshold
(** Cut a doorway into the wall that would otherwise run from [a] to [b]: the
    two jambs left either side of a gap [width] wide in the middle, and the
    {!type-threshold} filling that gap, [opening] tall.

    The threshold comes out wound the same way as the wall it replaces, which is
    the winding rule {!Transform.between} depends on, and it takes the wall's
    own height and material as its {!type-lintel}, so the strip left above the
    opening is still drawn. Cutting both sides of a doorway this way is what
    keeps a room's boundary and its thresholds honest about each other.

    @raise Invalid_argument
      on any of three authoring mistakes, refused here rather than left to
      spread. A wall with no length has no middle to cut, and the division would
      hand back a threshold whose every coordinate is [nan] — which
      {!World.make} would then accept, because [nan] answers false to every
      ordered comparison it is asked. A doorway of no width is not a doorway.
      And one wider than the wall it is cut into leaves jambs wound backwards,
      which is the one thing every transform derived from the opening depends
      on. Each test is written as the negation of the passing condition, so a
      [nan] argument fails it rather than slipping through. *)

val regular_polygon :
  center:Vec.t ->
  radius:float ->
  sides:int ->
  rotation:float ->
  height:float ->
  material:Material.t ->
  wall list
(** A regular polygon of [sides] walls, [radius] from [center], turned by
    [rotation]. A cheap way to draw rooms and pillars whose walls face every
    direction. *)
