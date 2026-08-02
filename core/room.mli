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
    rooms be authored — or generated — one at a time, in any order.

    {1 The winding rule}

    This is the one statement of it; everything else refers here.
    {b A room's boundary is wound so that, walking a wall from [a] to [b], the
       inside of the room is on the side {!Vec.perp} of the walk direction
       points.} The demos and these pages call that counter-clockwise — the word
    it earns on paper, with y up. On the screen's map, where y grows downward,
    the same loop reads clockwise to the eye: trust the rule, not the picture.

    Each wall's [normal] is that same [perp], so for a boundary wound this way
    every normal faces {e into} the room — which is what makes [Front] the
    inside, shading face the light, and a doorway's {!Transform.between} pairing
    come out right. Wind a room the other way and every normal faces out: the
    symptom is a room that is {b black from the inside}. Reverse the walls.
    {!rectangle} and {!regular_polygon} cannot be wound wrong; {!path} and
    hand-written walls can. *)

(** {1 Surfaces} *)

type surface = { plane : Plane.t; material : Material.t }
(** A floor or a ceiling: where it is, and what it is made of.

    Concrete and open, and deliberately so: it is two independent fields with
    nothing derived from either, so there is no invariant for a closed record to
    protect. {!val-floor} and {!roof} build one at the two places one is wanted,
    and a literal — [{ Room.plane; material }] — is just as good. *)

val floor : plane:Plane.t -> material:Material.t -> surface
(** What is underfoot: a {!type-surface}, named for where it goes. The twin of
    {!roof}, so the two arguments of {!make} that bound a room vertically read
    the same way. *)

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
      if [along] or [z] is not a real number, if either half extent is not
      finite and positive, or if [glow] is outside [0. .. 1.]. Each test is the
      negation of the passing condition, so a [nan] fails it rather than
      slipping through to be divided by a frame later.

    {b Where it is, and not only how big it is.} {!decal_column} and
    {!decal_row} answer for a point unless it falls outside this decal, and a
    [nan] falls outside nothing — so one placed at [nan] is not invisible and is
    not somewhere odd. It answers for {e every} point of its wall, at texel
    column zero, which the renderer draws as a smear over the whole wall and
    {!Sight} picks in front of whatever is really there. An infinite half extent
    arrives at the same place from the other side, swallowing the wall and
    dividing to a [nan]. Both are refused here, which is the only place they can
    be. *)

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
    this side of a grille is not also on the other side of it, reversed.

    {b A mark on the [Back] face is read back to front.} [along] runs from the
    wall's [a] to its [b], and by the winding rule above that walk goes left to
    right seen from the [Front] and right to left seen from the [Back] — so the
    same offset names opposite columns of the picture depending on which side
    you are standing. The column is mirrored for the far face, and what that
    buys is that a picture is drawn the way it was authored on either face: a
    sign hung on the back of a wall is legible from behind it rather than
    written in reverse. The extent is untouched, because that is where the paint
    is; only the picture within it turns round. *)

val decal_row : decal -> above:float -> int option
(** Where down a decal's height a point [above] the wall's foot falls, as a row
    of its image. A decal hangs a height above the {e floor} under the wall and
    not at an absolute elevation, so on a sloped floor it rides with the wall
    instead of tilting across it. *)

(** {1 Walls} *)

type wall = private {
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
    computes all three at once, and each of them is something else's
    measurement: {!Ray.cast} intersects against [edge], a decal is placed along
    [length], and {!Atmosphere.face_shading} and {!side_of} both work off
    [normal].

    Private for exactly that reason. Every field stays readable — the renderer
    and the ray caster read them per column — but [{ w with Room.a = ... }]
    would move an endpoint and leave all three describing the wall it used to
    be, silently, so the update is refused at the type. The one wall anything
    ever built by hand was {!Renderer}'s — a door's leaf and the lintel strip
    are drawn as though they were walls — and {!threshold_wall} is that record
    literal, written down once. *)

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
  glow : float;
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

    [glow] is how much of its own light it makes, from [0.] to [1.], exactly as
    a {!type-decal}'s is: at [0.] the room lights it, at [1.] it is drawn in its
    own colours however dark the room and however far off. See {!sprite_light}.

    Private for the same reason a {!type-decal} is: [size] is a divisor in
    {!sprite_row} and in {!Viewport.sprite_box}, so {!val-sprite} is the only
    way to arrive at one and what it refuses stays refused. [glow] is here for
    the reason it is there — out of range it carries {!sprite_light} past the
    colours the picture was drawn in. *)

val sprite :
  ?base:float -> ?glow:float -> size:float -> image:Image.t -> Vec.t -> sprite
(** A sprite at [pos], [size] cells tall, made of [image]; [base] cells above
    the floor if you say so, and standing on it if you do not, making [glow] of
    its own light, which is none unless said otherwise.

    A constructor rather than a bare record so that a floating sprite is the
    only kind anyone has to write down, exactly as {!val-wall} lets a wall
    without decals stay silent about them.

    @raise Invalid_argument
      if [pos] or [base] is not a real number, if [size] is not finite and
      positive, or if [glow] is outside [0. .. 1.]. Negated, so a [nan] is
      refused with the rest.

    [base] for the same reason {!val-decal} holds its placement to it, and by
    the same road: it is added to the floor to find the foot, and {!sprite_row}
    answers for a height unless it falls outside the picture, so an unreal one
    puts every row of the sprite at every height. [pos] because every distance
    to this sprite is measured from it. *)

val sprite_light : sprite -> light:float -> float
(** The light a sprite is drawn in, given the light the room gives it.

    {!decal_light} for billboards, in every respect: the same interpolation, the
    same promise that it only ever brightens and never past the colours the
    picture holds, and the same asking twice — once of the light and once of the
    fog, since a fog factor is a fraction of a surface's own colour too, and
    lifting only the light would leave a glowing sprite at full brightness under
    a coat of haze.

    What differs is where the [light] comes from, and it is worth saying because
    a billboard is not a surface. A wall's light is {!Atmosphere.face_shading}
    of its normal, and a billboard has no normal to take: it turns to face the
    player, so any orientation read off it would be the player's and a barrel
    would brighten as you walked around it. What it gets instead is
    {!Atmosphere.t.ambient}, which is that model's name for the light a surface
    has facing away from the light — the one number in it that asks nothing
    about which way a thing is turned. Fog is over the top of that, as it is for
    everything.

    A sprite that ought not to be in shadow says so with [glow] rather than by
    being exempt: a lamp, a torch, a mote catching the light. That is the same
    control a decal has and the reason both have it. *)

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
    other end.

    {b And that is the one place in the engine where the two ends are not the
       same code.} A wall's are: {!Sight} and {!Renderer} both reach
    {!Texture.column_of_offset} and {!Texture.row_of_height}, so there is
    nothing to keep in step. A sprite's cannot be, because the renderer's end is
    per pixel of every billboard and this end is a dot product — the screen
    rectangle exists to turn that into one divide per column. What holds them
    equal is care, and care is worth exactly the test under it: care had them
    mirrored, {!Sight} passing the sprite's offset from the eye where this wants
    the point's offset from the sprite, and a sprite with a lopsided cut-out was
    pickable precisely where it is transparent. Nothing in this repository could
    see it — every sprite drawn here is symmetric or solid to its edges — so
    what says otherwise now is [a_sprite_is_picked_where_it_is_drawn], which
    sweeps a deliberately lopsided one and compares the answer against the
    frame.

    One caveat that survives, and is not worth removing. Dead ahead the
    crosshair falls on the box's exact centre, which for an even-width picture
    is exactly a texel boundary. The renderer arrives at that number through the
    projection and this arrives through {!sprite_half_width}; they agree to
    about [5e-16] and so may land either side of the boundary, which is a
    disagreement of one texel column in the one aiming position where a sprite
    is likeliest to be symmetric anyway. Two routes to one real number cannot be
    made to round alike, and the alternative is the renderer asking this per
    pixel. *)

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

    [top] bounds the strip in both directions that matter: {!Renderer} draws it
    from the opening's head up to [top] or the ceiling, whichever comes first,
    and {!Sight} picks it over exactly that band. Above it is the room's own
    ceiling on both sides of the question.

    Open, like a {!type-surface} and for the same reason: two independent fields
    written as a literal wherever one is wanted. *)

type threshold = private {
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
    {!type-wall}, and private for the same reason: {!val-threshold} and
    {!doorway} are what compute them, and moving an endpoint by hand would leave
    all three behind. [door] and [lintel] are derived from nothing and changing
    them is routine — {!with_door} and {!with_lintel} are the two functional
    updates, sharing everything else. *)

val leaf : threshold -> Material.t option
(** What is drawn across this opening, if anything — {!Door.leaf} of whatever
    hangs in it, and nothing at all where nothing does. *)

val shut : threshold -> bool
(** Does this opening stop a step? Exactly when there is a leaf across it. *)

val with_door : threshold -> Door.t option -> threshold
(** The same opening with another leaf hung in it — or none, which is a bare
    opening again. Every derived field is shared, since the endpoints have not
    moved. What {!World.set_door} is made of; a game reaches for that, not this,
    so the world's portals stay honest. *)

val with_lintel : threshold -> lintel option -> threshold
(** The same opening under another lintel — or under none, which claims the
    opening reaches the top of its wall (see {!type-lintel} for what that
    means). Like {!with_door}, nothing derived is touched.

    @raise Invalid_argument
      if the lintel does not sit above the opening, on exactly the terms
      {!val-threshold} refuses one on. A threshold is private so that it carries
      one invariant rather than one per route to it, and this is a route to it.
*)

val threshold_wall : threshold -> height:float -> material:Material.t -> wall
(** The threshold drawn as though it were a wall: its own endpoints, [height]
    tall and made of [material], with [edge], [length] and [normal] copied
    across rather than recomputed — they describe the same segment, and
    recomputing them per frame is what {!val-wall} exists to avoid. This is how
    {!Renderer} draws a door's leaf and the lintel strip above an opening, and
    it is a function here so that the copying rule is written down once.

    @raise Invalid_argument
      if [height] does not rise above the floor. The geometry needs no checking
      — it came from a threshold that passed the same test — but [height] comes
      from the caller, and {!val-wall}'s docstring has what a wall that does not
      rise costs. *)

val across : threshold -> threshold -> Transform.t
(** The rigid motion carrying this room's coordinates onto the neighbour's,
    given this room's threshold and the neighbour's it is linked to — that is,
    {!Transform.between} with the endpoint pairing the winding rule requires,
    written down once. {!World.make} works this out for every link itself; a
    game wants it {e before} the world exists, to author a neighbour's floor so
    it meets this room's across the doorway. *)

(** {1 The room itself} *)

(** What is overhead: an inclined plane of some material, or nothing at all and
    the sky beyond it. A variant rather than a {!type-surface} option, so that
    an open room says which {!Sky} it is open to and cannot forget to. *)
type ceiling =
  | Roof of surface  (** an inclined plane overhead, of some material *)
  | Open of Sky.t  (** nothing overhead, and which {!Sky} shows instead *)

val roof : plane:Plane.t -> material:Material.t -> ceiling
(** An inclined plane overhead: [Roof { plane; material }], so that a call to
    {!make} names its two vertical bounds the same way —
    [~floor:(Room.floor ...)] and [~ceiling:(Room.roof ...)] — instead of one
    being a record and the other a record inside a variant. *)

val open_sky : Sky.t -> ceiling
(** Nothing overhead, and which {!Sky} shows instead: [Open sky]. *)

type t
(** A whole room: its boundary, the doorways cut into it, what is underfoot and
    overhead, and whatever stands in it.

    Abstract, and for the reason {!World.t} is. Three of the five things a room
    holds are arrays, and a private record would hand those out: private stops a
    room being {e built} by hand, so {!make} and the three functions under
    {e Changing a room} stay the only ways to arrive at one, but it cannot stop
    a caller writing into an array it can read. {!World} holds a portal row
    running parallel to each room's thresholds and reaches its rooms out through
    {!World.val-room}, so a threshold moved after the fact would be a doorway
    the world still thinks it knows where to find — and because a world shares
    every level it does not replace with the world it was grown from, the write
    would reach those too. [demo/endless.ml] holds several generations of a
    level at once.

    The arrays are not mutated. A room that changes is a room built again, and
    the functions below share everything they do not replace — which is a claim
    worth making only if nothing outside here can write into one.

    {2 Reading a room's parts}

    Rooms are read a part at a time rather than by the array, because handing
    the array back is the hole this closes and handing a copy back would
    allocate once per screen column. A count and an index apiece, exactly as
    {!World.room_count} and {!World.val-room} do it — the [_at] on each is only
    because {!val-wall}, {!val-threshold} and {!val-sprite} already name the
    constructors.

    The index is bare and last on all three, which is load-bearing rather than
    an oversight beside {!add_decal}'s [~wall]: it is what makes

    {[
    List.init (wall_count room) (wall_at room)
    ]}

    the way a whole row is asked for, here and in the suites and in a game built
    on this. See {!World} for the rule the two modules share and for which
    functions sit on the other side of it. *)

val wall_count : t -> int
(** How many walls the boundary is made of. *)

val wall_at : t -> int -> wall
(** One of them, by the index {!Ray.cast} reports a hit at.

    @raise Invalid_argument if there is no wall with that index. *)

val threshold_count : t -> int
(** How many doorways are cut into it. *)

val threshold_at : t -> int -> threshold
(** One of them, by the index {!Ray.openings} and {!World.val-portal} number it
    by.

    @raise Invalid_argument if there is no threshold with that index. *)

val sprite_count : t -> int
(** How many billboards stand in it. *)

val sprite_at : t -> int -> sprite
(** One of them, by the index {!Sight} names it by.

    @raise Invalid_argument if there is no sprite with that index. *)

(** {2 Reading a room's bounds}

    A floor is always a {!type-surface} and a ceiling only sometimes is, and a
    read that has to say so — the floor's plane, a [match] on the ceiling — is
    longhand at every place that only wanted the plane. These answer the common
    questions directly; exactly one of {!ceiling_surface} and {!sky} answers
    [Some] for any room. *)

val floor_surface : t -> surface
(** What is underfoot, plane and material together, for the caller rebuilding a
    room around it — {!make} takes one of these. The two below answer the
    commoner questions without the field read. *)

val floor_plane : t -> Plane.t
(** The plane underfoot. *)

val floor_material : t -> Material.t
(** What the floor is made of. *)

val ceiling : t -> ceiling
(** What is overhead, roof or sky, for the caller that has to tell them apart —
    {!Renderer} does, once per column, because a roof is cast and a sky is
    looked up, and so does {!Sight}. The three below take one side of that
    answer without the [match].

    Unlike the floor's three, that is not a thing anything in this repository
    wants. Everything here needs to know {e which} of the two it has, so
    everything here matches; the engine calls none of the three below. They are
    for a game — a tool asking whether this is the outdoor room, a check that
    every roofed room's ceiling clears its walls — where the question really
    does have one side to it and [Option.map (fun s -> s.plane)] over a [match]
    is longhand for a one-word answer. *)

val ceiling_surface : t -> surface option
(** The roof overhead — [None] under the open sky. *)

val ceiling_plane : t -> Plane.t option
(** The plane of the roof overhead — [None] under the open sky. *)

val sky : t -> Sky.t option
(** The sky this room is open to — [None] under a roof. *)

(** {1 Building a room} *)

val wall :
  height:float ->
  material:Material.t ->
  ?decals:decal list ->
  Vec.t ->
  Vec.t ->
  wall
(** Build a wall between two points, precomputing the quantities the renderer
    and the ray caster would otherwise recompute every frame.

    @raise Invalid_argument
      if the two ends are the same point, or either is not finite. Both leave
      {!Vec.normalize} nothing to work with, and it hands a vector it cannot
      scale straight back — so [normal] would be neither a unit vector nor
      perpendicular to anything, and {!Atmosphere.face_shading}, {!side_of} and
      every decal placed along [length] read it anyway. Nothing downstream
      refuses it a second time: {!Ray.cast} finds no intersection with an edge
      of no length and {!distance_to_wall} degrades to a point, so it would
      stand in the room as a collision blocker nobody can see. Or if [height] is
      not a positive finite number, which is that same blocker reached from the
      other end of the wall: {!Renderer} takes a wall's top to be the floor
      under it plus its height and draws nothing at all unless that clears the
      floor, while {!blocked} and {!passable} never read the height in the first
      place — so a wall that does not rise is walked into and never seen. Both
      tests are written as the negation of the passing condition, so a [nan]
      fails them rather than slipping through. *)

val threshold :
  name:string ->
  height:float ->
  ?door:Door.t ->
  ?lintel:lintel ->
  Vec.t ->
  Vec.t ->
  threshold
(** Build a doorway between two points, precomputing the same quantities as
    {!type-wall} — a threshold is intersected by exactly the same ray test.

    @raise Invalid_argument
      on the same terms as {!val-wall}, and for more: a threshold's [normal] is
      what {!Transform.between} turns into the frame change a portal carries a
      player through, and its [length] is the first thing {!World.make}
      measures. The [height] carries a stake of its own, since {!World.passable}
      is flat and never consults it — an opening that does not rise is walked
      through all the same, while {!Renderer} draws it as a sliver a row deep or
      as no opening at all. And if a [lintel] is given, its [top] has to be
      finite and to reach at least the [height] it stands over: a lintel is the
      strip of wall above an opening, so one hanging below its own opening
      describes a wall that cannot be drawn. Most thresholds arrive through
      {!doorway}, which cuts one out of a wall it has already refused to cut
      into; this catches one built any other way. *)

val make :
  ?thresholds:threshold list ->
  ?sprites:sprite list ->
  floor:surface ->
  ceiling:ceiling ->
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
    it is. Being abstract is what keeps the arrays a room already holds out of
    anyone's reach; it cannot say anything about one handed in, which is what
    this copy is for. One copy per door opening, and none per frame. *)

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

val segments_cross : a1:Vec.t -> a2:Vec.t -> b1:Vec.t -> b2:Vec.t -> bool
(** Do the two segments [a1..a2] and [b1..b2] cross? Solved with the same cross
    product as {!Ray.cast}: the crossing exists when both parameters land in
    [0, 1], and — as there — whether it exists at all is asked with both lengths
    scaled out of the product, because the product is an area and only the sine
    left after dividing them out is a fact about being parallel. Here it is a
    step that gets short: movement clips a leg where it meets a doorway and asks
    again about the remainder, and a remainder called parallel is a doorway not
    reported and a room not entered.

    That solution does not exist for parallel segments, but two of them can
    still lie on the same line and overlap — a step taken straight along a wall
    — which counts as a crossing just as much. Those are settled separately, by
    projecting [b1..b2] onto [a1..a2] and asking whether the two spans meet.

    {b The same cross product means the same, bit for bit.} The denominator, the
    offset and the two quotients here and in the ray's are one expression, over
    half a million random configurations. Everything around them differs on
    purpose: a ray runs forever and has only to be ahead, a step stops and is
    bounded at both ends; the overlap branch above has no counterpart there,
    because a ray sliding along a wall meets nothing worth drawing while a step
    sliding along one still has to be stopped; and the parallel test is
    inclusive here and strict there, so a segment of no length keeps the branch
    it has always taken.

    A shared core would be the way to stop the middle drifting, and it was
    measured rather than assumed. Pinning what is worth pinning — which endpoint
    the offset runs from, and which segment's direction goes with which
    parameter — means handing back both parameters, and on this compiler a
    returned pair allocates however hard it is inlined: 327 words per cast
    against 163, and 33.9 ms against 30.1 for the same work, which is thirteen
    per cent of {!Ray.cast}. Handing back one at a time is free and shares only
    the division, leaving both of those facts written out twice as before. So
    the middle is held by a test instead —
    [a_cast_agrees_with_a_step_about_where_a_wall_is] puts what a cast reports
    to a step and fails if either side's offset or pairing moves. *)

val distance_between_segments :
  a1:Vec.t -> a2:Vec.t -> b1:Vec.t -> b2:Vec.t -> float
(** Shortest distance between the segments [a1..a2] and [b1..b2]. Segments that
    cross are no distance apart at all; for two that miss, the closest pair of
    points must include an endpoint of one of them — slide along either segment
    away from an interior closest point and the distance would keep falling — so
    the four endpoint-to-segment distances cover every remaining case. *)

val nearest_threshold :
  ?within:float -> ?where:(threshold -> bool) -> t -> Vec.t -> int option
(** The index of the threshold whose midpoint is nearest the point — among those
    [where] admits, which is all of them unless said, and no farther than
    [within] cells away, which is any distance unless said. [None] in a room
    with no threshold that qualifies.

    This is what "press the key at the door in front of you" is made of:
    [nearest_threshold ~within:reach ~where:(fun t -> t.door <> None) room pos].
    Nearest-wins rather than what-you-are-looking-at, because a player standing
    at a door is usually working it, wherever the crosshair drifted; the index
    is the one {!World.set_door} takes. *)

val clears_segment : from:Vec.t -> dest:Vec.t -> a:Vec.t -> b:Vec.t -> bool
(** May a step from [from] to [dest] be taken with the segment [a..b] — a wall,
    or a doorway that stops a step — where it is? What {!passable} asks of each
    wall in turn, and what {!World.passable} asks of a shut doorway, so that the
    two answer the question the same way.

    The rule is the swept disc: the player is a circle of
    {!Config.collision_padding} and a step that brings it against the segment
    anywhere along the way is refused.

    {b With one way out.} A player can be inside the padding without having
    walked there, because {!World.set_door} shuts a leaf without moving anybody
    and {!World.replace_room} can put a wall beside them. Under the swept rule
    alone every step from there is refused — the step away from the wall sweeps
    the same disc through the same place as the step into it — so a game that
    shuts a door behind a player it has just watched walk through freezes them
    until it opens it again. A step that does not {e decrease} the separation is
    therefore allowed. It cannot make matters worse, and one of them is enough
    to be clear.

    Passing through is refused either way. *)

val passable : t -> from:Vec.t -> dest:Vec.t -> bool
(** May the player step from [from] to [dest]? True when every wall
    {!clears_segment} — the player is a disc of radius
    {!Config.collision_padding}, so the step sweeps that disc along the segment
    [from..dest] and is refused when the swept shape touches a wall — that is,
    when the step comes within the padding of a wall {e anywhere along the way}.

    Testing the whole path and not just the destination is what makes a step
    longer than the padding safe: it can neither tunnel through a thin wall nor
    clip past the end of one, both of which land clear of every wall and would
    pass a test taken at the destination alone.

    A player already closer than the padding may step so long as it does not
    take them closer still; see {!clears_segment} for how one comes to be there
    and why leaving has to be possible. *)

(** {1 Shortcuts for authoring}

    Walls in the shapes rooms are usually made of. Each returns plain
    {!type-wall}s, so a room built this way is no different from one written out
    segment by segment. *)

val rectangle :
  height:float -> material:Material.t -> Vec.t -> Vec.t -> wall list
(** The four walls of the axis-aligned rectangle with these two opposite corners
    — the single most common room there is, which every demo used to open by
    naming four corners and a winding. The walls come out wound
    counter-clockwise whichever two opposite corners are given, so the one
    authoring mistake a box invites cannot be made through here.

    @raise Invalid_argument
      if the corners do not span an area in both axes — a flat rectangle has
      walls of no length, whose normals could not be computed — or if [height]
      is not a positive finite number, which would give four walls that block
      the way and are never drawn. Both negated, so a [nan] coordinate or height
      is refused with the flat ones. *)

val path :
  ?closed:bool -> height:float -> material:Material.t -> Vec.t list -> wall list
(** Walls following a run of points; [closed] joins the last point back to the
    first, turning a polyline into a polygon. An open run of fewer than two
    points has no wall in it and comes back empty.

    @raise Invalid_argument
      if two points in a row are the same or either is not finite — the
      {!val-wall} that would be built between them is one this refuses on its
      own account, and it is refused here so that it carries the name of the
      function the caller actually wrote. [closed] is what makes this worth
      saying twice: shutting a loop by repeating the first point at the end is
      the natural way to write one down, and it is exactly the mistake, since
      [closed] already joins them. A closed run also has to have at least three
      points, there being no polygon in fewer, and [height] has to be a positive
      finite number, on the same terms and for the same reason as the points:
      {!val-wall} would refuse it under a name the caller never wrote. *)

val cut_points : width:float -> Vec.t -> Vec.t -> Vec.t * Vec.t
(** The two points an opening [width] cells wide is cut at, in the wall from [a]
    to [b]: centred on it, with a jamb of the same length left at either end.

    {b Measured in from the ends and not out from the middle}, and the
    difference is only ever in the last bits — which is the whole of it. The
    inset is the fraction of the wall each jamb takes, so at [width = span] it
    is a plain zero, which scales the edge to nothing and gives back the very
    floats [a] and [b] arrived as.

    Out from the middle they do not come back.
    [(a + b) / 2 ± edge * width / (2 span)] is the same number on paper and a
    different one in binary, and at [width = span] — a whole side that is one
    opening, which {!doorway} allows and documents as leaving no jamb — the two
    disagree by a few times [1e-17]. Whether they happen to cancel depends on
    the coordinates: [(0,0)-(4,0)] leaves nothing either way, and
    [(0.1,0.2)-(0.7,1.3)] leaves a wall [6.21e-17] long.

    Such a wall is the invisible blocker. It is far too short for a ray to hit,
    so nothing draws it and nothing shows it is there, and it is long enough
    that {!val-wall} builds it rather than refusing it — while {!blocked}
    measures to the nearest point of a segment, making it a
    {!Config.collision_padding} disc of solid nothing at the corner of an
    opening the player is meant to walk through. It also takes a wall index, and
    those are what {!Sight} reports and what {!add_decal} counts.

    Public because {!doorway} is not the only thing that has to know where an
    opening lands: a description naming the same doorway from both of its rooms
    needs the two ends without cutting anything, which is what
    {!Camlcast.P.opening} is, and it used to be the midpoint form written out
    again. Guarded by neither — the degenerate cases are refused by whichever
    function the caller actually named, in its own words, and this is what is
    left once they have passed. [span] must be positive. *)

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
    jambs left either side of a gap [width] wide in the middle, and the
    {!type-threshold} filling that gap, [opening] tall. Usually two jambs, but a
    doorway as wide as the wall it is cut into is allowed and leaves none — the
    ends that came to nothing are dropped rather than handed back as walls of no
    length, which {!val-wall} refuses.

    The threshold comes out wound the same way as the wall it replaces, which is
    the winding rule {!Transform.between} depends on, and it takes the wall's
    own height and material as its {!type-lintel}, so the strip left above the
    opening is still drawn. Cutting both sides of a doorway this way is what
    keeps a room's boundary and its thresholds honest about each other.

    @raise Invalid_argument
      on any of five authoring mistakes, refused here rather than left to
      spread. A wall with no length, or one running off to infinity, has no
      middle to cut, and the division would hand back a threshold whose every
      coordinate is [nan] — refused again by {!val-threshold} on its own
      account, and a third time by {!World.make} if one ever reached a world,
      because each of those tests is written the way this one is. A doorway of
      no width is not a doorway. One wider than the wall it is cut into leaves
      jambs wound backwards, which is the one thing every transform derived from
      the opening depends on. A wall that does not rise above its floor is the
      blocker nobody can see that {!val-wall} exists to refuse. And an [opening]
      taller than the [height] it is cut into would become a threshold under a
      lintel hanging below it, since the wall's height is what the lintel takes
      — refused here, in the terms the caller wrote, rather than left to
      {!val-threshold} to refuse in terms of a lintel they never mentioned. Each
      test is written as the negation of the passing condition, so a [nan]
      argument fails it rather than slipping through. *)

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
    direction.

    @raise Invalid_argument
      if there are fewer than three sides, or the radius or the height is not a
      positive finite number, or the rotation or the center is not finite. Two
      sides are a pair of coincident walls wound against each other, one is a
      wall of no length, none is a room with no boundary at all, and a negative
      count would reach [List.init] and be refused under its name rather than
      this one; a polygon of no height is a ring of walls that block the way and
      are never drawn. All five are negated, so a [nan] fails them with the flat
      ones. *)
