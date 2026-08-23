(** The parts a world is written from.

    This module is the vocabulary. A game builds descriptions out of these
    constructors, wraps them in components of its own, and never mentions a
    {!Camlcast_core.World.t}, a framebuffer or a renderer again.

    Every constructor here takes and returns plain values; a description is
    data. A component that returns one is an ordinary function, and a
    higher-order component that takes children and puts something around them is
    an ordinary function too. There is no machinery to learn beyond the list
    below.

    {1 Winding}

    A room is dark from the inside if its boundary is wound the wrong way round.
    This has been the one silent trap in this engine: nothing catches it,
    nothing wants the reversed version, and the symptom — a black room — points
    nowhere near the cause.

    {!val-boundary} removes the question by not asking it. It measures the loop
    it is given and winds it correctly, so the same corners in either order
    build the same room. The mistake becomes impossible instead of merely
    diagnosable. Write a boundary with it and the question never arises.

    A free-standing {!wall} needs no rule: it is drawn from both sides and has
    no inside for a normal to face into. *)

open Camlcast_core

type t = Prim.t Camlcast_loom.Element.t
(** A description of part of a world — what a component returns. *)

(** {1 Being looked at}

    {!wall}, {!sprite} and {!doorway} each take [on_gaze] and [on_use], because
    those are the three things an eye can stop on. {!Camlcast_core.Sight}
    defines that set, and defines it by casting the same ray the renderer draws
    with, so what can be picked is exactly what can be seen.

    [on_gaze] is called with [true] when the crosshair arrives and [false] when
    it leaves, and not once a frame in between: an enter and a leave, not a
    poll. [on_use] is called when the player works the use control while looking
    at the thing. It receives an {!Aim.spot} saying where on the thing the
    crosshair was, which is what marking a wall where the player pointed needs.
    Both may set state, and the frame after will show it.

    On a {!doorway} the handlers go on the opening rather than on the jambs
    either side of it, because what a player aims at to work a door is the door.

    {2 Give one a [key] if its siblings can change}

    The two halves of [on_gaze] are found in different ways, and only one of
    them is found afresh. The {b enter} comes from this frame's cast against
    this frame's world, so it is always the thing the player is actually looking
    at. The {b leave} has to be sent to something the crosshair has already
    moved off, so last frame's target must be recognised in a world rebuilt
    since. It is recognised by its {!Camlcast_loom.Path.t}, whose last step, for
    a child with no [key], is {e its position among its siblings}.

    A description that inserts, removes or reorders unkeyed children between
    frames therefore moves that position out from under the leave. Write one
    more wall ahead of the one being looked at, and the [false] goes to
    whichever child now stands at that position: a thing that never had the
    crosshair is told it has lost it, and the thing that did have it is never
    told. A highlight stays lit, and a handler that toggles is left inverted.
    Nothing raises; the frame is otherwise correct.

    [key] is the whole remedy, and every constructor here takes one. A keyed
    child is identified by its key and never by where it stands, so it keeps the
    crosshair — and its hook state with it — across any rearrangement of its
    siblings. Key anything a description can rearrange, and reach for a key the
    moment a list of walls or sprites is built from something that varies. A
    fixed list written out in order needs none. *)

(** {1 What is under a room and over it} *)

type surface = Prim.surface
(** A floor or a ceiling: what it is made of, and where it lies. *)

type ceiling = Prim.ceiling

val floor : ?plane:Plane.t -> Material.t -> surface
(** What the floor is made of, and the plane it lies on.

    {b The material is always said and the plane usually is not.} Omit [plane]
    and the floor is carried through a {!connect} from the room on the other
    side, by the transform that opening implies — which is the pair of ends the
    engine cut it at, so it cannot be the wrong pair. The room the {!val-spawn}
    is in has to give one, there being nothing to carry it from, and any room
    may give one to stop the carrying there. *)

val roof : ?plane:Plane.t -> ?headroom:float -> Material.t -> ceiling
(** A ceiling, over the room's own [height] unless it is told otherwise.

    {!Camlcast_core.Plane.above} keeps a plane's gradient, so an implied ceiling
    over a sloping floor slopes with it at a constant headroom.

    [headroom] is that constant, where it is not the room's height: a cellar
    whose walls stand 2.8 under a ceiling at 2.5. It is said this way rather
    than as a plane because the floor it is over may itself have been carried
    through a {!connect}, and a room cannot name a plane nothing wrote.

    [plane] is for a ceiling that {e diverges} from its floor — a hall whose
    headroom grows toward the far wall — which is the one thing neither of the
    other two can be. A room that gives one has to give its floor a plane too,
    there being nothing else to derive the divergence from. *)

val open_sky : Sky.t -> ceiling
(** Nothing overhead, and the sky that shows instead. *)

(** {1 The world} *)

val world : ?spawn:string * Vec.t -> atmosphere:Atmosphere.t -> t list -> t
(** The root of every description: the air its rooms are seen through.

    Its children are {!val-room}s and the {!connect}ions between them. A
    description has exactly one of these, as its outermost element.

    Where the player starts is a {!val-spawn} in the room they start in, which
    is why nothing here names a room. *)

val spawn : Vec.t -> t
(** Where the player starts, in the coordinates of the room that holds it.

    A child of that room rather than a room's name and a point, because the room
    is already the thing it is written inside. Exactly one description-wide. *)

type corner
(** One point on an {!val-room} outline, and how the wall {e leaving} it is
    made. *)

val room :
  ?key:string ->
  ?name:string ->
  ?outline:corner list ->
  ?height:float ->
  ?material:Material.t ->
  floor:surface ->
  ceiling:ceiling ->
  t list ->
  t
(** A room in a coordinate frame of its own: a closed [outline], and what stands
    inside it.

    {b The outline is closed, and doors are cut out of it.} That is what makes
    the winding the engine's business rather than the author's: a closed run
    says which side is in, so every wall and every opening on it is wound from
    here. It is also why removing a door puts its wall back — the leg was never
    missing, only cut.

    [height] and [material] are what a leg that does not say gets, exactly as
    they were on a boundary; a {!val-corner} overrides either.

    [name] is for diagnostics alone. Nothing in a description refers to a room
    by name — a {!connect} joins two {!val-door}s, and a door is already in one
    room — so a description that says nothing here reads by its path instead. *)

val corner :
  ?key:string ->
  ?on_gaze:(bool -> unit) ->
  ?on_use:(Aim.spot -> unit) ->
  ?decals:t list ->
  ?material:Material.t ->
  ?height:float ->
  Vec.t ->
  corner
(** A corner, and what to make of the wall running from it to the next one.

    Every argument is optional, and anything omitted falls back to what
    {!boundary} was given, so a corner that is only a corner is [corner p]. The
    arguments are exactly what {!wall} takes, because the wall this describes is
    one.

    {b The wall leaving it, not arriving at it.} The last corner of an open
    boundary therefore describes no wall at all, and {!boundary} refuses one
    carrying anything, because a handler put there would never fire — the one
    mistake this shape invites. A closed boundary has no such corner. *)

val corners : Vec.t list -> corner list
(** Every one of these points as a plain corner. The bulk form of {!val-corner},
    for a boundary that says nothing at any leg — which is most of them. *)

val boundary :
  ?key:string ->
  ?closed:bool ->
  height:float ->
  material:Material.t ->
  corner list ->
  t
(** A run of wall through these corners, each leg made to its own taste, wound
    so the room is on the inside.

    This is the one way to lay a run of wall in a description, and the whole of
    {!wall} is available at every leg of it.

    One form, not three. A closed run and an open one are this with
    {!val-corners} in front of them and nothing said at any leg, so naming them
    separately would be two names for a special case rather than two things. The
    pull towards separate names is that a run whose legs differ looks like a
    different shape of problem; it is not, and the alternative is dropping to
    {!wall} per side and giving up the winding the moment one leg wants a
    handler.

    [closed] joins the last corner back to the first, and defaults to true. Say
    [~closed:false] for a run that stops, which is a boundary with a {!doorway}
    in it. [height] and [material] are what any corner that does not say gets.

    The name says what it builds rather than its shape. It was [run], the
    ordinary word for a run of wall, and the wrong one here: a game reading
    [Camlcast] already sees {!Camlcast.Run} and {!Camlcast_core.Engine.run},
    both about playing a game rather than laying masonry, and a third [run] in
    the module a level is written in would be one word doing two jobs a facade
    apart. A free-standing {!wall} is not a boundary and does not come through
    here, so the name also says when not to reach for it.

    {b The winding is still not the caller's concern}, and it is the reason this
    takes corners rather than a list of {!wall}s. A boundary written the wrong
    way round is reversed, and every leg is reversed with it, so what a corner
    said about its wall stays true of that wall. That is one wall's worth of
    care to get right and easy to get wrong: reversing the corners and letting
    each leg travel with its own corner puts every leg one wall out, because a
    leg describes the wall it leaves and the reversal makes that the wall it
    arrives by.

    {b One thing does not survive the reversal: a hand-placed {!decal}.} Its
    [along] is measured from the wall's first point, and a reversed wall has the
    other point first, so a decal written for a boundary that turns out to need
    reversing lands mirrored along its wall. Nothing here can fix that: a decal
    arrives already built, and there is no reading [along] back out of it to
    flip. A decal whose [along] came {e from} the engine is unaffected, which is
    the ordinary case and the chalk demo's: {!Aim.spot} reports [along] on the
    wall as built, and a decal placed back at that [along] is on the same wall
    in the same frame. For hand-placing a decal on a boundary whose winding is
    unchecked, {!wall} is still there. *)

val block :
  ?key:string -> height:float -> material:Material.t -> corner list -> t
(** A closed shape standing inside a room: a pillar, a column, a plinth.

    An outline like a room's, and wound like one, but it is not the room's
    boundary and no door is cut into it. Use {!polygon} for the corners of a
    round one. For something with two ends rather than a loop — a partition, a
    bench seen over — reach for {!val-wall}. *)

type door
(** One opening: how wide, how tall, and what hangs in it.

    Made once, at the top level, for the reason {!Camlcast_loom.Element.declare}
    is: it carries the identity a {!connect} joins by, and a value made inside a
    render is a different door every frame.

    A door belongs to the room it is {!cut} into, and stands on its own. One
    that nothing connects leads nowhere {e yet}: the renderer fills it with the
    world's haze and it is solid to walk into. That is a state to build in, not
    a mistake, so {!Check} reports it as a warning and the engine builds it. *)

val door : ?name:string -> width:float -> clearance:float -> unit -> door
(** An opening [width] across and [clearance] tall.

    {b What the two sides of an opening have to agree about is here, and what
       they may differ about is on {!cut}.} A width and a height are the opening
    itself, and the engine refuses a connection between two sides that disagree;
    a leaf and a lintel are how one room presents it, and two rooms are allowed
    to present it differently — a hall may hang oak over its side of a gate and
    a cellar stone over the other.

    [clearance] has to fit under the height of every room this is cut into —
    both of them, once it is connected, and they need not be the same height.

    [name] is for diagnostics alone. *)

val cut :
  ?key:string ->
  ?leaf:Door.t ->
  ?lintel:Room.lintel ->
  ?on_gaze:(bool -> unit) ->
  ?on_use:(Aim.spot -> unit) ->
  door ->
  along:Vec.t * Vec.t ->
  t
(** Cut this door into the leg of the room's outline running between these two
    corners.

    {b The two corners are given in either order.} They name a leg; the leg's
    own winding is what the opening takes. That is the one thing this shape is
    for: a doorway wound against its room is what
    {!Camlcast_core.Transform.between} turns into a neighbour placed outside
    itself, and no check in the engine catches it.

    [leaf] is what hangs in it and [lintel] the strip left above it, which is
    the wall's own height and material unless this says otherwise. Both are
    given here rather than on the door because both may change from frame to
    frame — a leaf that swings open is a different leaf — and a door carries the
    identity a {!connect} joins by, which may not.

    The jambs either side are legs of the outline like any other, so a
    {!val-corner} dresses them. *)

val connect : door -> door -> t
(** These two doors are the same opening, seen from either side.

    A child of the world rather than of either room, so joining two rooms and
    unjoining them touches neither of them. The two have to agree about their
    width, their height and their leaf; a description that connects two that do
    not is refused in those terms. *)

val polygon :
  center:Vec.t -> radius:float -> sides:int -> rotation:float -> corner list
(** The corners of a regular polygon, ready for {!boundary}: [sides] of them,
    [radius] from [center], turned by [rotation].

    Usually a pillar. [radius] is to a corner and not to a face.

    It returns corners and not walls, so that a polygon is a boundary like any
    other and its legs can carry what any other leg can. A six-sided pillar with
    a handler on one face was not writable before. Wrap the corners in
    {!boundary} and give the height and material there. *)

val opening : width:float -> Vec.t -> Vec.t -> Vec.t * Vec.t
(** The two ends of the opening {!doorway} would cut into the wall from [a] to
    [b].

    {!doorway} works them out for itself; hiding that arithmetic is what
    {!doorway} is for. This exists for a description that must name the doorway
    in terms of both sides — "this room's floor is that room's floor, carried
    through the doorway between them" — without doing the arithmetic twice. Feed
    the pair to {!through}.

    It is literally the same arithmetic: this calls
    {!Camlcast_core.Room.cut_points}, which is what {!doorway} cuts at, so the
    two cannot land a doorway in two places. Worth stating because restating the
    formula here instead would not look wrong: the two forms of it agree on a
    wall along an axis, and part company by [6.21e-17] on an oblique one — close
    enough that a full-width opening looks placed and is not.

    At [width] equal to the wall's own length the two ends come back as [a] and
    [b] exactly; measuring in from the ends guarantees it. A description
    building its own jambs from these — [wall a p] and [wall q b], the way
    {!doorway} would — then has two walls of no length, and
    {!Camlcast_core.Room.val-wall} refuses those. That failure is intended, not
    a trap: a full-width opening has no jambs, {!doorway} drops them, and a
    description that wants one should not ask for the two walls that are not
    there.

    @raise Invalid_argument
      on the geometry {!doorway} refuses, and for the same reasons: two points
      in the same place, a width that is not positive and finite, or one wider
      than the wall it is being cut into. The check raises here rather than
      dividing, because dividing would answer with a pair of nans, and a nan
      travels — it comes back much later as a transform that will not invert. *)

val through : from:Vec.t * Vec.t -> into:Vec.t * Vec.t -> Plane.t -> Plane.t
(** A plane carried through a doorway. [from] and [into] are the same opening's
    two ends as each of its rooms writes them; the result is the plane in the
    second room's frame.

    This is how a floor meets itself across a threshold. Two rooms have no
    coordinates in common — that separation is the point of a
    {!Camlcast_core.World} — so a second floor written by hand to look right is
    a second floor that will drift. A derived one cannot: {!Check} reports a
    step in the floor at a doorway, and a plane carried through one never has
    one. *)

val threshold :
  ?key:string ->
  ?door:Door.t ->
  ?lintel:Room.lintel ->
  ?on_gaze:(bool -> unit) ->
  ?on_use:(Aim.spot -> unit) ->
  name:string ->
  height:float ->
  Vec.t ->
  Vec.t ->
  t
(** An opening between two points, with nothing cut for it.

    Reach for {!doorway} first: it cuts the opening out of a wall and hands back
    the jambs with it, so the two cannot drift apart. This is for the case that
    will not do — a lintel of a different material from the wall under it, or a
    boundary whose jambs are already drawn some other way. The caller then owns
    making the walls either side meet its ends.

    {b That ownership is the whole difference between the two names.} This is
    the one place in the engine where "threshold" and "doorway" are not the same
    word; {!Camlcast_core.Room} states the distinction. A doorway is the opening
    {e and} its jambs; this is the opening alone. Leave an end of it meeting no
    wall and the room shows its floor and sky to the horizon through the gap,
    which {!Check} reports and {!doorway} cannot produce. *)

val wall :
  ?key:string ->
  ?on_gaze:(bool -> unit) ->
  ?on_use:(Aim.spot -> unit) ->
  ?decals:t list ->
  height:float ->
  material:Material.t ->
  Vec.t ->
  Vec.t ->
  t
(** One segment, from one point to another, with any {!decal}s hung on it.

    For a room's boundary use {!val-boundary} instead; every leg of one takes
    all of this. This is for what stands on its own — a partition, a bench seen
    over, a monolith. *)

val decal :
  ?key:string ->
  ?facing:Room.side ->
  ?glow:float ->
  along:float ->
  z:float ->
  half_width:float ->
  half_height:float ->
  Image.t ->
  t
(** A picture flat on the wall it is given to, [along] its length and [z] above
    the floor.

    [facing] defaults to the inside of the room. [glow] is how much light the
    decal makes of its own, and defaults to none. *)

val doorway :
  ?key:string ->
  ?door:Door.t ->
  ?on_gaze:(bool -> unit) ->
  ?on_use:(Aim.spot -> unit) ->
  name:string ->
  width:float ->
  opening:float ->
  height:float ->
  material:Material.t ->
  Vec.t ->
  Vec.t ->
  t
(** A wall with a doorway cut through the middle of it: the jambs either side
    and the opening between them, which is what a {!link} joins.

    [width] is how wide the opening is and [opening] how tall, under a lintel
    that reaches [height]. The jambs and the threshold are made together and
    cannot drift apart, which is the reason to cut a doorway rather than place
    one.

    [key] goes on the three of them together. What a game rearranges is the
    doorway, and no one part of it is the doorway. The key therefore belongs
    where {!Camlcast_loom.Element.fragment} takes one, not on a primitive. *)

val sprite :
  ?key:string ->
  ?on_gaze:(bool -> unit) ->
  ?on_use:(Aim.spot -> unit) ->
  ?base:float ->
  ?glow:float ->
  size:float ->
  image:Image.t ->
  Vec.t ->
  t
(** A billboard standing at a point, [size] cells tall, turning to face the
    player. [base] floats it above the floor; without it, the sprite stands on
    the floor.

    [glow] is how much light the sprite makes of its own, and defaults to none —
    the same control {!decal} has, with the same range. A sprite with none is
    lit by the room like everything else in it. For a billboard that means
    {!Camlcast_core.Atmosphere.t.ambient}, there being no facing to take; see
    {!Camlcast_core.Room.sprite_light}. [glow] is for a lamp, a torch or a
    will-o'-the-wisp, and shows when a game's light is turned down.

    Key anything that can be rearranged. A list of sprites that sorts itself is
    exactly the case keys exist for. *)

val camera : ?pitch:float -> pos:Vec.t -> angle:float -> unit -> t
(** Put the eye here, instead of letting the runtime walk it.

    A child of the room it is in, in that room's own coordinates — the same way
    a {!val-spawn} says where the player starts, and for the same reason: the
    room is already the thing it is written inside. While one of these is in a
    description the controls do not move the player at all: the description says
    where the eye is every frame, and a walk it did not ask for would fight
    that. Take it out again and the player carries on from wherever the
    description last put the eye.

    [angle] is in radians. [pitch] is the fraction of the window height the
    horizon is shifted by, the same measure the mouse gives.

    {b The world is not worked through this eye either.} The view is the
    description's, so [on_gaze] and [on_use] are silent for as long as one of
    these is placed: a pan across a room does not drag an enter and a leave over
    everything it sweeps past, and the use control does not work whatever the
    camera happens to be facing. See {!Run.aiming}. {!Events.crossings} is empty
    for the same reason on the other axis: putting the eye somewhere is a jump
    and not a walk, so there is no path along which a doorway was gone through.

    {b One to a world.} A description that places the camera twice is saying two
    things, and the last one written wins. The rule is stated here so it need
    not be discovered, since two components can each be sure they hold the eye.
    {!Check} reports the ones being overruled. *)

(** {1 The layer over the top}

    Everything below draws on the finished frame, in the order it is written, in
    the framebuffer's own pixels — which are not the window's. The engine
    renders at whatever whole-number fraction of the window keeps it under
    {!Camlcast_core.Config.max_render_height} and stretches the result, so a
    thousand-pixel window is commonly a five-hundred-pixel buffer.
    {!Events.use_viewport} is how a component asks how big the buffer actually
    is. *)

val hud : t list -> t
(** The layer drawn over the finished world. A child of {!world}.

    Its children are drawn in the order they are written, so the last one is on
    top. *)

val rect :
  ?key:string ->
  ?alpha:int ->
  x:int ->
  y:int ->
  w:int ->
  h:int ->
  color:Color.t ->
  unit ->
  t
(** A filled rectangle. [alpha] is out of 255 and defaults to solid. It is
    clamped, so it cannot wrap round to an unintended value however it is
    arrived at: at or below 0 nothing is drawn, at or above 255 the fill is
    solid. *)

val bar :
  ?key:string ->
  x:int ->
  y:int ->
  w:int ->
  h:int ->
  fraction:float ->
  color:Color.t ->
  unit ->
  t
(** A meter [fraction] full, growing rightwards. [fraction] is clamped, so the
    fill cannot overrun its box however the value is arrived at.

    The trough paints one pixel proud on every side, so leave that much room
    around it. *)

val text :
  ?key:string -> ?color:Color.t -> font:Font.t -> x:int -> y:int -> string -> t
(** A run of text with its top-left corner at [(x, y)].

    The engine holds no font, exactly as it holds no colours or pictures, so one
    must be given. {!Camlcast_core.Font.measure} works out what the text will
    take before drawing it, and {!Camlcast_core.Font.wrap} breaks it. *)

val picture : ?key:string -> ?tint:Color.t -> x:int -> y:int -> Image.t -> t
(** A picture with its top-left corner at [(x, y)], multiplied by [tint] if one
    is given. *)

val highlight : ?color:Color.t -> unit -> t
(** Draw a ring round whatever the crosshair is on.

    A sprite is ringed by a rectangle. A picture on a wall is ringed by the
    trapezoid its four corners project to, because a wall recedes. Nothing is
    drawn when the crosshair is on a bare wall, a doorway or nothing at all.
    Nothing is drawn either on a frame the crosshair is not the player's — under
    {!cursor} or a placed {!camera} — which is {!Run.aiming}'s answer, read here
    as well as by the handlers.

    The maths needs the viewport the frame is drawn with, which does not exist
    when a description is written. So this constructor asks for it, and
    {!Aim.ring} does the work. *)

val crosshair : ?color:Color.t -> unit -> t
(** Two short arms at the middle of the buffer — the pixel the straight-ahead
    ray goes through, which is what makes it agree with {!Camlcast_core.Sight}.
*)

val cursor : t
(** Ask for the pointer instead of the camera.

    A child of {!world}. While one of these is in a description the mouse is
    loose and visible, and moving it does not turn the eye. A screen drawn over
    a world wants that, and the world underneath must not also want the motion
    at the same time. Take it out again and the mouse goes back to looking
    around.

    {b Nothing in the world is worked while this is up.} The crosshair is not
    the player's: the mouse is loose, so the crosshair sits wherever the view
    was left, and the runtime stops telling things they are under it. [on_gaze]
    and [on_use] are both silent, so the use control does not work the door
    behind a pause menu, and whatever was lit when this appeared is told it has
    been let go. See {!Run.aiming}. {!Events.aim} still answers, because it is
    read rather than notified.

    Declared rather than called, because everything else here is. *)

val finish : t
(** Say the game is over.

    A child of {!world}. A description that returns this has ended: the frame it
    appears in is drawn, and then the run stops and the window closes.

    Declared rather than called, because everything else here is. A component
    that has reached its ending says so by describing an ending, in the same
    place and the same way it says everything else —
    [if done then P.finish else Element.empty] — instead of reaching for a
    callback the runtime handed it. *)

val link : string * string -> string * string -> t
(** [link (room, threshold) (room', threshold')] makes two doorways the two
    sides of one.

    Each is named by its room and its own name. A child of {!world}, not of
    either room: a link is the one thing in a description that is about two
    rooms at once, and neither of them can hold it. *)
