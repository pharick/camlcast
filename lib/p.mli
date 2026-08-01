(** The parts a world is written from.

    This is the vocabulary. A game builds descriptions out of these, wraps them
    in components of its own, and never mentions a {!Camlcast_core.World.t}, a
    framebuffer or a renderer again.

    Every constructor here takes and returns plain values — a description is
    data — so a component that returns one is an ordinary function, and a
    higher-order component that takes children and puts something around them is
    an ordinary function too. There is no machinery to learn beyond the list
    below.

    {1 Winding, and why it is not your problem}

    A room is dark from the inside if its boundary is wound the wrong way round.
    That has been the one silent trap in this engine: nothing catches it,
    nothing wants the reversed version, and the symptom — a black room — points
    nowhere near the cause.

    {!outline} settles it by not asking. It measures the loop it was given and
    winds it correctly, so the same corners in either order build the same room,
    and the mistake stops being possible rather than being diagnosed. Write a
    boundary with {!outline} and the question never arises.

    A free-standing {!wall} is a different matter and needs no rule: it is drawn
    from both sides and has no inside for a normal to face into. *)

open Camlcast_core

type t = Prim.t Camlcast_loom.Element.t
(** A description of part of a world — what a component returns. *)

(** {1 Being looked at}

    {!wall}, {!sprite} and {!doorway} each take [on_gaze] and [on_use], because
    those are the three things an eye can stop on — {!Camlcast_core.Sight} says
    so, and it says so by casting the same ray the renderer draws with, so what
    can be picked is exactly what can be seen.

    [on_gaze] is called with [true] when the crosshair arrives and [false] when
    it leaves, and not once a frame in between: it is an enter and a leave, not
    a poll. [on_use] is called when the player works the use control while
    looking at it, with an {!Aim.spot} saying where on it the crosshair was —
    which is what marking a wall where you pointed needs. Both may set state,
    and the frame after will show it.

    On a {!doorway} they go on the opening rather than on the jambs either side
    of it, because what a player aims at to work a door is the door. *)

(** {1 What is under a room and over it} *)

val floor : plane:Plane.t -> material:Material.t -> Room.surface
(** A floor: an inclined plane, and what it is made of. *)

val roof : plane:Plane.t -> material:Material.t -> Room.ceiling
(** A ceiling of the same. *)

val open_sky : Sky.t -> Room.ceiling
(** Nothing overhead, and the sky that shows instead. *)

(** {1 The world} *)

val world : atmosphere:Atmosphere.t -> spawn:string * Vec.t -> t list -> t
(** The root of every description: the air its rooms are seen through, and the
    room and spot the player starts in.

    Its children are {!room}s and {!link}s. There is exactly one of these in a
    description, and it is the outermost thing in it. *)

val room :
  ?key:string ->
  name:string ->
  floor:Room.surface ->
  ceiling:Room.ceiling ->
  t list ->
  t
(** A room in a coordinate frame of its own, holding its boundary, the sprites
    standing in it and the doorways cut through it.

    [name] is how a {!link} finds it, so it has to be unique within the world.
    Use {!Camlcast_core.Room.floor} for the surface and
    {!Camlcast_core.Room.roof} or {!Camlcast_core.Room.open_sky} for what is
    overhead. *)

val outline : height:float -> material:Material.t -> Vec.t list -> t
(** A closed boundary through these corners, wound so the room is on the inside.

    The corners in either order describe the same room; see the note at the top
    of this page. Three at least, and no two the same in a row. *)

val path : height:float -> material:Material.t -> Vec.t list -> t
(** An open run of wall through these corners: what a boundary is when part of
    it is a {!doorway} rather than a wall.

    Wound like {!outline}, by measuring the loop it would be if its two ends
    were joined. A room's boundary with a gap in it is exactly that loop, so the
    measurement is the same one and a game gets the same freedom not to think
    about it. A run that doubles back on itself, or one whose corners are all in
    a line, encloses nothing to measure and is left in the order it was given.

    Two corners at least. *)

val polygon :
  center:Vec.t ->
  radius:float ->
  sides:int ->
  rotation:float ->
  height:float ->
  material:Material.t ->
  t
(** A regular polygon of [sides] walls, wound so its inside is inside.

    A pillar, most of the time. [radius] is to a corner and not to a face, and
    [rotation] turns the whole thing about its centre. *)

val opening : width:float -> Vec.t -> Vec.t -> Vec.t * Vec.t
(** The two ends of the opening {!doorway} would cut into the wall from [a] to
    [b].

    {!doorway} works them out for itself, which is the point of it — but a
    description that wants to say "this room's floor is that room's floor,
    carried through the doorway between them" has to name the doorway in terms
    of both sides, and this is how it gets them without doing the arithmetic
    twice. Feed the pair to {!through}. *)

val through : from:Vec.t * Vec.t -> into:Vec.t * Vec.t -> Plane.t -> Plane.t
(** A plane carried through a doorway: [from] and [into] are the same opening's
    two ends as each of its rooms writes them, and the answer is the plane in
    the second room's frame.

    This is how a floor meets itself across a threshold. Two rooms have no
    coordinates in common — that is the whole point of a {!Camlcast_core.World}
    — so a second floor written by hand to look right is a second floor that
    will drift. Derived, it cannot: {!Check} reports a step in the floor at a
    doorway, and a plane carried through one never has one. *)

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

    {!doorway} is what to reach for: it cuts the opening out of a wall and hands
    back the jambs with it, so the two cannot drift apart. This is for the case
    that will not do — a lintel of a different material from the wall under it,
    or a boundary whose jambs are already drawn some other way. Whoever uses it
    owns making the walls either side meet its ends. *)

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

    For a room's boundary reach for {!outline} instead. This is for what stands
    on its own — a partition, a bench you see over, a monolith. *)

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

    [facing] is the inside of the room unless said otherwise, and [glow] is how
    much light it makes of its own, which is none. *)

val doorway :
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
    cannot drift apart, which is the whole reason to cut a doorway rather than
    place one. *)

val sprite :
  ?key:string ->
  ?on_gaze:(bool -> unit) ->
  ?on_use:(Aim.spot -> unit) ->
  ?base:float ->
  size:float ->
  image:Image.t ->
  Vec.t ->
  t
(** A billboard standing at a point, [size] cells tall, turning to face the
    player. [base] floats it above the floor; without it, it stands on it.

    Key anything that can be rearranged. A list of sprites that sorts itself is
    exactly the case keys exist for. *)

val camera :
  ?pitch:float -> room:string -> pos:Vec.t -> angle:float -> unit -> t
(** Put the eye here, instead of letting the runtime walk it.

    A child of {!world}. While one of these is in a description the controls do
    not move the player at all — the description is saying where the eye is,
    every frame, and a walk it did not ask for would fight it. Take it out again
    and the player carries on from wherever the description last put it.

    [angle] is in radians and [pitch] is the fraction of the window height the
    horizon is shifted by, the same as the mouse gives. *)

(** {1 The layer over the top}

    Everything below draws on the finished frame, in the order it is written, in
    the framebuffer's own pixels — which are not the window's. The engine
    renders at whatever whole-number fraction of the window keeps it under
    {!Camlcast_core.Config.max_render_height}, and stretches the result, so a
    thousand-pixel window is commonly a five-hundred-pixel buffer.
    {!Events.use_viewport} is how a component asks how big it actually is. *)

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
(** A filled rectangle. [alpha] is out of 255 and solid unless said otherwise.
*)

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
(** A meter [fraction] full, growing rightwards. Clamped, so it cannot overrun
    its box however it is arrived at.

    It paints one pixel proud on every side — the trough — so leave that much
    room around it. *)

val text :
  ?key:string -> ?color:Color.t -> font:Font.t -> x:int -> y:int -> string -> t
(** A run of text with its top-left corner at [(x, y)].

    The engine holds no font, exactly as it holds no colours or pictures, so one
    has to be given. {!Camlcast_core.Font.measure} is how to work out what it
    will take before drawing it, and {!Camlcast_core.Font.wrap} is how to break
    it. *)

val picture : ?key:string -> ?tint:Color.t -> x:int -> y:int -> Image.t -> t
(** A picture with its top-left corner at [(x, y)], multiplied by [tint] if one
    is given. *)

val crosshair : ?color:Color.t -> unit -> t
(** Two short arms at the middle of the buffer — the pixel the straight-ahead
    ray goes through, which is what makes it agree with {!Camlcast_core.Sight}.
*)

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
    either room: it is the one thing in a description that is about two rooms at
    once, and neither of them can hold it. *)
