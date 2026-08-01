(** The parts a world is written from.

    This is the vocabulary. A game builds descriptions out of these, wraps them
    in components of its own, and never mentions a {!Camlcast.World.t}, a
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

open Camlcast

type t = Prim.t Camlcast_loom.Element.t
(** A description of part of a world — what a component returns. *)

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
    Use {!Camlcast.Room.floor} for the surface and {!Camlcast.Room.roof} or
    {!Camlcast.Room.open_sky} for what is overhead. *)

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

val wall :
  ?key:string ->
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
  ?key:string -> ?base:float -> size:float -> image:Image.t -> Vec.t -> t
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

val finish : t
(** Say the game is over.

    A child of {!world}. A description that returns this has ended: the frame it
    appears in is drawn, and then the run stops and the window closes.

    Declared rather than called, because everything else here is. A component
    that has reached its ending says so by describing an ending, in the same
    place and the same way it says everything else —
    [if done then Parts.finish else Element.empty] — instead of reaching for a
    callback the runtime handed it. *)

val link : string * string -> string * string -> t
(** [link (room, threshold) (room', threshold')] makes two doorways the two
    sides of one.

    Each is named by its room and its own name. A child of {!world}, not of
    either room: it is the one thing in a description that is about two rooms at
    once, and neither of them can hold it. *)
