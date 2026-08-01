(** What the crosshair is on, and who asked to be told.

    {!Camlcast_core.Sight} already answers the hard half: it casts the middle of
    the screen through up to three doorways, stops on exactly what stops the eye
    — per-texel alpha included — and reports the room and the index of the wall,
    sprite or doorway it landed on. What it cannot do is say which part of a
    game wrote that wall, because by then a wall is a number in an array.

    This is the other half. {!Host.assemble} keeps, beside every room it builds,
    the handlers the description hung on each thing in it, and a
    {!Camlcast_loom.Path.t} for each so that being looked at survives the world
    being rebuilt from scratch every frame.

    That path is what makes an enter and a leave possible at all. Indices move
    when a room is rebuilt; a path does not. *)

type where =
  | On_wall of {
      along : float;  (** how far along the wall, from the end it starts at *)
      z : float;  (** how far up it, above the floor under that point *)
      facing : Camlcast_core.Room.side;  (** which face is being looked at *)
      decal : int option;  (** which picture on it, if the eye stopped on one *)
    }
  | On_sprite
  | On_doorway
      (** Which of the three things the eye stopped on, and where on it.

          Only a wall has a where: a sprite is a picture that turns to face you
          and a doorway is a hole, and neither has a coordinate a game could
          usefully be told. *)

type spot = {
  distance : float;  (** how far away, in cells *)
  crossed : int;  (** how many doorways the ray went through to get there *)
  where : where;
}
(** Where the crosshair landed, for a handler that needs to know more than that
    it did. Marking a wall wants [along] and [z]; a prompt that only appears
    within reach wants [distance]. *)

type reaction = {
  path : Camlcast_loom.Path.t;
      (** which part of the description this is, stable across frames *)
  on_gaze : (bool -> unit) option;
      (** called with [true] when the crosshair arrives and [false] when it
          leaves, and not once a frame in between *)
  on_use : (spot -> unit) option;
      (** called when the player works the use control while looking at it, with
          where on it the crosshair was *)
}
(** What one thing in a world asked to be told. *)

type t
(** Every reaction in a world, laid out to be found from a
    {!Camlcast_core.Sight.t}. *)

val none : t
(** A world that asked for nothing. *)

val of_rooms :
  (reaction option array * reaction option array * reaction option array) list ->
  t
(** Room by room, in world order: the reactions of its walls, its sprites and
    its doorways, each running parallel to the arrays
    {!Camlcast_core.Room.wall_at} and its neighbours index into. *)

val find : t -> Camlcast_core.Sight.t -> reaction option
(** What the crosshair landed on asked for, if it asked for anything.

    [None] for a world grown since this was built, rather than an exception: a
    stale index is a frame out of date and not a mistake, and the next frame
    will have the right one. *)

val spot_of : Camlcast_core.Sight.t -> spot
(** What a cast of the crosshair comes to, without the indices — those name
    places in a world that is rebuilt every frame, and a handler has no use for
    them. *)

val crosshair :
  t ->
  Camlcast_core.World.t ->
  Camlcast_core.Player.t ->
  was:Camlcast_loom.Path.t option ->
  used:bool ->
  Camlcast_loom.Path.t option
(** Cast the middle of the screen, tell whatever it lands on, and answer with
    what that was.

    Everything an interacting frame does, in one function of values, so that the
    loop has no logic of its own to be tested through a window. [was] is what
    the crosshair was on last frame, [used] whether the player worked the use
    control, and the answer is what to pass as [was] next time.

    Whatever is losing the crosshair hears so before whatever is gaining it, so
    two things swapping a highlight are never both lit. *)

val ring :
  Camlcast_core.World.t ->
  Camlcast_core.Player.t ->
  width:int ->
  height:int ->
  (float * float) list option
(** The corners of whatever the crosshair is on, projected onto a buffer this
    size, or [None] if it is on nothing worth ringing.

    This is in the library because it cannot be anywhere else: it needs the
    {!Camlcast_core.Viewport} the frame was drawn with, and a description is
    written before there is a frame. {!P.highlight} is how a description asks
    for it.

    A {b sprite} is square to the view, so its box is a rectangle. A {b decal}
    is flat on a wall, which recedes — the far edge of a picture on it is
    smaller than the near one — so its ring is the trapezoid through its four
    projected corners. Both take the pose from the sighting rather than from the
    player, so a thing in the room next door is placed in {e that} room's
    coordinates and still lands where it was drawn. *)

val leaving : t -> Camlcast_loom.Path.t -> (bool -> unit) option
(** The [on_gaze] of whatever sits at this path, for telling last frame's target
    that the crosshair has gone.

    By path and not by index, because the world it was found in has been rebuilt
    since and the indices have moved. [None] if that part of the description is
    no longer there at all — which is a thing that was being looked at and then
    ceased to exist, and has nothing left to tell. *)
