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

type reaction = {
  path : Camlcast_loom.Path.t;
      (** which part of the description this is, stable across frames *)
  on_gaze : (bool -> unit) option;
      (** called with [true] when the crosshair arrives and [false] when it
          leaves, and not once a frame in between *)
  on_use : (unit -> unit) option;
      (** called when the player works the use control while looking at it *)
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

val leaving : t -> Camlcast_loom.Path.t -> (bool -> unit) option
(** The [on_gaze] of whatever sits at this path, for telling last frame's target
    that the crosshair has gone.

    By path and not by index, because the world it was found in has been rebuilt
    since and the indices have moved. [None] if that part of the description is
    no longer there at all — which is a thing that was being looked at and then
    ceased to exist, and has nothing left to tell. *)
