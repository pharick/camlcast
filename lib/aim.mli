(** Maps crosshair hits to the handlers registered for them.

    {!Camlcast_core.Sight} casts from the screen centre, through doorways, to
    the same depth the frame was drawn — both read
    {!Camlcast_core.Config.max_portal_depth}. It stops on exactly what stops the
    eye, per-texel alpha included, and reports the room and the index of the
    wall, sprite or doorway hit. It cannot say which part of the game
    description produced that wall, because by then a wall is an array index.

    This module supplies that mapping. {!Host.assemble} stores, beside every
    room it builds, the handlers the description attached to each thing, plus a
    {!Camlcast_loom.Path.t} for each. The path lets gaze identity survive the
    world being rebuilt from scratch every frame.

    Paths make enter and leave events possible: indices move when a room is
    rebuilt, paths do not. A description that rearranges its unkeyed children
    changes what a path {e means}; see {!leaving}. *)

type where =
  | On_wall of {
      along : float;  (** distance along the wall from its starting end *)
      z : float;  (** height above the floor under that point *)
      facing : Camlcast_core.Room.side;  (** which face is being looked at *)
      decal : int option;
          (** index of the decal hit, if the eye stopped on one *)
    }
  | On_sprite
  | On_doorway
      (** Which of the three things the eye stopped on, and where on it.

          Only a wall carries a position. A sprite always faces the viewer and a
          doorway is a hole, so neither has a surface coordinate a game could
          use. *)

type spot = {
  distance : float;  (** how far away, in cells *)
  crossed : int;  (** how many doorways the ray went through to get there *)
  where : where;
}
(** Where the crosshair landed, for handlers that need more than the fact of a
    hit. Marking a wall uses [along] and [z]; a prompt shown only within reach
    uses [distance]. *)

type reaction = {
  path : Camlcast_loom.Path.t;
      (** which part of the description this is. Stable across a rebuild; stable
          across a rearrangement of its siblings only if it was given a key —
          see {!leaving} *)
  on_gaze : (bool -> unit) option;
      (** called with [true] when the crosshair arrives and [false] when it
          leaves; not called every frame in between *)
  on_use : (spot -> unit) option;
      (** called when the player works the use control while looking at it, with
          the crosshair's position on it *)
}
(** The handlers one thing in a world registered. *)

type t
(** Every reaction in a world, indexed for lookup from a
    {!Camlcast_core.Sight.t}. *)

val of_rooms :
  (reaction option array * reaction option array * reaction option array) list ->
  t
(** Builds a {!t} room by room, in world order. Each room supplies the reactions
    of its walls, sprites and doorways, in arrays parallel to those
    {!Camlcast_core.Room.wall_at} and its neighbours index into. *)

val find : t -> Camlcast_core.Sight.t -> reaction option
(** Returns the reaction of the thing the crosshair landed on, if it has one.

    Returns [None] rather than raising when the world has grown since this was
    built: a stale index is a frame out of date, not an error, and the next
    frame carries the right one. *)

val spot_of : Camlcast_core.Sight.t -> spot
(** Converts a sight to a {!spot}, dropping the indices. The indices name places
    in a world rebuilt every frame, so a handler has no use for them. *)

val crosshair :
  t ->
  sight:Camlcast_core.Sight.t option ->
  was:Camlcast_loom.Path.t option ->
  used:bool ->
  Camlcast_loom.Path.t option
(** Notifies the current crosshair target and returns its path.

    This is everything an interacting frame does, in one function of values, so
    the loop has no logic of its own that could only be tested through a window.
    [sight] is the cast: {!Camlcast_core.Sight.look} of the world and player
    this frame settled on. [was] is what the crosshair was on last frame. [used]
    is whether the player worked the use control. The result is the value to
    pass as [was] next frame.

    The cast is a parameter rather than computed here because a frame needs it
    twice: once to tell what was hit and once as the value a description reads
    through {!Events.aim}. Two separate casts could disagree.

    The thing losing the crosshair is notified before the thing gaining it, so
    two things swapping a highlight are never both lit. *)

val ring :
  Camlcast_core.World.t ->
  Camlcast_core.Player.t ->
  width:int ->
  height:int ->
  (float * float) list option
(** Returns the corners of the crosshair target projected onto a buffer of this
    size, or [None] if the target is nothing worth ringing.

    This is in the library because it needs the {!Camlcast_core.Viewport} the
    frame was drawn with, and a description is written before there is a frame.
    {!P.highlight} is how a description asks for it.

    A {b sprite} is square to the view, so its box is a rectangle. A {b decal}
    is flat on a wall, which recedes — the far edge of a picture on it is
    smaller than the near one — so its ring is the trapezoid through its four
    projected corners. Both take the pose from the sighting rather than from the
    player, so a thing in an adjacent room is placed in {e that} room's
    coordinates and still lands where it was drawn. *)

val leaving : t -> Camlcast_loom.Path.t -> (bool -> unit) option
(** Returns the [on_gaze] at this path, for telling last frame's target that the
    crosshair has gone.

    Lookup is by path and not by index, because the world it was found in has
    been rebuilt since and the indices have moved. Returns [None] if that part
    of the description no longer exists; a vanished target has nothing left to
    notify.

    A path is as stable as the description makes it. This is the one place in
    the runtime that relies on a path {e across} a frame; everything else here
    is answered from the cast this frame made against the world this frame
    built. {!Camlcast_loom.Path.equal} identifies a keyed child by its key and
    an unkeyed one by its position, so a description that rearranges unkeyed
    siblings between frames sends the leave to whichever child now stands where
    last frame's target stood. That is the specified identity model, not a fault
    to work around here: a runtime guessing which unkeyed wall was "really" the
    old one would be wrong in the cases keys exist to settle. This is why {!P}
    tells a game to key anything it rearranges. *)
