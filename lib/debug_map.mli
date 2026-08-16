(** The room from above, with what is wrong with it marked.

    Two of this engine's mistakes are invisible from inside and obvious from
    over the top:
    - A boundary wound the wrong way round draws a room black, and the black
      says nothing about which wall or which way. On the map every wall carries
      a short tick pointing the way its normal faces, so a reversed loop is a
      row of ticks pointing outwards.
    - A doorway that leads nowhere draws as haze, which is also what distance
      draws as. On the map it is red where a linked one is green.

    Everything the map needs was already public — {!Camlcast_core.Room.wall_at},
    {!Camlcast_core.Paint.line} — and [doc/building-the-engine.mld] has listed a
    minimap as an exercise since before there was anything to debug with one.

    {1 One room}

    The map shows the room the player is standing in, in that room's own
    coordinates, and no other. A world has no shared frame to draw two rooms in;
    {!Camlcast_core.Transform} exists because of that. A map of several rooms
    would have to pick one room's frame and carry the rest into it through their
    portals. That is worth doing, and not needed to see a wall pointing the
    wrong way. *)

open Camlcast_core

val panel : Framebuffer.t -> int * int * int * int
(** Where the map goes on a buffer of this size: [(x, y, side, side)].

    Exposed so a test can assert that nothing was drawn outside it, and so a
    game that wants the map somewhere else knows what it is moving. *)

val draw : Framebuffer.t -> World.t -> Player.t -> Check.t list -> unit
(** Draw the map over a finished frame.

    The diagnostics are {!Check.assembled}'s. Those with a {!Check.spot} in the
    room being drawn are marked where they are: red for an error, amber for a
    warning. The rest are not shown at all, because a map is no place to read a
    sentence and a diagnostic about no particular place has nowhere on it to go.
    Read those with {!Check.format}. *)
