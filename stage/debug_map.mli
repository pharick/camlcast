(** The room from above, with what is wrong with it marked.

    Two of this engine's mistakes are invisible from inside and obvious from
    over the top. A boundary wound the wrong way round draws a room black, and
    the black tells you nothing about which wall or which way; here every wall
    carries a short tick pointing the way its normal faces, and a reversed loop
    is a row of ticks pointing outwards. A doorway that leads nowhere draws as
    haze, which is also what distance draws as; here it is red where a linked
    one is green.

    Everything it needs was already public — {!Camlcast_core.Room.wall_at},
    {!Camlcast_core.Paint.line} — and [doc/building-the-engine.mld] has listed a
    minimap as an exercise since before there was anything to debug with one.

    {1 One room}

    The room the player is standing in, in that room's own coordinates, and no
    other. A world has no shared frame to draw two rooms in — that is the whole
    point of {!Camlcast_core.Transform} — so a map of several would have to pick
    one room's frame and carry the rest into it through their portals. Worth
    doing, and not needed to see a wall pointing the wrong way. *)

open Camlcast_core

val panel : Framebuffer.t -> int * int * int * int
(** Where the map goes on a buffer of this size: [(x, y, side, side)].

    Exposed so a test can assert that nothing was drawn outside it, and so a
    game that wants the map somewhere else knows what it is moving. *)

val draw : Framebuffer.t -> World.t -> Player.t -> Check.t list -> unit
(** Draw the map over a finished frame.

    The diagnostics are {!Check.world}'s. Those with a {!Check.spot} in the room
    being drawn are marked where they are, in red for an error and amber for a
    warning; the rest are not shown at all, since a map is no place to read a
    sentence and there is nowhere on it to put one that is about no particular
    place. {!Check.format} is where those are meant to be read. *)
