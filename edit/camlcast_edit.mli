(** Looking at a running game, and editing the world it is standing in.

    {1 What this is}

    A game describes its world every frame and the runtime works out what
    changed. That is a closed loop, and two things about it are invisible from
    inside: what the tree is actually doing, and which line of which file
    describes the wall in front of the player. This library is the way to see
    both.

    Nothing here is part of a game. A release build does not link it, and the
    game names it only where it opens the overlay — see {!Camlcast.Watch} for
    the three openings a run offers and this library uses.

    {1 What it does not hold}

    No font, no colour, no picture. The engine holds none either, and an overlay
    drawn over a game should look like it belongs to that game rather than to
    this library. Every panel is handed what it draws with. *)

module Session = Session
(** The overlay itself: what it is showing, what is picked, and the one call a
    game makes to turn it on. Start here. *)

module Panel = Panel
(** The box both views are drawn in, and where the room a HUD actually has is
    worked out. Read it before assuming a maximised window means a large
    overlay. *)

module Span = Span
(** Where an expression sits in a file, and how to change those bytes without
    disturbing the file around them. *)

module Field = Field
(** The parts of a call an editor can change — a number to type over, a name to
    swap. One rule for a sprite's size, a floor's plane and the air a world is
    seen through. *)

module Graph = Graph
(** The world as a graph: rooms as nodes, doorways as stubs, connections as
    edges. The only honest picture of what is between rooms, there being no
    frame to draw two of them in. *)

module Link = Link
(** From a thing in the world to the line that describes it, and to what about
    that line can be changed. *)

module Patch = Patch
(** Edits held against a running world, so a drag shows before the file it
    changes has been rebuilt. *)

module Tree = Tree

module Revise = Revise
(** Changing a file on disk, and taking the change back. The one thing in this
    engine that writes. *)

module Scaffold = Scaffold
(** Writing a new component, and moving part of one into a new file — with the
    two things about [Element.declare] that are correctness rules held by
    construction. *)

module Sheet = Sheet
(** A room from above, drawn from the committed frame so that everything in it
    knows where it sits in the tree and where it was written. *)

module Slots = Slots
