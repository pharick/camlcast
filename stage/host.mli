(** Turning a committed description into a world.

    This is {!Camlcast_loom.Host.HOST} for the raycaster, and the whole of what
    the runtime knows about walls. It runs once a frame, over the forest of
    {!Prim}s the reconciler has settled on, and calls the same
    {!Camlcast_core.Room.make} and {!Camlcast_core.World.make} a hand-written
    level always did.

    It builds the world from scratch each time. That is a real cost and a
    deliberately deferred one: every node carries a {!Camlcast_loom.Path.t} that
    is stable from frame to frame, so caching what a subtree assembled is a
    change to this module alone, to be made when a benchmark asks for it. *)

type prim = Prim.t
type scene = Scene.t

exception Malformed of string
(** A description that cannot be a world: no root, two roots, a wall loose at
    the top level, a room inside a room.

    A blunt instrument, and temporary. The next step replaces it with
    diagnostics that name the component the offending part was written in, which
    is the thing a bare message cannot do and the reason the exception is worth
    replacing rather than keeping. *)

val assemble : prim Camlcast_loom.Host.node list -> scene
(** Build the world this description describes.

    @raise Malformed if the description is not one that could be a world.
    @raise Invalid_argument
      from the engine's own constructors, which check what they always checked:
      a wall of no length, a doorway wider than its wall, two thresholds linked
      to the same one. *)
