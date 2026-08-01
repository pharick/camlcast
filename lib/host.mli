(** Turning a committed description into a world.

    This is {!Camlcast_loom.Host.HOST} for the raycaster, and the whole of what
    the runtime knows about walls. It runs once a frame, over the forest of
    {!Prim}s the reconciler has settled on, and calls the same
    {!Camlcast_core.Room.make} and {!Camlcast_core.World.make} a hand-written
    level always did.

    It builds the world from scratch each time. Every step of this rewrite
    called that a real cost and deferred settling it until something measured
    it; [bench/frame.exe] has, and the answer is that there is nothing to
    settle. Describing five rooms and a hundred and forty-five walls — the shape
    and size of the largest world this engine has — takes {b 20 microseconds}
    against the {b 14 milliseconds} the renderer spends drawing that same frame.
    The layer is a seventh of one percent of the work, and caching it would be
    optimising the wrong end by two and a half orders of magnitude.

    If that ever stops being true, every node carries a {!Camlcast_loom.Path.t}
    stable from frame to frame, so caching what a subtree assembled is a change
    to this module alone. The benchmark is there to be re-run before anybody
    makes it. *)

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
