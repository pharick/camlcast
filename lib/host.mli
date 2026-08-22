(** Turns a committed description into a world.

    This is {!Camlcast_loom.Host.HOST} for the raycaster, and the whole of what
    the runtime knows about walls. It runs once a frame, over the forest of
    {!Prim}s the reconciler has settled on. It calls the same
    {!Camlcast_core.Room.make} and {!Camlcast_core.World.make} a hand-written
    level always did.

    It builds the world from scratch each time, and does not cache what it
    assembles, because [bench/frame.exe] says there is nothing worth caching.
    Describing five rooms and a hundred and forty-five walls — the shape and
    size of the largest world this engine has — takes {b 20 microseconds}; the
    renderer spends {b 14 milliseconds} drawing that same frame. The layer is a
    seventh of one percent of the work, so caching it would optimise the wrong
    end by two and a half orders of magnitude.

    If that ever stops being true, every node carries a {!Camlcast_loom.Path.t}
    stable from frame to frame, so caching what a subtree assembled is a change
    to this module alone. Re-run the benchmark before making that change. *)

type prim = Prim.t
type scene = Scene.t

exception Malformed of string
(** A description that cannot be a world: no root, two roots, a wall loose at
    the top level, a room inside a room.

    The exception is temporary. The next step replaces it with diagnostics that
    name the component the offending part was written in; a bare message cannot
    do that, and that is the reason to replace the exception rather than keep
    it. *)

val assemble : prim Camlcast_loom.Host.node list -> scene
(** Build the world this description describes.

    @raise Malformed if the description is not one that could be a world.
    @raise Invalid_argument
      from the engine's own constructors, which check what they always checked:
      a wall of no length, a doorway wider than its wall, two thresholds linked
      to the same one. *)
