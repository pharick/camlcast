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

val openings :
  prim Camlcast_loom.Host.node ->
  (int * Camlcast_core.Room.threshold * prim Camlcast_loom.Host.node) list
(** Every door this room cuts, as the threshold it will become: its identity,
    the threshold, and the node it was written at.

    Exported for {!Check}, which asks it of a description before any of it is
    built. The two sides of an opening have to agree about their width and their
    height, and saying so before assembly is what lets the complaint name the
    component that wrote it rather than come back out of the engine as a raised
    string. {!assemble} computes the same thresholds the same way, so the two
    cannot answer differently.

    @raise Malformed
      on a door too tall for the room it is cut into, or wider than the leg it
      names — the same two refusals assembly makes, because it is the same code.
*)

val assemble : prim Camlcast_loom.Host.node list -> scene
(** Build the world this description describes.

    @raise Malformed if the description is not one that could be a world.
    @raise Invalid_argument
      from the engine's own constructors, which check what they always checked:
      a wall of no length, a doorway wider than its wall, two thresholds linked
      to the same one. *)
