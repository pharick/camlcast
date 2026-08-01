(** Rendering a description into a world, over and over.

    A mount holds the instance tree between frames: the state every component
    has accumulated, the effects it has running, and the identity that lets both
    survive a description being rebuilt from scratch. Made once and rendered
    into every frame.

    Nothing here opens a window. Rendering a description produces a {!Scene.t} —
    a world, and in later steps a camera and an overlay — and what happens to
    that is the loop's business. Which is why every test of this library, and
    every test of a game built on it, can run with no SDL at all. *)

type t
(** A mounted description and everything it remembers. *)

val create : unit -> t
(** An empty mount. The first {!render} into it builds everything. *)

val render :
  ?trace:(Prim.t Camlcast_loom.Trace.event -> unit) -> t -> Parts.t -> Scene.t
(** Reconcile a description against what this mount holds, assemble the result,
    and run whatever effects that leaves owing.

    @raise Host.Malformed if the description could not be a world.
    @raise Invalid_argument from the engine's own constructors. *)

val dirty : t -> bool
(** Whether a setter or a store has asked for a frame since the last {!render}.
*)

val build : Parts.t -> Scene.t
(** One description, one world, nothing kept. What a test asserting on geometry
    wants, and what nothing that has to run twice should use. *)
