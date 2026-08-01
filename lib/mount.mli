(** Rendering a description into a world, over and over.

    A mount holds the instance tree between frames: the state every component
    has accumulated, the effects it has running, and the identity that lets both
    survive a description being rebuilt from scratch. Made once and rendered
    into every frame.

    Nothing here opens a window. Rendering a description produces a {!Scene.t} —
    a world, where the eye is, and what is drawn over the top — and what happens
    to that is the loop's business. Which is why every test of this library, and
    every test of a game built on it, can run with no SDL at all. *)

type t
(** A mounted description and everything it remembers. *)

val create : unit -> t
(** An empty mount. The first {!render} into it builds everything. *)

val render :
  ?trace:(Prim.t Camlcast_loom.Trace.event -> unit) -> t -> P.t -> Scene.t
(** Reconcile a description against what this mount holds, assemble the result,
    and run whatever effects that leaves owing.

    @raise Host.Malformed if the description could not be a world.
    @raise Invalid_argument from the engine's own constructors. *)

val dirty : t -> bool
(** Whether a setter or a store has asked for a frame since the last {!render}.
*)

val destroy : ?trace:(Prim.t Camlcast_loom.Trace.event -> unit) -> t -> unit
(** Unmount everything this mount holds and run every cleanup it owes.

    What a run does with its mount when the window closes. A description whose
    components subscribed to something, opened something or armed something on
    the way in lets go of all of it here; one whose effects held nothing spends
    a walk of the tree and no more.

    Every cleanup owed runs even if one of them raises, so a window that closes
    on a broken effect still closes on the rest of them being given back. The
    first exception is what comes out. *)

val build : P.t -> Scene.t
(** One description, one world, nothing kept. What a test asserting on geometry
    wants, and what nothing that has to run twice should use.

    Nothing kept includes the effects: they are started, because a description
    is not finished being read until they have been, and then stopped again
    before this returns.

    @raise Host.Malformed if the description could not be a world.
    @raise Invalid_argument from the engine's own constructors.
    @raise Fun.Finally_raised
      if a cleanup raises on the way out, which replaces whatever the render was
      raising or returning. *)
