(** Renders a description into a world, over and over.

    A mount holds the instance tree between frames: the state every component
    has accumulated, the effects it has running, and the identity that lets both
    survive a description being rebuilt from scratch. Make one once and render
    into it every frame.

    Nothing here opens a window. Rendering a description produces a {!Scene.t} —
    a world, where the eye is, and what is drawn over the top — and what happens
    to that is the loop's business. This is why every test of this library, and
    every test of a game built on it, can run with no SDL at all. *)

type t
(** A mounted description and everything it remembers. *)

val create : unit -> t
(** An empty mount. The first {!render} into it builds everything. *)

val render :
  ?trace:(Prim.t Camlcast_loom.Trace.event -> unit) ->
  ?inspect:(Camlcast_loom.Path.t -> Camlcast_loom.Hook.slot array -> unit) ->
  ?patch:
    (Prim.t Camlcast_loom.Host.node list -> Prim.t Camlcast_loom.Host.node list) ->
  t ->
  P.t ->
  Scene.t
(** Reconcile a description against what this mount holds, assemble the result,
    and run whatever effects that leaves owing.

    [trace] reports what the reconciler did, [inspect] what each component is
    holding as it renders, and [patch] is handed the committed forest and gives
    back the one to assemble. See {!Camlcast_loom.Reconcile.Make.render} for all
    three, and for why a patch is a transform of the whole forest rather than of
    one node.

    A patch is how a tool outside edits a world that is running: it changes what
    a frame assembles without changing the description that frame came from, so
    a dragged wall moves in the next frame whichever component wrote it. Nothing
    in this library installs one.

    @raise Host.Malformed if the description could not be a world.
    @raise Invalid_argument from the engine's own constructors. *)

val dirty : t -> bool
(** Whether a setter or a store has asked for a frame since the last {!render}.

    {!Run.on} never asks: a game draws every frame, so it renders every frame.
    This exists for the loop that would rather not — a menu, an editor, a tool
    that redraws when something changed and idles otherwise. Such a loop relies
    on the answer in a way a loop that renders anyway does not, so what it can
    rely on is stated here.

    Only a component still in the description can ask for a frame. A setter kept
    past its component's life — in a timer, or in a subscription that was slow
    to be dropped — does nothing. After {!destroy} the same holds for every
    setter the description ever handed out: a torn-down mount stays clean, and a
    loop polling one is not told to rebuild what it has just let go of. *)

val destroy : ?trace:(Prim.t Camlcast_loom.Trace.event -> unit) -> t -> unit
(** Unmount everything this mount holds and run every cleanup it owes.

    This is what a run does with its mount when the window closes. A description
    whose components subscribed to something, opened something or armed
    something on the way in lets go of all of it here. One whose effects held
    nothing spends a walk of the tree and no more.

    Every cleanup owed runs even if one of them raises, so a window that closes
    on a broken effect still releases the rest. The first exception raised is
    the one that comes out. *)

val build : P.t -> Scene.t
(** One description, one world, nothing kept. Use it in a test asserting on
    geometry; do not use it for anything that has to run twice.

    Nothing kept includes the effects. They are started, because a description
    is not finished being read until they have been, and then stopped again
    before this returns.

    @raise Host.Malformed if the description could not be a world.
    @raise Invalid_argument from the engine's own constructors.
    @raise Fun.Finally_raised
      if a cleanup raises on the way out, which replaces whatever the render was
      raising or returning. *)
