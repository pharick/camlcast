(** Matching this frame's description against last frame's tree.

    A game hands over a whole {!Element.t} every frame. Somewhere behind it is a
    tree of instances that has been alive since the frame each of its parts
    first appeared, holding everything that has to outlive a description. This
    is what puts the two together: it walks them in step, decides at each place
    whether the thing described is the thing already there, and keeps or
    replaces accordingly.

    {1 What counts as the same thing}

    - A {b component} is the same when its [render] is physically the same
      closure and its key agrees. See {!Element} for why that is the only
      sensible test and what it asks of a game in return.
    - A {b primitive} is the same when its key agrees — {e and nothing more.}
      The runtime cannot tell a wall from a sprite, because [prim] is the host's
      own type and opaque here. So a wall replaced by a sprite at the same place
      is an update rather than a teardown, and any components underneath keep
      their state. Nothing is corrupted by this: primitives are inert
      descriptions with no state of their own and nothing to release. It is
      written down because it differs from React, where a [div] becoming a
      [span] tears the subtree down, and the difference is a deliberate
      consequence of keeping {!Host.HOST} to one function.
    - {!Element.Empty} and {!Element.Fragment} match themselves.

    Anything else is a replacement: the old subtree is unmounted deepest-first
    and the new one mounted in its place.

    {1 Children}

    Among one parent's children, a keyed element is matched to last frame's by
    its key wherever either of them has moved to, and an unkeyed one is matched
    against whatever stood at its own index — and only if that was unkeyed too.
    Position is the whole of an unkeyed child's identity, which is exactly what
    {!Path} says; the two have to agree, or a child could keep its state across
    a move that changed the name everything else knows it by.

    {b One parent's keys are unique}, and a repeat raises
    {!Element.Duplicate_key}. See {!Element} for why an ambiguous path is not a
    question worth answering.

    A fragment is matched by key like anything else, which is what lets a helper
    that returns several primitives at once — with no single one of them to hang
    a key on — be rearranged with everything under it intact.

    Leftovers are unmounted in the order they were declared, so that a trace of
    a frame reads the same way twice. *)

module Make (H : Host.HOST) : sig
  type element = H.prim Element.t
  (** {!Element.t} at this host's primitive, which is the type a game writes. *)

  type t
  (** A mounted root: the instance tree, and everything hanging off it.

      Made once and rendered into repeatedly. Two roots share nothing, which is
      what lets a test drive several in one process — and, later, what lets a
      launcher hold a menu and a world at the same time. *)

  val create : unit -> t
  (** An empty root. The first {!render} into it mounts everything. *)

  val render : ?trace:(H.prim Trace.event -> unit) -> t -> element -> H.scene
  (** [render root description] reconciles [description] against what [root]
      already holds, commits the result, and returns the scene the host
      assembled from it.

      Effects run last, after the scene has been assembled — see {!Hook} for why
      that is the only time they may — and cleanups run before setups, so a
      component leaving and a component arriving in the same frame never overlap
      on whatever they both hold. One of them raising does not cancel the rest:
      everything owed runs, and the first exception comes back out once nothing
      is left owing. See {!Hook.flush}.

      A render that raises commits nothing. The description is assembled before
      anything is committed, so a host that refuses one — {!Host.HOST.assemble}
      is the last thing that can — leaves the tree from the frame before exactly
      where it stood, starts no effect and runs no cleanup, and a frame that had
      already been asked for is still asked for afterwards.

      What it cannot do is take back what the render itself wrote. A component
      runs before the host has had its say, and what it did while it ran stands:
      a {!Hook.use_ref} it wrote stays written, because that box is the
      component's own and is the same box physically every render; a
      {!Hook.use_memo} may have recomputed, which costs a recompute and nothing
      else; a setter called during a render has already written its slot. The
      [trace] events are not unsaid either — a trace records what the reconciler
      did, not what survived. The tree and the effects are the transaction; a
      component's own mutations are its own.

      [trace], if given, is called with every mount, update and unmount as they
      happen, in the order they happen. Left out, nothing is recorded and
      nothing is spent recording it. *)

  val dirty : t -> bool
  (** Whether a setter has run since this root was last rendered.

      Cleared at the start of every {!render} and set by {!Hook.use_state}'s
      setter, wherever it was called from — an effect, an event handler, a
      timer. A game that renders every frame regardless can ignore it; a menu
      that would rather not rebuild a scene nothing has changed can ask.

      By a setter belonging to a component that is {e in} the tree, which is the
      part that matters to a loop driven by this rather than by the clock. One
      whose component has left says nothing, and after {!destroy} every setter
      there ever was has left — so this cannot be stuck true by a timer that
      outlived the mount, which would be a loop rendering forever for a
      component that is not there, or reviving a root that was torn down. *)

  val destroy : ?trace:(H.prim Trace.event -> unit) -> t -> unit
  (** Unmount everything this root holds and run what that owes.

      Deepest first, and then the same flush a {!render} ends with, so a
      subscription taken out on mount is dropped here in the order it was taken.
      A root that is simply let go of does none of this: the only other path to
      a cleanup is a later {!render} that removes the component, and there is no
      later render.

      One cleanup raising does not keep the others from running, so a teardown
      that goes wrong is still a teardown that let go of everything it could.
      The exception arrives afterwards.

      The root is empty afterwards rather than spent — rendering into it again
      mounts everything fresh — and destroying it twice owes nothing the second
      time.

      Why this and not [render root Element.empty], which unmounts a tree just
      as thoroughly: that needs a scene, and a host is entitled to refuse to
      assemble one from nothing. Releasing what a root holds is not something a
      host should get a say in. *)
end
