(** Matching this frame's description against last frame's tree.

    A game hands over a whole {!Element.t} every frame. Behind it is a tree of
    instances, alive since the frame each of its parts first appeared, holding
    everything that must outlive a description. The reconciler walks the two in
    step, decides at each place whether the thing described is the thing already
    there, and keeps or replaces accordingly.

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
    its key wherever either of them has moved to. An unkeyed one is matched
    against whatever stood at its own index, and only if that was unkeyed too.
    Position is the whole of an unkeyed child's identity, exactly as {!Path}
    states it. The two must agree, or a child could keep its state across a move
    that changed the name everything else knows it by.

    {b One parent's keys are unique}, and a repeat raises
    {!Element.Duplicate_key}. See {!Element} for why an ambiguous path is not a
    question worth answering.

    A fragment is matched by key like anything else. That lets a helper that
    returns several primitives at once — with no single one of them to hang a
    key on — be rearranged with everything under it intact.

    Leftovers are unmounted in the order they were declared, so that a trace of
    a frame reads the same way twice. *)

module Make (H : Host.HOST) : sig
  type element = H.prim Element.t
  (** {!Element.t} at this host's primitive, which is the type a game writes. *)

  type t
  (** A mounted root: the instance tree, and everything hanging off it.

      Made once and rendered into repeatedly. Two roots share nothing. That lets
      a test drive several in one process, and later lets a launcher hold a menu
      and a world at the same time. *)

  val create : unit -> t
  (** An empty root. The first {!render} into it mounts everything. *)

  val render :
    ?trace:(H.prim Trace.event -> unit) ->
    ?inspect:(Path.t -> Hook.slot array -> unit) ->
    ?patch:(H.prim Host.node list -> H.prim Host.node list) ->
    t ->
    element ->
    H.scene
  (** [render root description] reconciles [description] against what [root]
      already holds, commits the result, and returns the scene the host
      assembled from it.

      Effects run last, after the scene has been assembled — see {!Hook} for why
      that is the only time they may. Cleanups run before setups, so a component
      leaving and a component arriving in the same frame never overlap on
      whatever they both hold. One of them raising does not cancel the rest:
      everything owed runs, and the first exception comes back out once nothing
      is left owing. See {!Hook.Runtime.flush}.

      A render that raises commits nothing. The description is assembled before
      anything is committed, so a host that refuses one — {!Host.HOST.assemble}
      is the last thing that can — leaves the tree from the frame before exactly
      where it stood, starts no effect and runs no cleanup. A frame that had
      already been asked for is still asked for afterwards.

      What a raising render cannot do is take back what the render itself wrote.
      A component runs before the host has had its say, and what it did while it
      ran stands: a {!Hook.use_ref} it wrote stays written, because that box is
      the component's own and is physically the same box every render; a
      {!Hook.use_memo} may have recomputed, which costs a recompute and nothing
      else; a setter called during a render has already written its slot. The
      [trace] events are not unsaid either — a trace records what the reconciler
      did, not what survived. The tree and the effects are the transaction; a
      component's own mutations are its own.

      What a refused render does say is {!Trace.Refused}, last and once, so that
      the events before it can be read for what they are. Without it a trace is
      not merely partial: a component reported unmounted by the refused frame is
      reported {e updated} by the next one, and those two lines cannot both be
      true of a tree. They are both true of the walk, which is what a trace is
      of, and {!Trace.Refused} is the line that says so.

      [trace], if given, is called with every mount, update and unmount as they
      happen, in the order they happen. Left out, nothing is recorded and
      nothing is spent recording it.

      [inspect], if given, is called for each component as it is rendered, with
      its path and what its hooks are holding. A separate channel from [trace]
      rather than a field on {!Trace.node}, because a trace reports what the
      reconciler {e did} and a row of slots is not that: the events are a
      history of one walk and this is a reading taken during it. It is called
      where the reconciler already holds the row, so it costs a walk of nothing
      extra; left out it costs the same nothing [trace] does, the row not even
      being read. See {!Hook.type-slot} for what can be said about a slot and
      what cannot.

      [patch] is given the committed forest and hands back the one to assemble,
      and is the seam a tool outside the game edits through. What reaches it is
      what the description came to rather than what the description said, so a
      change made here lands whatever component described the thing — and
      whether that component holds state, hooks or neither does not enter into
      it. Every node carries the {!Path.t} it was reconciled at, which is what a
      patch says {e which} node it means by.

      It transforms the whole forest rather than one node at a time so that
      adding something and moving something are the same kind of edit, made at
      the same moment. It runs inside the frame's one chance to be refused: a
      patch that raises, or that hands back a forest the host will not build,
      loses the frame exactly as a description doing either would, leaving the
      tree from the frame before standing and no effect run.

      Left out, nothing is spent. It is a match on an option once a frame, which
      is the same nothing [trace] costs. *)

  val dirty : t -> bool
  (** Whether a setter has run since this root was last rendered.

      Cleared at the start of every {!render} and set by {!Hook.use_state}'s
      setter, wherever it was called from — an effect, an event handler, a
      timer. A game that renders every frame regardless can ignore it; a menu
      that would rather not rebuild an unchanged scene can ask.

      Only a setter belonging to a component that is {e in} the tree sets it,
      which is the part that matters to a loop driven by this rather than by the
      clock. A setter whose component has left says nothing, and after
      {!destroy} every setter there ever was has left. So this cannot be stuck
      true by a timer that outlived the mount — which would mean a loop
      rendering forever for a component that is not there, or reviving a root
      that was torn down. *)

  val destroy : ?trace:(H.prim Trace.event -> unit) -> t -> unit
  (** Unmount everything this root holds and run what that owes.

      Deepest first, and then the same flush a {!render} ends with, so a
      subscription taken out on mount is dropped here in the order it was taken.
      A root that is simply let go of does none of this: the only other path to
      a cleanup is a later {!render} that removes the component, and there is no
      later render.

      One cleanup raising does not keep the others from running, so a teardown
      that goes wrong still lets go of everything it could. The exception
      arrives afterwards.

      The root is empty afterwards rather than spent — rendering into it again
      mounts everything fresh — and destroying it twice owes nothing the second
      time.

      This exists instead of [render root Element.empty], which unmounts a tree
      just as thoroughly, because that form needs a scene, and a host is
      entitled to refuse to assemble one from nothing. Releasing what a root
      holds is not something a host should get a say in. *)
end
