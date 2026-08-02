(** State that outlives the description that asked for it.

    A component is a function called afresh every frame, so anything it wants to
    remember has to live somewhere else. That somewhere is a row of slots on the
    component's instance, and a hook is a request for the next one: the first
    {!use_state} in a render gets the first slot, the second gets the second,
    and so on every frame in the same order. Nothing names them, so
    {b the order is the identity}, which is where the one rule comes from.

    {1 The rule}

    {b Call hooks unconditionally, in the same order, every render.} Not inside
    an [if], not inside a loop whose length varies, not after an early return.
    Where React's docs ask this and shrug, this raises: a slot remembers which
    hook made it, and a render that asks for the wrong kind gets
    {!Hook_order_changed} rather than a value that was never there.

    {1 What the check does and does not catch}

    Slots hold [Obj.t], because a row of them is heterogeneous and OCaml has no
    other way to say so. The cast back is sound exactly when the rule above
    holds — the same call site reaches the same slot every time, so the type it
    stored is the type it reads.

    The tag on each slot makes most violations of the rule loud: a {!use_state}
    where a {!use_ref} used to be, a render that asks for more hooks than last
    time or fewer, all raise. What a tag cannot catch is a slot that keeps its
    kind and changes its type — same kind, same index, different type. That is
    unsound rather than merely wrong: the cast back reads an [int] as a
    [string], and nothing downstream is checking. There are two ways to write
    it, and they are not variations of one thing.

    The first is a conditional that swaps one {!use_state} for another of a
    different type at the same index. That is a violation of the rule above, and
    the rule is the whole of the answer: do not write conditional hooks.

    The second breaks no rule stated anywhere else, which is why it is stated
    here: a render closure that is {b polymorphic in its props}. A component is
    its closure and nothing else, so one explicitly typed ['a. 'a -> _] is a
    single component at every type it is ever applied at — and a single
    component has one slot row. A frame that renders it at [int] and a frame
    that renders it at [string] share that row, so a state stored on the first
    is read back at the wrong type on the second. Hooks are ordered correctly
    and called unconditionally throughout; there is nothing here for a tag to
    notice. See {!Element} for the rule and for why {!Element.declare} happens
    to make it hard to break.

    {1 Setting state}

    A setter writes the slot and marks the root as having work to do. It does
    {e not} re-enter the render that is running, and the new value is not
    visible to the render that read the old one — that render already has its
    answer. The change shows up next frame.

    This is deliberately simpler than React, which re-renders until the tree
    settles. A game renders every frame anyway, so "next frame" is a wait of
    milliseconds, and in exchange there is no possibility of a render loop that
    does not converge.

    A setter is a value, and a game may keep one — in a timer, a subscription, a
    callback handed to something the runtime knows nothing about — for longer
    than the component that made it lasts. Called after that component has left
    the tree, it does nothing: the slot it would write is one nothing will read
    again, and it asks for no frame. That last part is the one worth stating,
    because the alternative is a frame nobody wants, and on a root that
    {!Reconcile.Make} has destroyed a frame nobody will ever render — leaving
    that root's [dirty] answering yes for good, with nothing able to clear it.
    Liveness is per component and not per root, so a departing component's
    cleanup calling a {e parent's} setter still asks for the frame it means to.
*)

exception Hook_outside_render
(** A hook was called with no render around it — at the top level of a module,
    say, or inside a callback that outlived the frame it was made in. *)

exception
  Hook_order_changed of { at : string; expected : string; found : string }
(** A render asked for a different hook than last time at the same position.
    [at] is the component's path, [expected] the hook the slot was made by, and
    [found] the one just asked for. [expected] is ["nothing"] when the render
    asked for more hooks than it did last time, and [found] is ["nothing"] when
    it stopped early and asked for fewer. *)

val use_state : 'a -> 'a * ('a -> unit)
(** [use_state initial] is the value held in this slot and a way to replace it.

    [initial] is used the first time this component renders and ignored every
    time after, so it costs nothing to write an expression there — but it is
    evaluated each render regardless, so an expensive one belongs in
    {!use_memo}.

    The setter outlives nothing: kept past the component's own life and called
    then, it writes what will not be read and asks for no frame — see "Setting
    state" above. *)

val use_ref : 'a -> 'a ref
(** A box made once and handed back unchanged ever after.

    The same box, physically, every render: writing to it is how a component
    remembers something without asking to be re-rendered. That is the whole
    difference from {!use_state} — a ref is for what the component needs to
    know, state is for what the picture depends on.

    Being the same box is also why a write to it survives a frame the host
    refuses. What a refused frame rolls back is the tree and the effects, and a
    ref is neither: it is the component's own, and it was written before there
    was any refusing. See {!Reconcile}. *)

val use_memo : ?equal:('d -> 'd -> bool) -> deps:'d -> (unit -> 'a) -> 'a
(** [use_memo ~deps compute] is [compute ()], recomputed only when [deps]
    changes.

    [equal] decides what changing means, and defaults to structural equality.
    That default raises on functional values, as [( = )] always does, so deps
    containing a closure need an [equal] of their own. *)

val use_effect :
  ?equal:('d -> 'd -> bool) ->
  deps:'d ->
  (unit -> (unit -> unit) option) ->
  unit
(** [use_effect ~deps f] runs [f] after the frame is committed, and again
    whenever [deps] changes.

    [f] returns an optional cleanup, which runs before the next [f] and once
    more when the component is unmounted. A subscription taken out in [f] and
    dropped in its cleanup is therefore exactly as long-lived as the component.

    Effects run {e after} the scene has been assembled, never during a render. A
    render is supposed to be a pure function of props and state, and this is the
    seam where a component is allowed to reach outside that.

    [~deps:()] is the common case of "once, on mount". To run every frame
    instead, pass [~equal:(fun _ _ -> false)].

    An [f] that raises is taken to have taken nothing: no cleanup is held for
    it, not even the one the run before it gave back, which by then has already
    been called. It is not tried again for those deps — the raise comes back out
    of the render that flushed it, and that is the report. *)

val use_context : 'a Context.t -> 'a
(** The value the nearest enclosing {!Element.provide} bound for this context,
    or the context's own default where there is none.

    Claims no slot, because it answers the same way every render of a given
    component — so unlike its neighbours above, it is harmless inside a
    conditional. Write it unconditionally anyway; the rule is easier to keep
    than to keep track of exceptions to. *)

val use_invalidate : unit -> unit -> unit
(** A function that tells this component's root it has work to do.

    The seam for anything the runtime does not own: a store, a timer, a file
    being watched. Take a subscription in a {!use_effect}, call this from the
    callback, drop the subscription in the cleanup, and the outside world can
    reach a frame without knowing what a frame is. {!Store} is exactly that and
    nothing more.

    Dropping the subscription in the cleanup is how it is meant to be written,
    and not what makes it safe: this goes quiet on its own once the component
    has left the tree, so a source that is slow to let go — or never asked to —
    cannot wake a root that has stopped listening, or a root that no longer
    exists. A {!use_state} setter is the same seam and behaves the same way.

    Claims no slot, for the same reason {!use_context} does not. *)

(** {1 The runtime side}

    Below is what {!Reconcile} needs to drive the above, and nothing a game
    should ever call. *)

type slots
(** One component's row of slots. *)

val slots : unit -> slots
(** A fresh, empty row, for a component being mounted. *)

type pending
(** Work a frame has accumulated but not yet done: cleanups to run and effects
    to start, both of which have to wait until the scene has been assembled. *)

val pending : unit -> pending

val on_unmount : pending -> slots -> unit
(** Queue every cleanup still outstanding in [slots], in the order the effects
    were declared. What unmounting a component owes the world.

    Queued rather than run, so that a component going away and a component
    arriving in the same frame still see cleanup-before-setup — the ordering
    {!flush} exists to keep. *)

val flush : pending -> unit
(** Run everything accumulated, cleanups before setups, and empty the queue.

    Cleanups first and all of them first: an effect that takes a resource its
    predecessor is still holding must not see the two overlap.

    {b Everything queued runs, whatever any of it raises.} The queue is emptied
    before the first of them is called, so stopping at a raise would leave the
    rest owed with nothing left holding them and no second flush coming — and
    the tree that owes them is already committed. A raise is therefore caught
    and held, and the first one is re-raised, with its own backtrace, once
    nothing is left owing. Later ones are lost: a frame has one thing to report,
    and this is the thing that went wrong first. *)

val discard : pending -> unit
(** Throw everything accumulated away without running any of it.

    What a render that did not finish does with the work it queued. A setup
    belongs to a tree that was never committed, and a cleanup to a component
    that is therefore still standing: neither is owed, and the render that tries
    again decides both again from the tree as it really is. *)

val run :
  slots:slots ->
  pending:pending ->
  at:string ->
  env:Context.binding list ->
  invalidate:(unit -> unit) ->
  (unit -> 'a) ->
  'a
(** Call a component's render with the hook effects handled against [slots].

    [at] names the component in an exception, [env] is the context bindings in
    force here — innermost first — and [invalidate] is what a setter calls to
    say the tree has work to do. Raises {!Hook_order_changed} if this render's
    hooks do not line up with the row as it stands. *)
