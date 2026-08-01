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
    time or fewer, all raise. What it cannot catch is a conditional that swaps
    one {!use_state} for another {!use_state} of a different type at the same
    index — same kind, same slot, different type. That is a violation of the
    rule, it is the only one that gets through, and it is unsound rather than
    merely wrong. Do not write conditional hooks.

    {1 Setting state}

    A setter writes the slot and marks the root as having work to do. It does
    {e not} re-enter the render that is running, and the new value is not
    visible to the render that read the old one — that render already has its
    answer. The change shows up next frame.

    This is deliberately simpler than React, which re-renders until the tree
    settles. A game renders every frame anyway, so "next frame" is a wait of
    milliseconds, and in exchange there is no possibility of a render loop that
    does not converge. *)

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
    {!use_memo}. *)

val use_ref : 'a -> 'a ref
(** A box made once and handed back unchanged ever after.

    The same box, physically, every render: writing to it is how a component
    remembers something without asking to be re-rendered. That is the whole
    difference from {!use_state} — a ref is for what the component needs to
    know, state is for what the picture depends on. *)

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
    instead, pass [~equal:(fun _ _ -> false)]. *)

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
    predecessor is still holding must not see the two overlap. *)

val run :
  slots:slots ->
  pending:pending ->
  at:string ->
  invalidate:(unit -> unit) ->
  (unit -> 'a) ->
  'a
(** Call a component's render with the hook effects handled against [slots].

    [at] names the component in an exception, [invalidate] is what a setter
    calls to say the tree has work to do. Raises {!Hook_order_changed} if this
    render's hooks do not line up with the row as it stands. *)
