(** State that outlives the description that asked for it.

    A component is a function called afresh every frame, so anything it wants to
    remember has to live somewhere else. That somewhere is a row of slots on the
    component's instance, and a hook is a request for the next one: the first
    {!use_state} in a render gets the first slot, the second gets the second,
    and so on every frame in the same order. Nothing names the slots, so
    {b the order is the identity}. That is where the one rule comes from.

    {1 The rule}

    {b Call hooks unconditionally, in the same order, every render.} Not inside
    an [if], not inside a loop whose length varies, not after an early return.
    React's docs ask this without checking it; here a violation raises. A slot
    remembers which hook made it, and a render that asks for the wrong kind gets
    {!Hook_order_changed} rather than a value that was never there.

    {1 What the check does and does not catch}

    Slots hold [Obj.t], because a row of them is heterogeneous and OCaml has no
    other way to say so. The cast back is sound exactly when the rule above
    holds: the same call site reaches the same slot every time, so the type it
    stored is the type it reads.

    The tag on each slot makes most violations of the rule loud. A {!use_state}
    where a {!use_ref} used to be, a render that asks for more hooks than last
    time, or one that asks for fewer — all raise. What a tag cannot catch is a
    slot that keeps its kind and changes its type: same kind, same index,
    different type. That is unsound rather than merely wrong. The cast back
    reads an [int] as a [string], and nothing downstream is checking. There are
    two ways to write it, and they are not variations of one thing.

    The first is a conditional that swaps one {!use_state} for another of a
    different type at the same index. That violates the rule above, and the rule
    is the whole of the answer: do not write conditional hooks.

    The second breaks no rule stated anywhere else, which is why it is stated
    here: a render closure that is {b polymorphic in its props}. A component is
    its closure and nothing else, so one explicitly typed ['a. 'a -> _] is a
    single component at every type it is ever applied at, and a single component
    has one slot row. A frame that renders it at [int] and a frame that renders
    it at [string] share that row, so a state stored on the first is read back
    at the wrong type on the second. Hooks are ordered correctly and called
    unconditionally throughout; there is nothing here for a tag to notice. See
    {!Element} for the rule and for why {!Element.declare} happens to make it
    hard to break.

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
    again, and it asks for no frame. The no-frame part is worth stating, because
    the alternative is a frame nobody wants — and on a root that
    {!Camlcast_loom.Reconcile.Make} has destroyed, a frame nobody will ever
    render, leaving that root's [dirty] answering yes for good with nothing able
    to clear it. Liveness is per component and not per root, so a departing
    component's cleanup calling a {e parent's} setter still asks for the frame
    it means to.

    {1 When a hook fails}

    A hook raises where it was written, and is in that respect an ordinary
    expression: a [try] around it in the render body catches, a {!Fun.protect}
    it is nested inside gives back what it was holding, and an
    [Invalid_argument] out of it is translated by {!Camlcast_loom.Reconcile}
    into an {!Element.Render_refused} naming the component, exactly as one
    raised straight from the body is.

    That is worth stating because it is not what the machinery does by itself. A
    hook is an effect, and the code that answers one runs {e outside} the fiber
    the component is suspended in. A raise from there would come out past every
    one of those constructs and abandon the fiber rather than unwind it, leaving
    finalisers unrun. The runtime walks the failure back to the point the hook
    was called instead. It matters most for the two hooks that run a game's own
    code while answering — {!use_memo}'s [compute] and either's [equal] — and it
    is the same for {!Hook_order_changed}, which is a hook failing like any
    other.

    Catching one is allowed the ordinary amount, and leaves the ordinary thing
    behind: a {!use_memo} whose [compute] raised has remembered nothing, so its
    slot is simply still empty and the next render asks again — a fallback
    rendered around a failure is a fallback, not a corruption. The exception to
    this is {!Hook_order_changed} itself. It means this render and the slot row
    no longer agree about what comes next, so the only sound thing to do after
    catching it is to call no further hooks: the slot it stopped at still holds
    the other order's value, and a later hook that happened to match its kind
    would read that value as something it never was. *)

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

val use_state : ?show:('a -> string) -> 'a -> 'a * ('a -> unit)
(** [use_state initial] is the value held in this slot and a way to replace it.

    [initial] is used the first time this component renders and ignored every
    time after, so it costs nothing to write an expression there. It is still
    evaluated each render regardless, so an expensive one belongs in
    {!use_memo}.

    The setter outlives nothing: kept past the component's own life and called
    then, it writes what will not be read and asks for no frame — see "Setting
    state" above.

    [show] is how this value reads to a tool looking at the tree, and nothing
    else ever calls it. Omit it and the slot is still counted, still says which
    hook made it and still says whether it changed; only its value goes unnamed.
    See "Looking at what a component holds" below for why it is asked for rather
    than worked out. *)

val use_ref : ?show:('a -> string) -> 'a -> 'a ref
(** A box made once and handed back unchanged ever after.

    It is physically the same box every render. Writing to it is how a component
    remembers something without asking to be re-rendered, which is the whole
    difference from {!use_state}: a ref is for what the component needs to know,
    state is for what the picture depends on.

    Being the same box is also why a write to it survives a frame the host
    refuses. What a refused frame rolls back is the tree and the effects, and a
    ref is neither: it is the component's own, and it was written before there
    was any refusing. See {!Camlcast_loom.Reconcile}.

    [show] is given the box's {e contents} rather than the box, that being the
    thing worth reading. Note that a ref written in place holds the same box
    throughout, so {!type-slot.changed} stays [false] however often it is
    written: physical identity is what it can ask, and the identity did not
    move. A ref is for what a component needs to know rather than for what the
    picture depends on, so there is nothing here for a view to miss. *)

val use_memo :
  ?show:('a -> string) ->
  ?equal:('d -> 'd -> bool) ->
  deps:'d ->
  (unit -> 'a) ->
  'a
(** [use_memo ~deps compute] is [compute ()], recomputed only when [deps]
    changes.

    [show] is given the remembered value. A slot whose [compute] has not run
    yet, or whose last one raised, holds nothing and reads as {!type-slot.shown}
    [None].

    [equal] decides what changing means, and defaults to structural equality.
    That default raises on functional values, as [( = )] always does, so deps
    containing a closure need an [equal] of their own.

    Both [compute] and [equal] run during the render, and neither is the place
    to reach outside the component — that is {!use_effect}. What either raises
    arrives at this call, as "When a hook fails" above sets out; the default
    [equal] on deps that hold a closure is the way a game reaches that without
    having written a raise at all. A [compute] that raises has computed nothing:
    the slot stays empty, and the same call on the next render runs it again. *)

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
    been called. It is not tried again for those deps. The raise comes back out
    of the render that flushed it, and that is the report. *)

val use_context : 'a Context.t -> 'a
(** The value the nearest enclosing {!Element.provide} bound for this context,
    or the context's own default where there is none.

    Claims no slot, because it answers the same way every render of a given
    component. Unlike its neighbours above it is therefore harmless inside a
    conditional. Write it unconditionally anyway; the rule is easier to keep
    than to keep track of exceptions to. *)

val use_invalidate : unit -> unit -> unit
(** A function that tells this component's root it has work to do.

    This is the seam for anything the runtime does not own: a store, a timer, a
    file being watched. Take a subscription in a {!use_effect}, call this from
    the callback, drop the subscription in the cleanup, and the outside world
    can reach a frame without knowing what a frame is. {!Store} is exactly that
    and nothing more.

    Dropping the subscription in the cleanup is how it is meant to be written,
    not what makes it safe. This goes quiet on its own once the component has
    left the tree, so a source that is slow to let go — or was never asked to —
    cannot wake a root that has stopped listening, or a root that no longer
    exists. A {!use_state} setter is the same seam and behaves the same way.

    Claims no slot, for the same reason {!use_context} does not. *)

(** {1 Looking at what a component holds}

    A slot's value is an [Obj.t]. The row is a heterogeneous array of whatever
    each hook was handed, and the tag records which hook made a cell rather than
    what type it holds — so the runtime cannot print one, and no amount of care
    here would let it. What can be answered without a type is answered below for
    every slot at no cost; what cannot is answered only where a component says
    how.

    That split is the engine's own rule applied once more. It holds no content,
    only the types content is a value of, and how to write a value down is
    content. A tool that guessed instead — walking the representation and
    printing what it found — would have to call an [int], a [bool], a [char] and
    a constant constructor the same thing, because at runtime they are the same
    thing. [true] would read as [1]. {!type-slot.shown} being [None] says less
    and is not wrong. *)

type kind =
  | State
  | Ref
  | Memo
  | Effect
      (** Which hook made a slot. Read from the tag the row already carries for
          the ordering rule above, so it costs nothing to ask. *)

type slot = {
  kind : kind;
  changed : bool;
      (** whether this slot's value is no longer physically the one the last
          look found.

          Needs no type and cannot be wrong about a value it does not
          understand, which is what makes it free. It is {e physical}
          inequality, so a setter called with a value equal to the one already
          there still counts as a change if it built a new one — which is the
          honest answer, [==] being the only question that can be asked here.

          "Since the last look" and not "since the last render": nothing is
          recorded per frame, so a view that redraws every frame reads it as
          per-frame and one that redraws when something happens reads it as
          since-then. The first look at a row reports [false] throughout, there
          being no earlier one to measure against; that a component is new is
          {!Camlcast_loom.Trace.Mounted}'s to say.

          A {!use_effect} slot holds its deps and whatever its last run took, so
          a change to one means {b the effect ran} — on the look after it first
          starts, and again whenever its deps move. That is the useful reading
          of the same rule rather than an exception to it. *)
  shown : string option;
      (** what the component said this holds, and [None] where it said nothing —
          including a {!use_memo} whose [compute] has not run or has raised,
          which holds nothing to print. *)
}
(** One slot, as much as can be said about it. *)

module Runtime : sig
  (** What {!Camlcast_loom.Reconcile} needs to drive the hooks above, and
      nothing a game should ever call.

      A submodule rather than a heading, which is what this was. Nothing here
      damages a game that calls it the way {!Camlcast_core.Input.Runtime} does —
      the worst is a row of slots nobody drives — but the same argument applies
      to both: a boundary a reader has to be told about is one a reader can
      miss. {!Camlcast.Hook} does not re-export this, so reaching it means
      naming [camlcast.loom] in a dune file. *)

  type slots
  (** One component's row of slots. *)

  val slots : unit -> slots
  (** A fresh, empty row, for a component being mounted. *)

  val inspect : slots -> slot array
  (** This row, in the order the component's hooks are called.

      Reads the values and writes nothing but the mark {!type-slot.changed} is
      measured against, which is why two readers of one row would each see the
      other's looks as having happened. There is one reader: the [inspect]
      {!Camlcast_loom.Reconcile.Make.render} takes. *)

  type pending
  (** Work a frame has accumulated but not yet done: cleanups to run and effects
      to start, both of which have to wait until the scene has been assembled.
  *)

  val pending : unit -> pending

  val on_unmount : pending -> slots -> unit
  (** Queue every cleanup still outstanding in [slots], in the order the effects
      were declared. This is what unmounting a component owes the world.

      Queued rather than run, so that a component going away and a component
      arriving in the same frame still see cleanup-before-setup — the ordering
      {!flush} exists to keep. *)

  val flush : pending -> unit
  (** Run everything accumulated, cleanups before setups, and empty the queue.

      Cleanups run first, and all of them first: an effect that takes a resource
      its predecessor is still holding must not see the two overlap.

      {b Everything queued runs, whatever any of it raises.} The queue is
      emptied before the first of them is called. Stopping at a raise would
      leave the rest owed with nothing left holding them and no second flush
      coming — and the tree that owes them is already committed. A raise is
      therefore caught and held, and the first one is re-raised, with its own
      backtrace, once nothing is left owing. Later ones are lost: a frame has
      one thing to report, and that is the thing that went wrong first. *)

  val discard : pending -> unit
  (** Throw everything accumulated away without running any of it.

      This is what a render that did not finish does with the work it queued. A
      setup belongs to a tree that was never committed, and a cleanup to a
      component that is therefore still standing: neither is owed, and the
      render that tries again decides both again from the tree as it really is.
  *)

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
      hooks do not line up with the row as it stands.

      Whatever a hook raises — that, or a game's own [compute] or [equal] — is
      raised {e into} the component at the point the hook was called, not out of
      this call over its head. It therefore reaches a caller through the render
      closure, having run whatever that closure had arranged to run on its way
      out. See "When a hook fails" above; the alternative is what an effect
      handler does if left to itself. *)
end
