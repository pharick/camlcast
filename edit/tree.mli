(** The component tree, as the reconciler reports it happening.

    {1 Why this is built from a trace}

    A committed frame is a forest of {!Camlcast_loom.Host.node}, and components
    have no representation in it: by then each has explained itself in terms of
    things the host understands. So a view of the {e component} tree cannot be
    read off a frame. {!Camlcast_loom.Trace} is the only thing that sees
    components at all, and this is that feed folded back into the shape it came
    from.

    Install {!watch} as {!Camlcast.Watch.trace} and call {!commit} after each
    render that stood.

    {1 What it is for}

    {b A component mounted where it should have been updated} is this engine's
    quietest mistake. Its state resets, the frame looks right, and nothing says
    so — {!Camlcast_loom.Trace} exists because of it. The usual cause is a
    component built inside another component's render: a fresh closure every
    frame is never the same component as last frame, so it is torn down and
    rebuilt along with everything under it, and the symptom is that state
    appears not to work. {!Camlcast_loom.Element} states the rule; this is how
    you find out you broke it.

    A {!row} carries {!row.remounts} for that, counted per place rather than per
    component: mounting at a path that was there the frame before is the thing
    worth showing, and mounting at a path that was not is a component arriving,
    which is ordinary.

    {1 Frames that did not happen}

    A render can be refused after events have been reported, and those events
    then describe the reconciler's walk rather than any tree that existed —
    {!Camlcast_loom.Trace.Refused} is the marker saying which reading applies. A
    frame's events are therefore held until {!commit}, and
    {!Camlcast_loom.Trace.Refused} throws them away. What this shows is always a
    tree that stood. *)

type row = {
  path : Camlcast_loom.Path.t;
  label : string;
      (** the component's own name, or the primitive as the host describes one
      *)
  depth : int;
      (** how far in, for an indented listing. The outermost element a
          description writes is 0 — the tree's own root names no element. *)
  component : bool;
      (** whether this is a component rather than a primitive. Only components
          hold state, so only they can lose it. *)
  remounts : int;
      (** how many times something has mounted at this path that was already
          there. Any number above zero is worth looking at; a number that climbs
          every frame is the mistake above. *)
}
(** One place in the tree. *)

type t
(** The tree as it stands, and what it has seen. *)

val create : unit -> t
(** A tree that has seen nothing. *)

val watch : t -> Camlcast.Prim.t Camlcast_loom.Trace.event -> unit
(** Take one event. This is {!Camlcast.Watch.trace}. *)

val commit : t -> unit
(** The frame just reported stood; show it.

    Called after a render that returned. A render that raised has already said
    {!Camlcast_loom.Trace.Refused} and needs nothing here — and calling this
    anyway does nothing, rather than promoting the emptied buffer over the frame
    that still stands. A caller should not have to know which kind of render it
    just had to be safe. *)

val rows : t -> row list
(** Every place in the last frame that stood, outermost first, each before its
    own children — the order an indented listing is written in.

    Empty until the first {!commit}. *)

val lines :
  plain:Camlcast.Color.t -> remounted:Camlcast.Color.t -> t -> Panel.line list
(** The tree as an indented listing, ready for {!Panel.draw}.

    A place that has remounted is drawn in [remounted] and carries the count,
    which is the whole point of the view: a number climbing every frame is a
    component being rebuilt every frame, and that is a mistake in the game
    rather than a fact about it. Everything else is [plain].

    Both colours are asked for because this library holds none, for the reason
    the engine holds none. *)

val remounting : t -> row list
(** The components that have remounted, worst first.

    {b Components only, though primitives are counted too.} A component torn
    down takes everything under it with it, so one mistake shows up as a remount
    on the component and on every primitive beneath — and only the first of
    those is the mistake. A primitive holds no state and so has none to lose; a
    wall rebuilt is a wall. Reach for {!rows} for the whole count, which is
    where the consequences are visible if you want them.

    What a panel with a few lines to spare should show, and what a test
    asserting "this game keeps its state" should find empty. *)
