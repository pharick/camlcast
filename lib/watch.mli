(** What a tool outside the game asks to be told, and allowed to change.

    A run is a closed loop: a description goes in and frames come out, and
    nothing in between is a value anything else holds. These three are the
    openings in that loop, and they exist for the same consumer — something
    watching the game while it runs, showing what the tree is doing and editing
    what it draws.

    {b One record, for the reason {!Controls} is one.} A run already takes a
    record of what it acts on by itself; this is the record of what it reports
    and what it lets be overruled. Adding a fourth opening later is then a field
    rather than a fourth optional argument on {!Run.on}, and turning the whole
    apparatus off is passing {!none} rather than remembering three names.

    {b Nothing here is on by default}, and each field costs nothing while it is
    [None] — not a call, and not the allocation of what the call would have been
    handed. {!Camlcast_loom.Trace} states that promise for the first and
    {!Camlcast_loom.Reconcile} keeps it for the other two. *)

type node = Prim.t Camlcast_loom.Host.node
(** One primitive in a committed frame, with its path, its position and what it
    contains.

    Aliased here because {!patch} is written in terms of it and a game naming
    only [camlcast] cannot otherwise spell it: the type belongs to
    [camlcast.loom], which [(implicit_transitive_deps false)] keeps out of reach
    until a dune file asks for it. Asking is reasonable for a tool built on
    this; having to ask in order to write down the type of an argument this
    library already takes is not. *)

type t = {
  trace : (Prim.t Camlcast_loom.Trace.event -> unit) option;
      (** every mount, update and unmount, as it happens.

          The only place components are visible at all: by the time a frame is a
          forest they have explained themselves away, so a view of the component
          tree can be built from this and from nothing else. *)
  inspect :
    (Camlcast_loom.Path.t -> Camlcast_loom.Hook.slot array -> unit) option;
      (** what each component is holding, as it renders.

          See {!Camlcast_loom.Hook.type-slot}: which hook made each slot and
          whether it has changed come free, and what a slot holds is readable
          only where the component said how with [~show]. *)
  patch :
    (Prim.t Camlcast_loom.Host.node list -> Prim.t Camlcast_loom.Host.node list)
    option;
      (** the committed frame, on its way to being built.

          What reaches it is what the description came to rather than what the
          description said, so an edit made here lands whichever component wrote
          the thing and whether that component holds state does not enter into
          it. A frame is refused if this hands back something the engine will
          not build, in exactly the way a description that could not be built is
          refused. *)
}
(** The three openings. A record with every field [None] is {!none}. *)

val none : t
(** Nothing watched and nothing overruled: what a run uses when it is not asked.
*)

val make :
  ?trace:(Prim.t Camlcast_loom.Trace.event -> unit) ->
  ?inspect:(Camlcast_loom.Path.t -> Camlcast_loom.Hook.slot array -> unit) ->
  ?patch:
    (Prim.t Camlcast_loom.Host.node list -> Prim.t Camlcast_loom.Host.node list) ->
  unit ->
  t
(** {!none}, with whatever is named here in place of it. *)
