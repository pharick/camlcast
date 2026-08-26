(** The world as a graph: rooms as nodes, doorways as stubs, connections as
    edges.

    {1 Why a graph and not a map}

    There is no map to draw. {!Camlcast_core.World} says it plainly —
    {e "deliberately no world-wide compass, no global position and no single
    floor"} — and {!Camlcast_core.Transform} exists because of it: each room is
    authored in its own coordinates and joined to its neighbours only by the
    motion one shared doorway implies. Lay a world out on one sheet of paper
    and rooms sit on top of each other; nothing ever does that, so nothing ever
    notices.

    A plan of one room is therefore the most a picture of the geometry can
    honestly be — see {!Camlcast_edit.Sheet} — and everything {e between}
    rooms is a graph. Which is convenient, because that is also the part that
    is hard to keep in your head: which door leads where, and which leads
    nowhere yet.

    {1 A doorway that leads nowhere is a state, not a mistake}

    {!Camlcast_core.World} builds one, the renderer fills it with haze, and
    {!Camlcast.Check} reports it as a warning rather than an error — a level is
    built rooms first and ways between them after, and refusing the half-built
    state would refuse the way anyone works. So a stub here is a thing to be
    shown and offered, not flagged. Joining two of them is what {!join} writes.
*)

type door = {
  path : Camlcast_loom.Path.t;
  at : Camlcast_loom.Element.pos option;
      (** where the {!Camlcast.P.cut} that made it was written, when
          [ppx_camlcast] put a position there. {!join} needs it: what a
          connection has to name is the {e binding} the door was made under,
          and only the source says what that is. *)
  id : int;  (** the identity a connection joins by *)
  name : string option;  (** what the description called it, for diagnostics *)
  joined : bool;  (** whether a connection names it *)
}

type room = {
  path : Camlcast_loom.Path.t;
  index : int;
      (** its place among the world's rooms, as assembly orders them *)
  name : string option;
  doors : door list;
}

type t = { rooms : room list; links : (int * int) list }
(** [links] pairs door identities, as the description's connections do. *)

val read : Camlcast.Watch.node list -> t
(** The graph this frame describes. *)

type placed = { room : room; x : int; y : int; radius : int }
(** A room given somewhere to be drawn. *)

val place : t -> x:int -> y:int -> width:int -> height:int -> placed list
(** Somewhere for each room, around a circle.

    {b Stable rather than pretty.} A force-directed layout would read better
    for a large world and would also move every frame it was re-run, which for
    a view redrawn sixty times a second means a graph that will not sit still
    to be clicked on. A circle is decided by the number of rooms and their
    order, so it is the same picture until the world itself changes. *)

type hit = Room of placed | Door of { room : placed; door : door } | Nothing

val hit : placed list -> int * int -> hit
(** What is under this pixel. A door stub wins over the room it belongs to. *)

val draw :
  view:placed list ->
  ink:Camlcast.Color.t ->
  joined:Camlcast.Color.t ->
  stub:Camlcast.Color.t ->
  picked:Camlcast.Color.t ->
  selected:hit ->
  t ->
  Camlcast.P.t
(** The graph over the finished frame.

    A connection is drawn in [joined] and a doorway leading nowhere in [stub] —
    the same green and red {!Camlcast.Debug_map} marks a threshold with, and
    for the same reason: from inside, an unlinked doorway draws as haze, and so
    does distance. *)

val join :
  Span.t ->
  world:Span.call ->
  door ->
  door ->
  (Span.span * string, [ `Msg of string ]) result
(** The edit that connects these two doorways, ready for
    {!Span.splice}.

    A connection is a child of the world rather than of either room, which is
    what lets joining two rooms and unjoining them touch neither — so this
    appends to the world's own list of children, and the span it hands back is
    the empty one just inside that list's closing bracket. A zero-width span is
    an insertion, which is what {!Span.splice} does with one.

    It refuses a door joined to something already, and refuses to join a
    doorway to itself. Neither is a thing the engine would build, and finding
    out here says so in the terms the editor is working in rather than as a
    world that will not assemble.

    {b The names it writes come from the source and not from the
       description.} A door's [name] is what [P.door ~name] was told, and that
    is for diagnostics: [demo/level.ml] calls one ["east"] while binding it to
    [plaza_east], and a connection naming ["east"] would name nothing. So each
    door is followed back to the {!Camlcast.P.cut} that placed it, and the
    identifier written there is what goes in the line. A door whose node
    carries no position cannot be followed, and is refused — which is the
    ordinary consequence of building without the rewriter, and says so. *)
