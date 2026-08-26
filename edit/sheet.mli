(** A room from above, drawn from the frame rather than from the world, and
    what is under the pointer.

    {1 Why from the frame}

    {!Camlcast.Debug_map} draws the same picture from a
    {!Camlcast_core.World.t}, and for its purpose that is right: it shows what
    is wrong with the room the player is standing in, and a fault is a fault
    whoever wrote it. An editor needs more. To take hold of a wall it has to
    know which wall, in terms that survive the description being rebuilt — and
    a {!Camlcast_core.Room.t} has no such terms. Its walls are an array, and an
    array index means nothing the next frame.

    So this draws the committed forest. Every node in one carries a
    {!Camlcast_loom.Path.t}, which is what {!Camlcast_edit.Patch} names an edit
    by, and the position [ppx_camlcast] put on it, which is what
    {!Camlcast_edit.Link} finds a line of source by. The picture is the same
    picture; what it is made of is different.

    Both use {!Camlcast_core.Overhead} for the projection, so the two views
    cannot disagree about where a wall is.

    {1 One room at a time}

    A world has no shared frame to draw two rooms in —
    {!Camlcast_core.World} says so, and {!Camlcast_core.Transform} exists
    because of it. What is drawn is one room's own coordinates. *)

type item = {
  path : Camlcast_loom.Path.t;
  at : Camlcast_loom.Element.pos option;
  what : Camlcast.Prim.t;
}
(** One thing in the room, with the two things an editor needs about it: where
    it is in the tree, and where it was written. *)

type hit =
  | Corner of { item : item; which : int }
  | Body of item
  | Nothing
      (** What is under a point.

    A corner wins over the body it belongs to, and within the grab radius, the
    nearer corner wins. That is the ordinary rule and it is what makes a wall
    draggable by its ends and by its middle without the two fighting. [which]
    is 0 for the first endpoint and 1 for the second. *)

val items : Camlcast.Watch.node list -> room:int -> item list
(** Everything drawable in one room of this frame, in the order the description
    wrote it.

    [room] counts rooms as they appear in the forest, which is the order
    {!Camlcast_core.World.make} gives them and so the order a
    {!Camlcast_core.Player} names one by. *)

val bounds : item list -> (float * float * float * float) option
(** The rectangle these occupy, for {!Camlcast_core.Overhead.fit}.

    {!Camlcast_core.Overhead.bounds} answers the same question of a
    {!Camlcast_core.Room.t}, and this one is asked of a frame instead —
    which is the whole difference this module exists for. Sprites are measured
    here where that one leaves them out: what is drawn on a plan should fit on
    it, and a sprite an editor can drag is a thing it has to be able to reach.
*)

val hit : Camlcast_core.Overhead.t -> item list -> int * int -> hit
(** What is under this pixel.

    The grab radius is in pixels rather than world units: a corner should be as
    easy to take hold of on a small room as on a large one, and it is the
    pointer that has to reach it. *)

val draw :
  view:Camlcast_core.Overhead.t ->
  draggable:(item -> bool) ->
  ink:Camlcast.Color.t ->
  computed:Camlcast.Color.t ->
  picked:Camlcast.Color.t ->
  selected:hit ->
  item list ->
  Camlcast.P.t
(** The room, over the finished frame.

    [ink] draws what [draggable] accepts and [computed] what it does not — the
    one predicate, made visible: an argument written as a literal can be
    changed and one worked out by a function cannot, so a wall from
    [List.init 12 plaza_corner] is drawn and walkable and plainly not for
    dragging. [picked] marks whatever {!val-hit} last returned.

    [draggable] is asked rather than worked out here, because the answer is
    {!Camlcast_edit.Link}'s and this module knows nothing of source. The
    colours are asked for because this library holds none, for the reason the
    engine holds none. *)
