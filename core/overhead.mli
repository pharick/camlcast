(** A room seen from above, and the two ways between its coordinates and a
    panel's pixels.

    A room is authored in its own flat frame and drawn from inside it. Looking
    at one from over the top — to see a wall pointing the wrong way, or to take
    hold of a corner — means fitting that frame into a rectangle of pixels, and
    that is all this is.

    {1 Why it is a module}

    Because the mapping has an inverse, and the two must agree. A view that
    drew a corner at one place and picked it up at another would be wrong in a
    way nobody could see: the wall would move, just not to where the pointer
    was. One projection with {!to_panel} and {!to_room} derived from the same
    numbers cannot drift; two written separately would, and the day they did
    the symptom would be a drag that lands slightly off.

    {!Camlcast.Debug_map} draws with it and the editor's own view draws and
    hit-tests with it. *)

type t
(** A placed projection: where the panel is, and how much of the room fits. *)

val bounds : Room.t -> (float * float * float * float) option
(** The rectangle a room occupies, as [(x0, y0, x1, y1)], or [None] for a room
    with no boundary and no doorways at all.

    {b Sprites are left out on purpose.} A small one drifting far from the room
    would shrink everything else to fit it in, and what a view from above is
    for is the shape of the place. What is measured is the boundary and the
    openings in it. *)

val fit :
  bounds:float * float * float * float ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  inset:int ->
  t
(** A projection putting [bounds] in the middle of this rectangle, [inset]
    pixels in from its edge.

    Centred on the room's own middle rather than on its origin, so a long thin
    room sits in the panel instead of in one corner of it. One scale for both
    axes, so it is not stretched.

    The scale is taken from the width alone. A panel is square wherever one is
    drawn here, and taking the smaller of the two would make a room that is
    wider than it is tall leave the height unused for no gain. *)

val to_panel : t -> Vec.t -> int * int
(** Where a point in the room lands on the panel.

    World [y] grows downward and so does the screen's, so this is a scale and
    an offset and never a flip. *)

val to_room : t -> int * int -> Vec.t
(** Where a pixel on the panel lands in the room: {!to_panel} run backwards.

    Exact where {!to_panel} is not — a pixel is a whole number and a point is
    not, so a point put through both comes back within a pixel's worth of world
    units rather than unchanged. That tolerance is the panel's resolution and
    nothing more, which is what makes dragging feel like dragging. *)

val scale : t -> float
(** Pixels per world unit, for anything that has to size itself in the room's
    terms rather than the panel's — a tick that should be the same length on
    every wall, a corner's grab radius. *)
