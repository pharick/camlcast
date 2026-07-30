(** Casting a ray against the wall segments of a {!Room}.

    A grid raycaster steps a ray from cell to cell (the DDA). With arbitrary
    wall segments there is no grid to step through, so instead the ray is
    intersected with each wall directly and the ones it actually crosses are
    kept. Write the ray as [origin + t*direction], [t >= 0], and a wall as
    [a + s*edge] with [edge = b - a] and [s] in [0, 1]; solving with the 2-D
    cross product (see {!Vec.cross}) gives [t] and [s], and the ray meets the
    wall when they are not parallel, [t > 0], and [s] lies in [0, 1].

    {b Why the distance has no fish-eye.} [direction] is deliberately {e not}
    normalised: {!Viewport.ray_direction} builds it as [dir + right * k] with
    [dir] the unit view direction. Because [t] is measured in units of
    [direction], projecting the hit onto [dir] leaves exactly [t] — so [t] is
    the distance perpendicular to the camera plane, which is what the
    projection needs and what removes the fish-eye bulge.

    {b Seeing past a wall.} Walls have different heights and the floor and
    ceiling are sloped, so a near wall does not necessarily hide what is
    behind it. The cast therefore keeps {e every} wall the ray crosses and
    returns them farthest-first, ready for {!Renderer} to paint back to
    front. *)

type hit = {
  distance : float;  (** perpendicular distance from the camera plane *)
  along : float;
      (** world distance from the wall's start [a] to the hit, for texturing *)
  wall : Room.wall;
  index : int;
      (** which of the room's walls it was. The wall itself is here for the
          renderer, which wants its material and its geometry; the index is for
          anything that has to name the wall afterwards — an index survives a
          {!World.replace_room} where a copy of the wall would go stale. *)
}

type opening = { distance : float; along : float; index : int }
(** A threshold the ray crossed, on the same terms as a {!type-hit} — [index]
    into the room's thresholds rather than its walls. *)

val min_distance : float
(** The distance floor a hit is refused under, so a player standing on a wall
    cannot divide by zero when the hit is turned into a wall height. *)

val cast : Room.t -> origin:Vec.t -> direction:Vec.t -> hit list
(** Every wall the ray crosses, farthest first — the order the painter's-
    algorithm renderer draws in. *)

val openings : Room.t -> origin:Vec.t -> direction:Vec.t -> opening list
(** Every threshold the ray crosses, farthest first, on the same intersection
    test as {!cast}. *)

(** One thing a ray met in a room, of whichever kind. Walls and doorways are
    found by two separate passes but have to be dealt with in one order, since
    each can stand in front of the other. *)
type step = Wall of hit | Opening of opening

val step_distance : step -> float
(** How far away that thing was, whichever kind it is. *)

val merge : hit list -> opening list -> step list
(** Both lists arrive farthest-first, so one merge puts walls and thresholds
    into a single far-to-near stream without sorting either of them again.

    Far-to-near is the renderer's order — it paints back to front — and the
    reverse of it is what anything asking "what is the first thing out there"
    wants. Both read this. *)

val nearest : hit list -> hit option
(** The closest wall along the ray, if it met one — the wall a solid-height
    caster would have stopped at. *)
