(** Casts a ray against the wall segments of a {!Room}.

    A grid raycaster steps a ray from cell to cell (the DDA). With arbitrary
    wall segments there is no grid to step through, so the ray is intersected
    with each wall directly and the walls it actually crosses are kept. Write
    the ray as [origin + t*direction], [t >= 0], and a wall as [a + s*edge] with
    [edge = b - a] and [s] in [0, 1]. Solving with the 2-D cross product (see
    {!Vec.cross}) gives [t] and [s]. The ray meets the wall when the two are not
    parallel, [t > 0], and [s] lies in [0, 1].

    {b Why the distance has no fish-eye.} [direction] is deliberately {e not}
    normalised: {!Viewport.ray_direction} builds it as [dir + right * k] with
    [dir] the unit view direction. Because [t] is measured in units of
    [direction], projecting the hit onto [dir] gives exactly [t]. So [t] is the
    distance perpendicular to the camera plane, which is what the projection
    needs and what removes the fish-eye bulge.

    {b Seeing past a wall.} Walls have different heights and the floor and
    ceiling are sloped, so a near wall does not necessarily hide what is behind
    it. The cast therefore keeps {e every} wall the ray crosses and returns them
    farthest-first, the order {!Renderer} paints in (back to front). *)

type hit = {
  distance : float;  (** perpendicular distance from the camera plane *)
  along : float;
      (** world distance from the wall's start [a] to the hit, for texturing *)
  wall : Room.wall;
  index : int;
      (** index of the wall in the room. The wall itself is included for the
          renderer, which needs its material and geometry; the index is for
          anything that must name the wall afterwards — an index survives a
          {!World.replace_room} where a copy of the wall would go stale. *)
}

type opening = { distance : float; along : float; index : int }
(** A threshold the ray crossed, in the same terms as a {!type-hit}, with
    [index] into the room's thresholds rather than its walls. *)

val min_distance : float
(** Minimum distance below which a hit is refused. A player standing exactly on
    a wall would otherwise divide by zero when the hit is turned into a wall
    height. *)

val cast : Room.t -> origin:Vec.t -> direction:Vec.t -> hit list
(** Every wall the ray crosses, farthest first — the order the
    painter's-algorithm renderer draws in. *)

val openings : Room.t -> origin:Vec.t -> direction:Vec.t -> opening list
(** Every threshold the ray crosses, farthest first, using the same intersection
    test as {!cast}. *)

(** One thing a ray met in a room, of either kind. Walls and doorways are found
    by two separate passes but must be processed in one order, since each can
    stand in front of the other. *)
type step = Wall of hit | Opening of opening

val step_distance : step -> float
(** The distance of a step, regardless of its kind. *)

val merge : hit list -> opening list -> step list
(** Merges walls and thresholds into a single far-to-near stream. Both lists
    arrive farthest-first, so a single merge suffices without re-sorting either.

    Far-to-near is the renderer's order (it paints back to front); the reverse
    is what a nearest-first query wants. Both consume this. *)

val nearest : hit list -> hit option
(** The closest wall along the ray, if any — the wall a solid-height caster
    would have stopped at.

    The engine itself never asks this: {!Renderer} paints the whole list and
    {!Sight} reads it from the near end, and both need what stands behind the
    first wall. That need comes from walls of differing heights over a sloped
    floor, and is the reason {!cast} returns a list at all. [nearest] is for
    callers with the simpler question — a line of sight between two points, a
    minimap ray, a tool asking what a direction runs into — where one wall is
    the whole answer. The full hit list is still computed; [nearest] discards
    the rest rather than short-circuiting. *)
