(** An inclined plane [z = a*x + b*y + c] that gives the height of the floor or
    the ceiling above any point of the flat world.

    The world itself stays two-dimensional — walls are still {!Vec} segments in
    the ground plane. All a plane adds is a height at each point, so the floor
    and ceiling no longer have to be horizontal: set [a] or [b] non-zero and the
    surface tilts. A horizontal plane is simply [a = b = 0]. *)

type t = { a : float; b : float; c : float }
(** Three coefficients and nothing derived from them, so there is no invariant
    to protect and the record stays readable. {!Renderer} reads them per pixel.
*)

val make : a:float -> b:float -> c:float -> t
(** The plane [z = a*x + b*y + c], every number in cells. [a] and [b] are the
    rise per cell travelled along each axis, so both zero is horizontal; [c]
    is the height over the origin. Nothing is checked or normalised — any
    three floats are a plane. *)

val horizontal : float -> t
(** [horizontal z] is the flat plane at constant height [z] cells, which is
    [make ~a:0. ~b:0. ~c:z]. *)

val elevation : t -> Vec.t -> float
(** [elevation plane p] is the height of [plane] over the ground point [p] — the
    [z] of its defining equation. Defined everywhere; a point outside any room
    still has a height. *)

val gradient : t -> Vec.t -> float
(** [gradient plane dir] is how fast [plane] rises per unit travelled along
    [dir]. [dir] need not be a unit vector: it is used with the camera ray,
    whose length already encodes distance, and the result scales with it in step
    — so this is rise per [dir], not rise per unit distance. *)

val above : t -> float -> t
(** [above plane height] is the plane parallel to [plane] and [height] cells
    above it — a ceiling that follows the slope of the floor it roofs,
    rather than closing in on it at one end. A negative [height] puts it below.
    Only [c] moves; the two gradients stay equal, which is what parallel means
    here. *)

val through : Transform.t -> t -> t
(** [through m plane] is [plane] expressed in a neighbouring {!Room}'s frame,
    where [m] is the rigid motion carrying this room's coordinates into the
    neighbour's. This is the plane the room reached through [m] must use if the
    two are to agree — in particular across the doorway they share, where a
    disagreement shows as a visible step in the floor.

    A point [p] here lands at [q = R p + offset] there, and we want the two
    heights to match, [elevation result q = elevation plane p]. Substituting
    [p = R⁻¹ (q - offset)] and using that a rotation moves a gradient the same
    way it moves any direction — [g · R⁻¹ v = (R g) · v] — the neighbour's
    gradient is this one rotated, and its constant absorbs the translation:

    {v
      elevation plane p = g · R⁻¹ (q - offset) + c
                        = (R g) · q  -  (R g) · offset  +  c
    v} *)

val view_distance :
  t ->
  eye_z:float ->
  eye_pos:Vec.t ->
  dir:Vec.t ->
  row_factor:float ->
  float option
(** [view_distance plane ~eye_z ~eye_pos ~dir ~row_factor] is the perpendicular
    distance at which one pixel's line of sight meets [plane], or [None] if that
    line runs parallel to the plane or only meets it behind the camera. The eye
    is at ground position [eye_pos], height [eye_z].

    A pixel in a column of horizontal direction [dir] sees, at perpendicular
    distance [d], the world point [eye_pos + d*dir] at height
    [eye_z - row_factor*d], where [row_factor = (row - horizon) / projection]
    grows as the pixel moves down the screen. Setting that height equal to the
    plane's own height there — [base + d*gradient], with [base] the plane's
    height under the eye — and solving for [d]:

    {v
      eye_z - row_factor*d = base + d*gradient
      eye_z - base         = d*(row_factor + gradient)
      d                    = (eye_z - base) / (row_factor + gradient)
    v}

    One formula serves both surfaces. For the floor the eye is above it
    ([eye_z > base]) and the line of sight must slope down to reach it; for the
    ceiling the eye is below it and the line must slope up. Either way the real
    surface is the one at a positive distance, so anything else is rejected. *)
