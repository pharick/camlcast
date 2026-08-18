(** An inclined plane [z = a*x + b*y + c] giving the height of the floor or
    ceiling above any point of the flat world.

    The world itself stays two-dimensional — walls are still {!Vec} segments in
    the ground plane. A plane only adds a height at each point, so the floor and
    ceiling need not be horizontal: set [a] or [b] non-zero and the surface
    tilts. A horizontal plane is [a = b = 0]. *)

type t = { a : float; b : float; c : float }
(** Three coefficients with nothing derived from them, so there is no invariant
    to protect and the record stays readable. {!Renderer} reads them per pixel.
*)

val make : a:float -> b:float -> c:float -> t
(** The plane [z = a*x + b*y + c], every number in cells. [a] and [b] are the
    rise per cell travelled along each axis, so both zero is horizontal; [c] is
    the height over the origin. Nothing is checked or normalised — any three
    floats are a plane. *)

val horizontal : float -> t
(** [horizontal z] is the flat plane at constant height [z] cells:
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
    above it — a ceiling that follows the slope of the floor it roofs, rather
    than converging on it at one end. A negative [height] puts it below. Only
    [c] changes; the two gradients stay equal, which is what parallel means
    here. *)

val through : Transform.t -> t -> t
(** [through m plane] is [plane] expressed in a neighbouring {!Room}'s frame,
    where [m] is the rigid motion carrying this room's coordinates into the
    neighbour's. The room reached through [m] must use this plane if the two are
    to agree — in particular across the doorway they share, where a disagreement
    shows as a visible step in the floor.

    A point [p] here lands at [q = R p + offset] there, and the two heights must
    match: [elevation result q = elevation plane p]. Substituting
    [p = R⁻¹ (q - offset)] and using that a rotation moves a gradient the same
    way it moves any direction — [g · R⁻¹ v = (R g) · v] — the neighbour's
    gradient is this one rotated, and its constant absorbs the translation:

    {v
      elevation plane p = g · R⁻¹ (q - offset) + c
                        = (R g) · q  -  (R g) · offset  +  c
    v} *)

val parallel : float
(** The denominator below which a line of sight counts as running along a plane
    rather than meeting it; {!cast} then answers [infinity].

    Unscaled, unlike {!Vec.parallel}: [row_factor + gradient] is already
    dimensionless, being a screen row's slope plus the plane's, so there is
    nothing to divide out and the figure compares to it directly.

    Public because {!Renderer} asks the same question. It calls {!cast} for two
    planes per pixel and tests the sign of that same denominator itself, to know
    which of the floor and ceiling a row could be showing; its three guards used
    to write [1e-9] out literally, correct only by inspection.

    The risk of divergence is small but worth stating exactly. A guard
    {e narrower} than this costs nothing: the row reaches {!cast}, comes back
    [infinity], and is left to the haze, which is where the guard would have
    sent it. A guard {e wider} does change the picture — the row is refused
    where {!cast} would have answered with an enormous finite distance, and an
    enormous distance is a fogged floor texel rather than the haze itself. That
    affects at most one row, and only where [row_factor + gradient] lands
    between the two figures, which for an authored slope is a coincidence on the
    order of the corner tie. So this is shared because it is one question asked
    twice and a reader should not have to work out which way the difference
    falls — not because any drawn frame was wrong. Widening the guard to [1e-4]
    fails no test in this repository. *)

val cast :
  eye_z:float -> base:float -> gradient:float -> row_factor:float -> float
(** The cast itself, with everything constant down a column already hoisted:
    [base] is the plane's height under the eye ([elevation plane eye_pos]) and
    [gradient] its rise per [dir] ([gradient plane dir]). The answer is
    [(eye_z - base) / (row_factor + gradient)], or [infinity] where that
    denominator vanishes and the line of sight runs parallel to the plane.

    A raw float, not an option, and no judgement about which side of the eye the
    answer falls on. Both are the caller's, and the two callers want different
    things: {!view_distance} below wants only the surface in front of the eye,
    while {!Renderer} wants a value it can compare a floor's against a ceiling's
    with [<=], for which [infinity] is the useful absence and an option is a box
    opened twice per pixel.

    This shape — hoisted, unboxed, unjudged — is why the function exists. The
    renderer casts two planes for every pixel of every background, and the
    arithmetic is small enough that a function call around it costs more than
    the arithmetic. That is an argument for writing it out at each of the three
    places that need it, and the price of doing so is the engine's central
    formula in as many copies as there are callers. Marked [[@inline always]],
    it is one copy and costs nothing; see the note in the implementation for the
    measurements. *)

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
    surface is the one at a positive distance, so anything else is rejected.

    This is {!cast} with the hoisting done and that final judgement applied —
    the complete statement, for a caller holding a plane and a pose rather than
    a loop. {!Renderer} is not that caller and uses {!cast}; what it adds
    instead is a test on the sign of the denominator, because it asks the
    question of two planes at once and must know which of them a row could be
    showing. Answering role-blind, this returns whichever plane the ray really
    meets — including a plane a game has put on the wrong side of the eye, where
    the renderer, having asked about a {e ceiling}, declines to paint one below
    the horizon. *)
