(** Immutable 2-D vectors. The whole world is flat: the "3-D" look is produced
    later, when the renderer decides how tall to draw each wall. *)

type t = { x : float; y : float }
(** Deliberately concrete. This is arithmetic, not an invariant: every module
    below reads [v.x] and [v.y] directly, and there is no state to protect from
    hand construction. *)

val make : float -> float -> t
(** [make x y] is that vector, [x] across and [y] along. The y axis points
    {e down} the screen, which is what makes a positive angle turn clockwise —
    see {!of_angle}. *)

val add : t -> t -> t
(** [add a b] is the componentwise sum. *)

val sub : t -> t -> t
(** [sub a b] is [a] minus [b], so it points from [b] towards [a] — the usual
    direction wanted for a displacement between two positions. *)

val scale : t -> float -> t
(** [scale v factor] stretches [v] by [factor], keeping its direction. A
    negative factor reverses it. *)

val length : t -> float
(** The Euclidean length, never negative. *)

val dot : t -> t -> float
(** The dot product [a.x*b.x + a.y*b.y]. For unit vectors it is the cosine of
    the angle between them: positive when they point the same way, zero when
    perpendicular, negative when opposed. This is how a surface is shaded by how
    squarely it faces the light. *)

val cross : t -> t -> float
(** [cross a b] is the 2-D cross product, a scalar: [a.x*b.y - a.y*b.x]. It is
    the signed area of the parallelogram spanned by [a] and [b], and it is zero
    exactly when they are parallel — the test ray-versus-wall intersection
    relies on. *)

val parallel : float
(** The sine below which two directions count as parallel, for a caller that has
    a {!cross} of them and both their lengths.

    {b Use it as [Float.abs (cross a b) <= parallel *. length a *. length b],}
    not against the bare cross product. The cross product is [|a| |b| sin θ] —
    an area, not an angle — so a fixed threshold tested against it reads as a
    parallel test but behaves as a length test: two directions at a perfectly
    good angle fail it merely for being short. Dividing through by both lengths
    leaves the sine, and the test then means what it appears to mean. Neither
    length costs anything at the two call sites: {!Room.type-wall} computes its
    edge's length once and keeps it, and a ray's is one square root per column
    rather than one per wall.

    Those two call sites are why the constant lives here rather than in either
    of them. {!Ray.cast} decides which walls a ray crosses and
    {!Room.segments_cross} decides whether a step crosses one; if the two
    answered differently, the result would be a doorway the renderer draws but
    the player cannot walk through, or a wall the player walks into but cannot
    see. A copy in each, with a comment in each saying it is the other's, leaves
    two comments holding the two equal; shared, the figure cannot drift at all.
    The two still differ in the comparison — the ray's test is
    strict and {!Room.segments_cross}'s inclusive, so that a segment of no
    length lands where each function needs it — and that difference is each
    function's own. The figure is not.

    Not in {!Config}: this is not a tunable. It marks where the cross product
    stops carrying an angle and starts carrying rounding error, and a game that
    moved it would get a room whose corners leak. *)

val normalizable : float -> bool
(** Whether a length is one {!normalize} can turn into a unit vector: finite,
    above zero, and — the part that is easy to miss — not so small that [1.]
    divided by it overflows.

    That last gap is real. Somewhere around [5.6e-309] the reciprocal stops
    being finite, and below that, scaling a vector by the resulting [infinity]
    gives [infinity] on the long axis and [nan] on the zero one. So a length
    like [1e-320] is finite and positive and passes any test written as
    [Float.is_finite l && l > 0.] — and {!normalize} still returns
    [(infinity, nan)].

    The reciprocal is computed rather than compared against a constant because
    no tidy constant exists. [1. /. Float.max_float] is the obvious candidate
    and is wrong: it is subnormal, so it carries too few bits to invert back,
    and its own reciprocal rounds past [Float.max_float] and overflows. Asking
    the actual question costs one division and cannot be off by one.

    Every caller that promises a direction must ask this, so it is asked in one
    place. Written as what {e passes}, like the rest of the engine's guards, so
    a caller writes [not (Vec.normalizable l)] to refuse, and a [nan] length is
    refused along with everything else. *)

val normalize : t -> t
(** The same vector scaled to unit length; a zero vector is returned unchanged
    rather than turned into [nan]s.

    That guard covers the zero vector and nothing else. A coordinate that is
    [nan] or infinite still comes back as [nan]s — the infinite one because its
    length is infinite too, so scaling by the reciprocal is [x *. 0.] — and so
    does a length too small to take a reciprocal; see {!normalizable}. This
    module is plain arithmetic with no constructor to check anything in, so
    refusal happens where a direction is first promised to be one:
    {!Atmosphere.make}, {!Transform.between} and the {!Room} constructors all
    put their length through {!normalizable} before it reaches here. *)

val of_angle : float -> t
(** [of_angle radians] is the unit vector at that angle: [0.] is the +x axis,
    and a growing angle turns clockwise on screen because the y axis points
    down. *)

val rotate : t -> float -> t
(** [rotate v radians] turns [v] by that angle, clockwise on screen for a
    positive one, keeping its length. The standard rotation matrix:

    {v
      | cos a   -sin a |   | x |
      | sin a    cos a | * | y |
    v} *)

val perp : t -> t
(** The perpendicular vector, a quarter turn clockwise on screen — the same
    direction {!rotate} gives for [pi /. 2.], but cheaper and exact. On paper,
    with y up, the same turn reads as a quarter turn to the {e left}; the
    winding rule that follows from that is stated once, at the top of {!Room}.
*)
