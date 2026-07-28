(** A rigid motion of the flat world: a rotation about the vertical, then a
    translation. What carries a point from one {!Room}'s local coordinates into
    another's.

    Rooms are authored independently, each in its own frame, so two of them may
    sit on top of each other numerically and still be separate places. A
    {!World} link says only that {e this} doorway of one room is {e that}
    doorway of the other; everything else — where the neighbour's walls, floor
    and sprites land relative to the camera — follows from the rigid motion the
    two doorways imply.

    {1 Why cos and sin, not an angle}

    The rotation is stored as its cosine and sine rather than as the angle
    itself. Every use of a transform is an application — to the camera's [pos],
    [dir] and [right], to a wall's endpoints, to a ray — and applying it in this
    form is two multiplies and an add per coordinate. Storing an angle would put
    a [cos] and a [sin] in front of each of those, and deriving the angle in the
    first place would put an [atan2] in front of {!between}, all to hold the
    same two numbers. Composition and inversion are just as direct: the angle
    sum and difference formulae are exactly the products in the implementation.

    Nothing here ever normalises, so a transform stays exactly as accurate as
    the two doorways it came from. That matters because {!Player.through} is
    applied to [dir] and [right] every time the player crosses a threshold: a
    rotation whose [cos] and [sin] satisfy [cos² + sin² = 1] keeps a unit vector
    unit and a perpendicular pair perpendicular, so the camera basis cannot
    drift no matter how many doorways are walked through. *)

type t = private {
  cos : float;  (** cosine of the rotation *)
  sin : float;  (** sine of the rotation *)
  offset : Vec.t;  (** translation applied after the rotation *)
}
(** Private, which is the useful half of abstract here: [cos], [sin] and
    [offset] stay readable — {!Plane.through} reads [offset] per room and the
    tests read all three — while writing one down from outside is closed off.

    That closes exactly the hole worth closing. Every value below preserves
    [cos² + sin² = 1], and the paragraph above is why that matters: the basis is
    carried through {!Player.through} once per doorway crossed, so a rotation
    that was never quite a rotation would let the camera drift a little further
    every time. A hand-written record is the only way to get one, and this is
    the line that says no. Nothing is hidden and nothing costs a call. *)

val identity : t
(** The motion that moves nothing: the frame a room is already in. *)

val direction : t -> Vec.t -> Vec.t
(** Rotate a {e direction} — [dir], [right], a wall normal, a ray. Directions
    carry no position, so the translation does not apply to them; only {!point}
    adds it. *)

val point : t -> Vec.t -> Vec.t
(** Carry a {e position} across: rotate it, then translate. *)

val inverse : t -> t
(** The motion back the other way, so that [point (inverse t) (point t p) = p].
    Undoing [p -> R p + offset] means subtracting the offset before rotating
    back, and [R⁻¹ (p - offset) = R⁻¹ p + R⁻¹ (-offset)] — so the inverse is the
    opposite rotation, which negates [sin] alone, carrying that same opposite
    rotation of the negated offset. *)

val compose : t -> t -> t
(** [compose outer inner] applies [inner] first and then [outer], so that
    [point (compose outer inner) p = point outer (point inner p)] — the motion
    that crosses two doorways in a row. Expanding that composition,
    [R_o (R_i p + o_i) + o_o = (R_o R_i) p + (R_o o_i + o_o)], gives the
    rotation as the product of the two (the angle-sum formulae) and the offset
    as the inner one carried through the outer motion. *)

val between : a1:Vec.t -> a2:Vec.t -> b1:Vec.t -> b2:Vec.t -> t
(** The rigid motion that lays the segment [a1..a2] of one room onto the segment
    [b1..b2] of another, {e endpoints reversed}: [a1] goes to [b2] and [a2] goes
    to [b1]. Both segments must have non-zero length, and the same length if the
    two doorways are to line up.

    A segment with no length is refused here, because there is no rotation that
    lays a point onto a segment and the value that came of trying was one with
    [cos = 0.] and [sin = 0.] — the one way past the invariant the private type
    above exists to hold.

    The {e matching} of the two lengths is not checked here and is not this
    function's business: two segments of different lengths still give a rotation
    that satisfies [cos² + sin² = 1], and merely fail to line the doorways up.
    {!World.make} is where that is refused, along with the heights and the
    doors, because a doorway is what it is a fact about.

    {1 Why the endpoints reverse}

    The two rooms describe the {e same} opening from opposite sides. Walk a
    room's boundary in a consistent direction — the wall loop's winding — and
    the doorway is the gap you pass through along the way; do that in the
    neighbour and you pass through the same gap running the other way. It is the
    half-edge and its twin.

    Concretely: room A occupies [x < 0] with its doorway [(0,0) -> (0,1)]; room
    B, authored on its own, occupies [x > 0] with its doorway also written
    [(0,0) -> (0,1)]. Reversing gives a half-turn plus an offset, which carries
    A's point [(-0.1, 0.5)] — just inside A — to [(0.1, 0.5)], just inside B.
    Mapping the endpoints straight across instead would leave the rotation the
    identity and drop the player back outside B, on the wrong side of its wall.

    So the {b authoring rule} for a {!Room.type-threshold} is: give its
    endpoints in the same winding direction as the room's own boundary walls.

    {1 The derivation}

    With [u] the unit direction of [a1..a2] and [w] the unit direction of the
    {e reversed} target [b2..b1], the rotation wanted is the one taking [u] to
    [w]. Its cosine is the projection of one onto the other and its sine is the
    signed area they span, which is to say [cos = u · w] and [sin = u × w] — no
    angle is formed and no trigonometry is called. The offset is then whatever
    puts the rotated [a1] on [b2]. That the other endpoint follows,
    [R a2 + offset = |a2 - a1| w + b2 = b1], is exactly the equal-length
    condition.

    @raise Invalid_argument if either segment's two ends are the same point. *)
