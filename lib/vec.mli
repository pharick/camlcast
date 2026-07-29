(** Immutable 2-D vectors. The whole world is flat: the "3-D" look is an
    illusion produced later, when we decide how tall to draw each wall. *)

type t = { x : float; y : float }
(** Deliberately concrete. This is arithmetic, not an invariant: every module
    below reads [v.x] and [v.y] directly, and there is no state here to protect
    from being built by hand. *)

val make : float -> float -> t
(** [make x y] is that vector, [x] across and [y] along. The y axis points
    {e down} the screen, which is what makes a positive angle turn clockwise —
    see {!of_angle}. *)

val add : t -> t -> t
(** [add a b] is the componentwise sum. *)

val sub : t -> t -> t
(** [sub a b] is [a] minus [b], so it points from [b] towards [a] — the way
    round a displacement between two positions is usually wanted. *)

val scale : t -> float -> t
(** [scale v factor] stretches [v] by [factor], keeping its direction. A
    negative factor reverses it. *)

val length : t -> float
(** The Euclidean length, never negative. *)

val dot : t -> t -> float
(** The dot product [a.x*b.x + a.y*b.y]. For unit vectors it is the cosine of
    the angle between them: positive when they point the same way, zero when
    they are perpendicular, negative when opposed — which is how a surface is
    shaded by how squarely it faces the light. *)

val cross : t -> t -> float
(** [cross a b] is the 2-D cross product, a scalar: [a.x*b.y - a.y*b.x]. It is
    the signed area of the parallelogram [a] and [b] span, and it is zero
    exactly when they are parallel — which is what ray-versus-wall intersection
    tests. *)

val normalize : t -> t
(** The same vector scaled to unit length; a zero vector is returned unchanged
    rather than turned into [nan]s.

    That guard is for the zero vector and nothing else. A coordinate that is
    [nan] or infinite still comes back as [nan]s — the infinite one because its
    length is infinite too, and scaling by that reciprocal is [x *. 0.]. This
    module is plain arithmetic with no constructor to check anything in, so the
    refusing is done where a direction is first promised to be one:
    {!Atmosphere.make} and {!Transform.between} both reject a length that is not
    finite and positive before it reaches here. *)

val of_angle : float -> t
(** [of_angle radians] is the unit vector pointing that way: [0.] is the +x
    axis, and a growing angle turns clockwise on screen because the y axis
    points down. *)

val rotate : t -> float -> t
(** [rotate v radians] turns [v] by that angle, clockwise on screen for a
    positive one, keeping its length. The standard rotation matrix:

    {v
      | cos a   -sin a |   | x |
      | sin a    cos a | * | y |
    v} *)

val perp : t -> t
(** The perpendicular vector, a quarter turn clockwise on screen — the same
    direction {!rotate} would give for [pi /. 2.], but cheaper and exact. *)
