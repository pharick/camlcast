(** Immutable 2-D vectors. The whole world is flat: the "3-D" look is an
    illusion produced later, when we decide how tall to draw each wall. *)

type t = { x : float; y : float }
(** Deliberately concrete. This is arithmetic, not an invariant: every module
    below reads [v.x] and [v.y] directly, and there is no state here to protect
    from being built by hand. *)

val make : float -> float -> t
val add : t -> t -> t
val sub : t -> t -> t
val scale : t -> float -> t
val length : t -> float
val dot : t -> t -> float

val cross : t -> t -> float
(** The 2-D cross product, a scalar: [a.x*b.y - a.y*b.x]. It is the signed area
    of the parallelogram [a] and [b] span, and it is zero exactly when they are
    parallel — which is what ray-versus-wall intersection tests. *)

val normalize : t -> t
(** The same vector scaled to unit length; a zero vector is returned unchanged
    rather than turned into [nan]s. *)

val of_angle : float -> t
(** Unit vector pointing at [angle] radians (0 = +x axis, growing clockwise on
    screen because the y axis points down). *)

val rotate : t -> float -> t
(** Rotate by [angle] using the standard rotation matrix:

    {v
      | cos a   -sin a |   | x |
      | sin a    cos a | * | y |
    v} *)

val perp : t -> t
(** Perpendicular vector, i.e. rotated a quarter turn. Cheaper and exact
    compared to [rotate v (pi /. 2.)]. *)
