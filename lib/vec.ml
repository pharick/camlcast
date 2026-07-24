(** Immutable 2-D vectors. The whole world is flat: the "3-D" look is an
    illusion produced later, when we decide how tall to draw each wall. *)

type t = { x : float; y : float }

let make x y = { x; y }
let add a b = { x = a.x +. b.x; y = a.y +. b.y }
let sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let scale v k = { x = v.x *. k; y = v.y *. k }
let length v = Float.hypot v.x v.y
let dot a b = (a.x *. b.x) +. (a.y *. b.y)

(** The 2-D cross product, a scalar: [a.x*b.y - a.y*b.x]. It is the signed area
    of the parallelogram [a] and [b] span, and it is zero exactly when they are
    parallel — which is what ray-versus-wall intersection tests. *)
let cross a b = (a.x *. b.y) -. (a.y *. b.x)

(** The same vector scaled to unit length; a zero vector is returned unchanged
    rather than turned into [nan]s. *)
let normalize v =
  let l = length v in
  if l = 0. then v else scale v (1. /. l)

(** Unit vector pointing at [angle] radians (0 = +x axis, growing clockwise on
    screen because the y axis points down). *)
let of_angle angle = { x = cos angle; y = sin angle }

(** Rotate by [angle] using the standard rotation matrix:

    {v
      | cos a   -sin a |   | x |
      | sin a    cos a | * | y |
    v} *)
let rotate v angle =
  let c = cos angle and s = sin angle in
  { x = (v.x *. c) -. (v.y *. s); y = (v.x *. s) +. (v.y *. c) }

(** Perpendicular vector, i.e. rotated a quarter turn. Cheaper and exact
    compared to [rotate v (pi /. 2.)]. *)
let perp v = { x = -.v.y; y = v.x }
