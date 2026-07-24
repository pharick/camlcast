(** An inclined plane [z = a*x + b*y + c] that gives the height of the floor or
    the ceiling above any point of the flat world.

    The world itself stays two-dimensional — walls are still {!Vec} segments in
    the ground plane. All a plane adds is a height at each point, so the floor
    and ceiling no longer have to be horizontal: set [a] or [b] non-zero and the
    surface tilts. A horizontal plane is simply [a = b = 0]. *)

type t = { a : float; b : float; c : float }

let make ~a ~b ~c = { a; b; c }

(** A flat plane at constant height [z]. *)
let horizontal z = { a = 0.; b = 0.; c = z }

(** The height of the plane above the point [p]. *)
let elevation t (p : Vec.t) = (t.a *. p.x) +. (t.b *. p.y) +. t.c

(** How fast the plane rises per unit travelled along [dir]. [dir] need not be a
    unit vector: it is used with the camera ray, whose length already encodes
    distance, and the gradient scales with it in step. *)
let gradient t (dir : Vec.t) = (t.a *. dir.x) +. (t.b *. dir.y)

(** The perpendicular distance at which a pixel's line of sight meets the plane,
    or [None] if that line runs parallel to the plane or only meets it behind
    the camera.

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
let view_distance t ~eye_z ~eye_pos ~dir ~row_factor =
  let denom = row_factor +. gradient t dir in
  if Float.abs denom < 1e-9 then None
  else
    let d = (eye_z -. elevation t eye_pos) /. denom in
    if d > 0. then Some d else None
