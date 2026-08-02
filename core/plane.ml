type t = { a : float; b : float; c : float }

let make ~a ~b ~c = { a; b; c }
let horizontal z = { a = 0.; b = 0.; c = z }
let elevation t (p : Vec.t) = (t.a *. p.x) +. (t.b *. p.y) +. t.c
let gradient t (dir : Vec.t) = (t.a *. dir.x) +. (t.b *. dir.y)
let above t height = { t with c = t.c +. height }

let through m t =
  let g = Transform.direction m (Vec.make t.a t.b) in
  { a = g.Vec.x; b = g.Vec.y; c = t.c -. Vec.dot g m.Transform.offset }

(* [@inline always] and not merely small. This is called once per pixel of every
   background, from another module, and the compiler here is not flambda: left
   to itself it emits a call, which boxes four floats on the way in and one on
   the way out. Measured over a 512x384 buffer that is 2.1x the cost of the same
   arithmetic written where it is used, and the whole reason the renderer had
   its own copy of this to begin with. Annotated, the difference is 1.1x on the
   cast and nothing at all on the frame. *)
let[@inline always] cast ~eye_z ~base ~gradient ~row_factor =
  let denom = row_factor +. gradient in
  if Float.abs denom < 1e-9 then infinity else (eye_z -. base) /. denom

let view_distance t ~eye_z ~eye_pos ~dir ~row_factor =
  let d =
    cast ~eye_z ~base:(elevation t eye_pos) ~gradient:(gradient t dir)
      ~row_factor
  in
  if Float.is_finite d && d > 0. then Some d else None
