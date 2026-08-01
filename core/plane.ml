type t = { a : float; b : float; c : float }

let make ~a ~b ~c = { a; b; c }
let horizontal z = { a = 0.; b = 0.; c = z }
let elevation t (p : Vec.t) = (t.a *. p.x) +. (t.b *. p.y) +. t.c
let gradient t (dir : Vec.t) = (t.a *. dir.x) +. (t.b *. dir.y)
let above t height = { t with c = t.c +. height }

let through m t =
  let g = Transform.direction m (Vec.make t.a t.b) in
  { a = g.Vec.x; b = g.Vec.y; c = t.c -. Vec.dot g m.Transform.offset }

let view_distance t ~eye_z ~eye_pos ~dir ~row_factor =
  let denom = row_factor +. gradient t dir in
  if Float.abs denom < 1e-9 then None
  else
    let d = (eye_z -. elevation t eye_pos) /. denom in
    if d > 0. then Some d else None
