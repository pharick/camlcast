type t = { x : float; y : float }

let make x y = { x; y }
let add a b = { x = a.x +. b.x; y = a.y +. b.y }
let sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let scale v k = { x = v.x *. k; y = v.y *. k }
let length v = Float.hypot v.x v.y
let dot a b = (a.x *. b.x) +. (a.y *. b.y)
let cross a b = (a.x *. b.y) -. (a.y *. b.x)
let normalizable l = Float.is_finite l && l > 0. && Float.is_finite (1. /. l)

let normalize v =
  let l = length v in
  if l = 0. then v else scale v (1. /. l)

let of_angle angle = { x = cos angle; y = sin angle }

let rotate v angle =
  let c = cos angle and s = sin angle in
  { x = (v.x *. c) -. (v.y *. s); y = (v.x *. s) +. (v.y *. c) }

let perp v = { x = -.v.y; y = v.x }
