type t = {
  horizon : Color.t;
  zenith : Color.t;
  sun : Color.t;
  sun_azimuth : float;
  sun_height : float;
  sun_radius : float;
  gradient : float;
}

let default =
  {
    horizon = Color.rgb 176 196 222;
    zenith = Color.rgb 40 62 126;
    sun = Color.rgb 255 246 216;
    sun_azimuth = -0.9;
    sun_height = 0.5;
    sun_radius = 0.55;
    gradient = 2.2;
  }

let wrap a =
  let two_pi = 2. *. Float.pi in
  let a = Float.rem a two_pi in
  if a > Float.pi then a -. two_pi
  else if a < -.Float.pi then a +. two_pi
  else a

let color t ~azimuth ~up =
  let g = Float.max 0. (Float.min 1. (up *. t.gradient)) in
  let sky = Color.lerp t.horizon t.zenith g in
  let daz = wrap (azimuth -. t.sun_azimuth) in
  let dup = up -. t.sun_height in
  let distance = Float.sqrt ((daz *. daz) +. (dup *. dup)) in
  let glow = Float.max 0. (1. -. (distance /. t.sun_radius)) in
  Color.lerp sky t.sun (glow *. glow)
