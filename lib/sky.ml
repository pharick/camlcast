(** The open sky drawn where a room has no ceiling ({!Room.ceiling} is
    [None]).

    A sky is an infinitely far backdrop, so its colour depends only on the
    direction it is looked in — the azimuth of the column's ray, and how high up
    the view the pixel sits — never on where the player stands. That is what
    makes it read as sky: it does not slide past as you walk, only wheels around
    as you turn and tilts as you look up and down. *)

let horizon_color = Color.rgb 176 196 222
let zenith_color = Color.rgb 40 62 126
let sun_color = Color.rgb 255 246 216

(* Where the sun sits: a fixed compass direction, and how high up the view it
   rides. [sun_radius] is the size of its soft glow. *)
let sun_azimuth = -0.9
let sun_height = 0.5
let sun_radius = 0.55

(* How fast the gradient climbs from the horizon to the zenith. Screen elevation
   only reaches about 0.4 looking level, so this stretches the gradient to span
   the visible sky rather than leaving it all near the horizon colour. *)
let gradient_scale = 2.2

(** Wrap an angle to [-pi, pi], so azimuths either side of the seam still
    compare by their true angular distance. *)
let wrap a =
  let two_pi = 2. *. Float.pi in
  let a = Float.rem a two_pi in
  if a > Float.pi then a -. two_pi
  else if a < -.Float.pi then a +. two_pi
  else a

(** The sky colour a column looking along [azimuth] shows at screen elevation
    [up] — zero at the horizon, growing towards the top of the view. A vertical
    gradient from the horizon haze up to the deep zenith, with a soft round sun
    added where the view points near it. *)
let color ~azimuth ~up =
  let t = Float.max 0. (Float.min 1. (up *. gradient_scale)) in
  let sky = Color.lerp horizon_color zenith_color t in
  let daz = wrap (azimuth -. sun_azimuth) in
  let dup = up -. sun_height in
  let distance = Float.sqrt ((daz *. daz) +. (dup *. dup)) in
  let glow = Float.max 0. (1. -. (distance /. sun_radius)) in
  Color.lerp sky sun_color (glow *. glow)
