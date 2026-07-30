type t = {
  horizon : Color.t;
  zenith : Color.t;
  sun : Color.t;
  sun_azimuth : float;
  sun_height : float;
  sun_radius : float;
  gradient : float;
}

let make ?(horizon = Color.rgb 176 196 222) ?(zenith = Color.rgb 40 62 126)
    ?(sun = Color.rgb 255 246 216) ?(sun_azimuth = -0.9) ?(sun_height = 0.5)
    ?(sun_radius = 0.55) ?(gradient = 2.2) () =
  (* Negated, so a nan fails rather than slipping through: sun_radius is the
     divisor in {!color}'s glow, and the other two enter every sky pixel. *)
  if not (Float.is_finite sun_radius && sun_radius > 0.) then
    invalid_arg "Sky.make: sun_radius has to be a positive size";
  if not (Float.is_finite gradient && gradient >= 0.) then
    invalid_arg "Sky.make: gradient has to be zero or above";
  if not (Float.is_finite sun_height) then
    invalid_arg "Sky.make: sun_height has to be finite";
  if not (Float.is_finite sun_azimuth) then
    invalid_arg "Sky.make: sun_azimuth has to be finite";
  { horizon; zenith; sun; sun_azimuth; sun_height; sun_radius; gradient }

let default = make ()

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
