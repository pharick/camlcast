type t = {
  haze : Color.t;
  fog_distance : float;
  min_brightness : float;
  light : Vec.t;
  ambient : float;
  directional : float;
}

(* The field is documented as a unit vector and {!face_shading} takes its cosine
   without normalising, so the normalisation below is the only thing holding
   that up — and a zero light is exactly what {!Vec.normalize} passes through
   unchanged.

   The length is the one number worth checking, because [Float.hypot] folds
   every bad component into it: a [nan] coordinate gives a [nan] length, and an
   infinite one gives an infinite length whose reciprocal is [0.], so
   {!Vec.normalize} would scale by zero and hand back [nan]s that way instead.
   Both are refused here — the finiteness explicitly, the [nan] by the guard
   being written as the negation of what would pass rather than as an assertion
   of what would fail. *)
let make ~haze ~fog_distance ~min_brightness ~light ~ambient ~directional =
  let l = Vec.length light in
  if not (Float.is_finite l && l > 0.) then
    invalid_arg "Atmosphere.make: the light has no direction";
  {
    haze;
    fog_distance;
    min_brightness;
    light = Vec.normalize light;
    ambient;
    directional;
  }

let default =
  make ~haze:(Color.rgb 24 24 32) ~fog_distance:12. ~min_brightness:0.25
    ~light:(Vec.make (-0.4) (-0.9)) ~ambient:0.6 ~directional:0.4

let fog t distance =
  Float.max t.min_brightness (1. -. (distance /. t.fog_distance))

let face_shading t (normal : Vec.t) =
  t.ambient +. (t.directional *. Float.abs (Vec.dot normal t.light))
