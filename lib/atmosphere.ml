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
   unchanged. Negated, so a [nan] is refused with it. *)
let make ~haze ~fog_distance ~min_brightness ~light ~ambient ~directional =
  if not (Vec.length light > 0.) then
    invalid_arg "Atmosphere.make: the light has no direction";
  {
    haze;
    fog_distance;
    min_brightness;
    light = Vec.normalize light;
    ambient;
    directional;
  }

let fog t distance =
  Float.max t.min_brightness (1. -. (distance /. t.fog_distance))

let face_shading t (normal : Vec.t) =
  t.ambient +. (t.directional *. Float.abs (Vec.dot normal t.light))
