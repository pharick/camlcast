type t = {
  haze : Color.t;
  fog_distance : float;
  min_brightness : float;
  light : Vec.t;
  ambient : float;
  directional : float;
}

let make ~haze ~fog_distance ~min_brightness ~light ~ambient ~directional =
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
