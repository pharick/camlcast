(** The air a {!World} is seen through: how fast it fades things out, what
    colour it fades them to, and where its light comes from.

    These used to be constants — two in {!Config} and three more in the palette
    — which meant every world was lit and fogged the same way. They are a value
    here because they are most of what tells one place from another. A sunlit
    courtyard and a windowless corridor can be built from the same geometry and
    the same materials; what makes the corridor read as a corridor is that its
    fog closes in at nine cells instead of twelve, fades to black instead of
    grey, and has no discernible direction to its light. *)

type t = {
  haze : Color.t;
      (** what fills a doorway the portal recursion could not reach, and the
          band where the eye looks past both the floor and the ceiling *)
  fog_distance : float;  (** where the fade has fully arrived *)
  min_brightness : float;  (** the floor of the fade *)
  light : Vec.t;  (** the fixed direction surfaces are lit from, a unit vector *)
  ambient : float;  (** the brightness a surface has facing away from it *)
  directional : float;  (** how much more it has facing squarely into it *)
}

(** [light] need not arrive normalised; it is only ever used for the cosine
    against a wall's normal, so it is normalised here once. *)
let make ~haze ~fog_distance ~min_brightness ~light ~ambient ~directional =
  {
    haze;
    fog_distance;
    min_brightness;
    light = Vec.normalize light;
    ambient;
    directional;
  }

(** Linear distance fog: full colour up close, [min_brightness] at and beyond
    [fog_distance]. On the floor and ceiling it doubles as a horizon haze,
    fading the planes out into the distance instead of letting them run to a
    hard edge. *)
let fog t distance =
  Float.max t.min_brightness (1. -. (distance /. t.fog_distance))

(** How brightly a surface of a given orientation catches the light. With walls
    at every angle a fixed east/west versus north/south rule no longer works, so
    the brightness follows how squarely the surface's [normal] faces the light.
    [Float.abs] means both faces of a wall light the same.

    The band is [ambient .. ambient + directional] and never reaches zero, so no
    wall falls to black on orientation alone. Narrowing it to almost nothing is
    how a place is made to look as though it has no light source at all: every
    wall the same brightness whichever way it faces, and only distance to tell
    them apart. *)
let face_shading t (normal : Vec.t) =
  t.ambient +. (t.directional *. Float.abs (Vec.dot normal t.light))
