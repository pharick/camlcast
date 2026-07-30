(** The air a {!World} is seen through: how fast it fades things out, what
    colour it fades them to, and where its light comes from.

    These used to be constants — two in {!Config} and three more in the palette
    — which meant every world was lit and fogged the same way. They are a value
    here because they are most of what tells one place from another. A sunlit
    courtyard and a windowless corridor can be built from the same geometry and
    the same materials; what makes the corridor read as a corridor is that its
    fog closes in at nine cells instead of twelve, fades to black instead of
    grey, and has no discernible direction to its light. *)

type t = private {
  haze : Color.t;
      (** what fills a doorway the portal recursion could not reach, and the
          band where the eye looks past both the floor and the ceiling *)
  fog_distance : float;  (** where the fade has fully arrived *)
  min_brightness : float;  (** the floor of the fade *)
  light : Vec.t;
      (** the fixed direction surfaces are lit from, a unit vector *)
  ambient : float;  (** the brightness a surface has facing away from it *)
  directional : float;  (** how much more it has facing squarely into it *)
}
(** Private: read every field, but build one with {!make}.

    [light] is the reason. It is documented as a unit vector and {!face_shading}
    takes its cosine against a wall normal without normalising either, so a
    light that was never normalised does not fail — it silently scales every
    surface in the world by its length. {!make} normalises once; a hand-written
    record was the only way past it. *)

val make :
  haze:Color.t ->
  fog_distance:float ->
  min_brightness:float ->
  light:Vec.t ->
  ambient:float ->
  directional:float ->
  t
(** The air with those six properties. [haze] is the colour distance fades to,
    [fog_distance] how many cells it takes to get there, and [min_brightness]
    the floor the fade stops at. [light] is the direction it comes from, and
    [ambient] and [directional] the two ends of the band a surface is lit across
    — see the field docs above for each.

    [light] need not arrive normalised; it is only ever used for the cosine
    against a wall's normal, so it is normalised here once. It must point
    {e somewhere}, though: a zero vector is what {!Vec.normalize} passes through
    unchanged, and it would leave the field below claiming to be a unit vector
    when it is not.

    A place with no discernible direction to its light is made with
    [~directional:0.] instead, which closes the band {!face_shading} works
    across so that every wall reads the same whichever way it faces. That is a
    setting and not a degenerate case, which is the whole reason for refusing
    the other spelling of it.

    @raise Invalid_argument
      if [light] has no finite positive length — the zero vector, or one whose
      coordinates are [nan] or infinite. All of them come to the same thing:
      {!Vec.normalize} cannot get a direction out of them, so the field would be
      left claiming to be a unit vector when it is not. *)

val default : t
(** The air of an unremarkable, mildly hazy day: dark blue-grey haze arriving
    over twelve cells, light from the north-west, and a [0.6 .. 1.0] shading
    band. The value to start from and perturb — every number in it is the one
    the demos settled on. *)

val fog : t -> float -> float
(** [fog air distance] is how much of a surface's own colour survives at that
    many cells away: [1.] up close, falling linearly to [min_brightness] at
    [fog_distance] and staying there beyond it.

    What is lost is not lost to black but to [haze], so this is the {e amount}
    of a blend and not a factor to multiply by. What reaches the eye is
    [Color.lerp surface air.haze (1. -. fog air d)]. Multiplying instead fades
    everything towards black however the air is coloured, which is only right in
    a world whose haze happens to be black — and the haze is a value here
    precisely so that it need not be.

    Orientation is the other half of the light and it {e does} multiply, because
    a wall turned away from the light goes dark rather than hazy. Together the
    two are [surface * face_shading * fog + haze * (1 - fog)]: {!face_shading}
    scales the surface's own colour, and this says how much of that colour is
    there to be seen at all. Note that the haze arrives at [1. -. fog] with no
    orientation in it — folding the shading in there as well would make a wall
    facing away fade into the distance faster than the wall beside it.

    On the floor and ceiling it doubles as a horizon haze, fading the planes out
    into the distance instead of letting them run to a hard edge — and into the
    same [haze] that fills the band where the eye looks past both planes, and
    the doorway the portal recursion could not reach, so that the three meet
    without a seam. *)

val face_shading : t -> Vec.t -> float
(** [face_shading air normal] is how brightly a surface facing that way catches
    the light, where [normal] is the surface's unit normal. With walls at every
    angle a fixed east/west versus north/south rule no longer works, so the
    brightness follows how squarely the normal faces the light. [Float.abs]
    means both faces of a wall light the same.

    A normal that is not unit length scales the result, the same way an
    un-normalised [light] would; nothing here checks it.

    The band is [ambient .. ambient + directional] and never reaches zero, so no
    wall falls to black on orientation alone. Narrowing it to almost nothing is
    how a place is made to look as though it has no light source at all: every
    wall the same brightness whichever way it faces, and only distance to tell
    them apart. *)
