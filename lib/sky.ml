(** The open sky drawn where a room has no roof ({!Room.field-ceiling} is
    [Open]).

    A sky is an infinitely far backdrop, so its colour depends only on the
    direction it is looked in — the azimuth of the column's ray, and how high up
    the view the pixel sits — never on where the player stands. That is what
    makes it read as sky: it does not slide past as you walk, only wheels around
    as you turn and tilts as you look up and down.

    A sky is a value rather than a set of constants because it belongs to the
    room. Rooms are authored in their own coordinate frames with no world
    compass between them, so a room seen through a doorway already takes its
    azimuth from the {e nested} direction and has its own sun whether we like it
    or not; letting it have its own sky as well only makes that honest. *)

type t = {
  horizon : Color.t;  (** at eye level *)
  zenith : Color.t;  (** straight up *)
  sun : Color.t;
  sun_azimuth : float;  (** the compass direction the sun sits in *)
  sun_height : float;  (** how high up the view it rides *)
  sun_radius : float;  (** the size of its soft glow *)
  gradient : float;
      (** how fast the gradient climbs from the horizon to the zenith. Screen
          elevation only reaches about 0.4 looking level, so a value above one
          stretches the gradient to span the visible sky rather than leaving it
          all near the horizon colour. *)
}

(** Wrap an angle to [-pi, pi], so azimuths either side of the seam still
    compare by their true angular distance. *)
let wrap a =
  let two_pi = 2. *. Float.pi in
  let a = Float.rem a two_pi in
  if a > Float.pi then a -. two_pi
  else if a < -.Float.pi then a +. two_pi
  else a

(** The colour a column looking along [azimuth] shows at screen elevation [up] —
    zero at the horizon, growing towards the top of the view. A vertical
    gradient from the horizon haze up to the deep zenith, with a soft round sun
    added where the view points near it. *)
let color t ~azimuth ~up =
  let g = Float.max 0. (Float.min 1. (up *. t.gradient)) in
  let sky = Color.lerp t.horizon t.zenith g in
  let daz = wrap (azimuth -. t.sun_azimuth) in
  let dup = up -. t.sun_height in
  let distance = Float.sqrt ((daz *. daz) +. (dup *. dup)) in
  let glow = Float.max 0. (1. -. (distance /. t.sun_radius)) in
  Color.lerp sky t.sun (glow *. glow)
