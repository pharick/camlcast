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

type t = private {
  horizon : Color.t;  (** at eye level *)
  zenith : Color.t;  (** straight up *)
  sun : Color.t;  (** the colour of the sun's disc and the glow around it *)
  sun_azimuth : float;
      (** the compass direction the sun sits in, in radians, on the same
          reckoning as {!Vec.of_angle}; any angle, wrapped where it is used *)
  sun_height : float;
      (** how high up the view it rides — a screen elevation on the same scale
          as {!color}'s [up], where a level view spans about [0. .. 0.4] from
          the horizon to the top of the window. Useful values sit around
          [0.1 .. 0.5]; [1.] is far above the visible sky until the player
          looks up. *)
  sun_radius : float;
      (** the size of its soft glow, finite and above [0.] — it is a divisor.
          Measured in view angle: the distance it bounds mixes azimuth radians
          with the same screen elevation [sun_height] rides on, so it is
          roughly radians near the horizon. Useful values sit around
          [0.2 .. 0.8]. *)
  gradient : float;
      (** how fast the gradient climbs from the horizon to the zenith; finite,
          zero or above. Screen elevation only reaches about 0.4 looking
          level, so a value above one stretches the gradient to span the
          visible sky rather than leaving it all near the horizon colour. *)
}
(** Private now that there is something to hold: [sun_radius] divides in
    {!color}, so a sky with a radius of [0.] — or a [nan] anywhere the glow is
    measured — would put [nan] into every pixel the sun touches, silently.
    {!make} is the only way to one, all seven fields stay readable, and a
    caller says only the fields that make their sky different from
    {!default}. *)

val make :
  ?horizon:Color.t ->
  ?zenith:Color.t ->
  ?sun:Color.t ->
  ?sun_azimuth:float ->
  ?sun_height:float ->
  ?sun_radius:float ->
  ?gradient:float ->
  unit ->
  t
(** A sky, each field defaulting to {!default}'s — the pale-horizoned noon the
    demos carry — so [make ~sun_azimuth:2.7 ~sun_height:0.06 ()] is a low
    evening sun and nothing else need be said. See the field docs above for
    each field's meaning, scale and useful range; the defaults are
    [Color.rgb 176 196 222] over the horizon, [Color.rgb 40 62 126] at the
    zenith, a sun of [Color.rgb 255 246 216] at azimuth [-0.9], height [0.5],
    radius [0.55], and a gradient of [2.2].

    @raise Invalid_argument
      if [sun_radius] is not finite and positive — it is the divisor in the
      sun's glow — or if [gradient] is negative, or if any of the four floats
      is not finite. Each test is the negation of the passing condition, so a
      [nan] fails it rather than slipping into every pixel of the sky. *)

val default : t
(** A clear noon: pale horizon, deep zenith, and a soft warm sun riding high.
    The sky to start from and perturb. *)

val color : t -> azimuth:float -> up:float -> Color.t
(** [color sky ~azimuth ~up] is the colour that sky shows to a column looking
    along [azimuth] radians, at screen elevation [up] — zero at the horizon,
    growing towards the top of the view. A vertical gradient from the horizon
    colour up to the deep zenith, with a soft round sun added where the view
    points near it.

    [azimuth] may be any angle; it is wrapped, so the sun is found once however
    many turns the player has made. *)
