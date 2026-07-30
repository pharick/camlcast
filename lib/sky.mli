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
  sun : Color.t;  (** the colour of the sun's disc and the glow around it *)
  sun_azimuth : float;
      (** the compass direction the sun sits in, in radians, on the same
          reckoning as {!Vec.of_angle} *)
  sun_height : float;  (** how high up the view it rides *)
  sun_radius : float;  (** the size of its soft glow *)
  gradient : float;
      (** how fast the gradient climbs from the horizon to the zenith. Screen
          elevation only reaches about 0.4 looking level, so a value above one
          stretches the gradient to span the visible sky rather than leaving it
          all near the horizon colour. *)
}
(** Concrete, and the one leaf record here that stays so. There is no
    constructor to make private in favour of: a sky is seven independent fields
    with nothing derived from them, and every one that exists is written down as
    a literal at the point of use. Closing the record would mean inventing a
    seven-argument constructor that no caller wants, to protect an invariant
    there isn't. *)

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
