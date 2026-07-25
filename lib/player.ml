(** The camera pose: where the player stands and which way they face.

    Instead of storing an angle and calling trigonometry once per screen column,
    we keep two unit vectors — [dir], where the camera looks, and [right], a
    quarter turn clockwise from it:

    {v
                  dir
                   ^
                   |
        [player]   +-----> right
    v}

    {!Viewport} scales [right] by the half width of the projection screen to
    build the ray for a column, which is where the field of view enters the
    picture. Turning rotates both vectors, so they stay perpendicular by
    construction — the property {!Ray} needs to report a distance free of
    fish-eye distortion.

    Looking up and down is a separate matter. A raycaster has no true vertical
    rotation, so [pitch] is not part of the [dir] / [right] basis at all: it is
    a fraction that {!Viewport} shears the image by. Keeping it here, clamped,
    just lets the game loop carry it alongside the rest of the pose. *)

type t = {
  pos : Vec.t;
  dir : Vec.t;
  right : Vec.t;
  pitch : float;  (** look up (+) or down (-), as a window-height fraction *)
}

let create ~pos ~angle =
  let dir = Vec.of_angle angle in
  { pos; dir; right = Vec.perp dir; pitch = 0. }

let spawn world = create ~pos:world.World.spawn ~angle:0.

let turn player ~radians =
  {
    player with
    dir = Vec.rotate player.dir radians;
    right = Vec.rotate player.right radians;
  }

(** Tip the view up or down by [delta], clamped to {!Config.max_pitch} so it
    never tips past where the sheared image stops looking right. *)
let pitch_by player ~delta =
  let limit = Config.max_pitch in
  {
    player with
    pitch = Float.max (-.limit) (Float.min limit (player.pitch +. delta));
  }

(** Move by [delta] cells, resolving the two axes independently so that walking
    into a wall at an angle keeps the component that is still free — you slide
    along the wall instead of sticking to it. {!World.can_step} sweeps the
    player's {!Config.collision_padding} disc along each of the two steps, so it
    is enough here to take the ones it allows and leave the axis where it was
    otherwise. *)
let slide world player (delta : Vec.t) =
  let open Vec in
  let step from moved =
    if World.can_step world ~from ~dest:moved then moved else from
  in
  let after_x =
    step player.pos { player.pos with x = player.pos.x +. delta.x }
  in
  let after_y = step after_x { after_x with y = after_x.y +. delta.y } in
  { player with pos = after_y }

(** The two movement axes of a first person camera: [forward] along [dir],
    [strafe] along [right]. Both vectors are unit length, so a step of the same
    size costs the same distance whichever way it points.

    Adding the two outright would make a diagonal step the {e diagonal} of the
    two — holding forward and strafe together would walk [sqrt 2] times faster
    than either alone — so the sum is clamped back to the longer of the two
    axes. A step along one axis alone is left as it is, and half a step still
    covers half the ground. *)
let walk world player ~forward ~strafe =
  let delta =
    Vec.add (Vec.scale player.dir forward) (Vec.scale player.right strafe)
  in
  let limit = Float.max (Float.abs forward) (Float.abs strafe) in
  let length = Vec.length delta in
  slide world player
    (if length > limit then Vec.scale delta (limit /. length) else delta)
