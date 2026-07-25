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
  room : int;
  pos : Vec.t;
  dir : Vec.t;
  right : Vec.t;
  pitch : float;  (** look up (+) or down (-), as a window-height fraction *)
}

let create ~room ~pos ~angle =
  let dir = Vec.of_angle angle in
  { room; pos; dir; right = Vec.perp dir; pitch = 0. }

let spawn world =
  create ~room:world.World.spawn.room ~pos:world.World.spawn.pos ~angle:0.

(** Carry the pose into a neighbouring room's frame: the position moves with
    {!Transform.point}, the two basis vectors only rotate ({!Transform.direction}
    — they carry no position), and the room changes. [pitch] is untouched,
    because a rigid motion of the flat world is horizontal and cannot tilt the
    view.

    Nothing is renormalised, and nothing needs to be: the rotation is exact, so
    [dir] and [right] come out unit and perpendicular however many doorways have
    been walked through. Used both by {!walk}, when the player crosses, and by
    the renderer, when a ray looks through. *)
let through transform ~room player =
  {
    player with
    room;
    pos = Transform.point transform player.pos;
    dir = Transform.direction transform player.dir;
    right = Transform.direction transform player.right;
  }

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
    otherwise.

    Also reports the first doorway the path crosses. Because the two axes are
    resolved one after the other, that path is an {e L} and not the straight
    line between where the step began and where it ended, so each leg has to be
    asked separately: near a jamb the two disagree about which side of a
    threshold the player passed. *)
let slide world player (delta : Vec.t) =
  let open Vec in
  let crossed = ref None in
  let step from moved =
    if World.can_step world ~room:player.room ~from ~dest:moved then begin
      if Option.is_none !crossed then
        crossed := World.crossing world ~room:player.room ~from ~dest:moved;
      moved
    end
    else from
  in
  let after_x =
    step player.pos { player.pos with x = player.pos.x +. delta.x }
  in
  let after_y = step after_x { after_x with y = after_x.y +. delta.y } in
  ({ player with pos = after_y }, !crossed)

(** The two movement axes of a first person camera: [forward] along [dir],
    [strafe] along [right]. Both vectors are unit length, so a step of the same
    size costs the same distance whichever way it points.

    Adding the two outright would make a diagonal step the {e diagonal} of the
    two — holding forward and strafe together would walk [sqrt 2] times faster
    than either alone — so the sum is clamped back to the longer of the two
    axes. A step along one axis alone is left as it is, and half a step still
    covers half the ground.

    A step that went through a doorway ends by carrying the whole pose into the
    room on the other side with {!through}. That the finish is confirmed to lie
    on the far side of the opening — and not merely to have crossed its line
    somewhere — is what stops a step that rounds a jamb, out through the doorway
    and straight back in, from being counted as a crossing. *)
let walk world player ~forward ~strafe =
  let delta =
    Vec.add (Vec.scale player.dir forward) (Vec.scale player.right strafe)
  in
  let limit = Float.max (Float.abs forward) (Float.abs strafe) in
  let length = Vec.length delta in
  let start = player.pos in
  let moved, crossed =
    slide world player
      (if length > limit then Vec.scale delta (limit /. length) else delta)
  in
  match crossed with
  | None -> moved
  | Some portal ->
      (* Which side of the opening a point falls on, by sign. *)
      let side p = Vec.cross portal.World.threshold.edge (Vec.sub p portal.World.threshold.a) in
      if side start *. side moved.pos < 0. then
        through portal.World.onto ~room:portal.World.to_room moved
      else moved
