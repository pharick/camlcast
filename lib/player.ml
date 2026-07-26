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

(** Did a step from [from] to [dest] go {e through} [portal], and not merely
    across the line its opening lies on? Which side of that line a point falls on
    is the sign of the cross product below, so the step went through exactly when
    its two ends disagree about it.

    A step that finishes {e on} the line has no side, and so does not count.
    That is what keeps a step taken along an opening rather than through it —
    including one that rounds a jamb and comes straight back — from being read as
    going through. *)
let crosses (portal : World.portal) ~from ~dest =
  let side p =
    Vec.cross portal.World.threshold.edge (Vec.sub p portal.World.threshold.a)
  in
  side from *. side dest < 0.

type crossing = {
  from_room : int;
  from_threshold : int;  (** which of that room's thresholds was gone through *)
  to_room : int;
  to_threshold : int;  (** the same doorway, numbered from the other side *)
  onto : Transform.t;  (** the frame change that was applied on the way *)
}
(** One doorway, gone through. Thresholds are given as indices and not as
    values: an index is what survives {!World.replace_room}, and it is already
    how a portal names its [twin], so a game that wants to lock the door it has
    just come through has the two numbers it needs to change both sides.

    [onto] is what the pose was carried by. A game keeping a return route stacks
    these and walks them back through {!Transform.inverse}, which is exact — so
    a route home through a loop that could not exist still arrives. *)

type movement = { player : t; crossings : crossing list }
(** Where a step ended, and every doorway it went through on the way, in the
    order they were crossed. Most frames cross nothing and the list is empty. *)

(** Move by [delta] cells, resolving the two axes independently so that walking
    into a wall at an angle keeps the component that is still free — you slide
    along the wall instead of sticking to it. {!World.can_step} sweeps the
    player's {!Config.collision_padding} disc along each of the two steps, so it
    is enough here to take the ones it allows and leave the axis where it was
    otherwise.

    A leg that goes through a doorway carries the whole pose into the room on the
    other side with {!through} before the next one is taken, and that is what
    makes the second leg safe. The two are resolved one after the other, so the
    path is an {e L} whose corner can lie well past an opening — far enough past
    that {!World.can_step} no longer finds the step near it, and this room's
    boundary has nothing left to say about where it went. Only the neighbour's
    does, and asking the neighbour means standing in the neighbour's frame. The
    leg still to come is rotated into that frame too, since it was measured
    along the axes of a room the player has left.

    A step can therefore cross two doorways, or the same one twice: an {e L} that
    rounds a jamb, out through an opening and back in, meets the twin threshold
    on the way back and returns through it. The two transforms of a link are
    inverses, so it lands where it should, in the room it set out from.

    Both are reported, in the order they were met. That order is the whole
    reason this returns a list rather than a count: a game building a route home
    has to unwind the crossings the way they were made, and a frame that went
    out and came back has to leave the stack as it found it. *)
let slide world player (delta : Vec.t) =
  (* Each leg reports the pose it ended in, the transform it went through to get
     there — the identity unless it crossed — and the crossing itself if there
     was one. *)
  let step player (leg : Vec.t) =
    let from = player.pos in
    let dest = Vec.add from leg in
    if not (World.can_step world ~room:player.room ~from ~dest) then
      (player, Transform.identity, None)
    else
      let moved = { player with pos = dest } in
      match World.crossing world ~room:player.room ~from ~dest with
      | Some (slot, portal) when crosses portal ~from ~dest ->
          ( through portal.World.onto ~room:portal.World.to_room moved,
            portal.World.onto,
            Some
              {
                from_room = player.room;
                from_threshold = slot;
                to_room = portal.World.to_room;
                to_threshold = portal.World.twin;
                onto = portal.World.onto;
              } )
      | _ -> (moved, Transform.identity, None)
  in
  let after_x, onto, first = step player (Vec.make delta.x 0.) in
  let after_y, _, second =
    step after_x (Transform.direction onto (Vec.make 0. delta.y))
  in
  { player = after_y; crossings = List.filter_map Fun.id [ first; second ] }

(** The two movement axes of a first person camera: [forward] along [dir],
    [strafe] along [right]. Both vectors are unit length, so a step of the same
    size costs the same distance whichever way it points.

    Adding the two outright would make a diagonal step the {e diagonal} of the
    two — holding forward and strafe together would walk [sqrt 2] times faster
    than either alone — so the sum is clamped back to the longer of the two
    axes. A step along one axis alone is left as it is, and half a step still
    covers half the ground.

    A step that went through a doorway comes back from {!slide} already in the
    room on the other side, pose and all, with the doorways it went through
    alongside. *)
let traverse world player ~forward ~strafe =
  let delta =
    Vec.add (Vec.scale player.dir forward) (Vec.scale player.right strafe)
  in
  let limit = Float.max (Float.abs forward) (Float.abs strafe) in
  let length = Vec.length delta in
  slide world player
    (if length > limit then Vec.scale delta (limit /. length) else delta)

(** {!traverse} for a caller that only wants to know where the player ended up.
    Everything that walks and does not care which doorways it went through —
    the demos, and the compatibility path through {!Engine.run} — goes through
    here. *)
let walk world player ~forward ~strafe =
  (traverse world player ~forward ~strafe).player
