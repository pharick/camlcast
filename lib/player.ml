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

    {1 Why a leg is walked and not jumped}

    A doorway is a gap in a room's boundary, so nothing of the room a leg starts
    in stops it once it is through: past the opening this room's walls have
    nothing left to say about where the leg went, and only the neighbour's do.
    Asking the neighbour means standing in the neighbour's frame — so a leg is
    not applied whole and then carried across. It is {e clipped} at the opening
    it goes through, the world vouches for that much of it, the pose is carried
    over with {!through}, and what is left of the leg is turned into the frame
    it has arrived in and walked again from there.

    Walked, and not merely done twice. A leg long enough to cross a room can
    reach a second doorway — or a wall, or a shut door, standing just beyond the
    first — and every one of those lives in a room that the leg's starting room
    has never heard of. Clipping repeats until the leg runs out, so every part
    of it is measured against the walls of the room that part of it is actually
    in. {!Config.max_crossings_per_step} bounds the repetition, because a world
    may fold back on itself and nothing about its shape would otherwise stop the
    walk.

    The leg still to come is carried across too, at every crossing and not only
    the first: it was measured along the axes of a room the player has since
    left, and one that has been left twice needs turning twice.

    A blocked leg leaves the pose where the last opening put it and abandons the
    rest — the same rule the two axes already follow, applied along a leg rather
    than across the pair of them.

    {1 What comes back}

    Every doorway gone through, in the order they were met. That order is the
    whole reason this returns a list rather than a count: a game building a route
    home has to unwind the crossings the way they were made, and a frame that
    went out and came back — an {e L} that rounds a jamb, or a step all the way
    round a loop of rooms — has to leave the stack as it found it. The two
    transforms of a link are inverses, so such a frame lands where it should, in
    the room it set out from, and the list is what says it went anywhere at
    all. *)
let slide world player (delta : Vec.t) =
  (* [leg] is what is left to walk, [pending] the axis not yet started; both are
     in the frame of the room the player is standing in, so both are carried at
     every crossing. Each call reports the pose it ended in, [pending] as the
     crossings left it, and the trace so far, newest first. *)
  let rec step player ~leg ~pending ~trace ~budget =
    let from = player.pos in
    let dest = Vec.add from leg in
    let refuse ~dest = not (World.can_step world ~room:player.room ~from ~dest) in
    match World.crossing world ~room:player.room ~from ~dest with
    | Some (slot, (portal : World.portal), at) when budget > 0 ->
        (* Only as far as the opening: past it this room cannot answer. *)
        let stop = Vec.add from (Vec.scale leg at) in
        if refuse ~dest:stop then (player, pending, trace)
        else
          let onto = portal.World.onto in
          let carry = Transform.direction onto in
          step
            (through onto ~room:portal.World.to_room { player with pos = stop })
            ~leg:(carry (Vec.scale leg (1. -. at)))
            ~pending:(carry pending)
            ~trace:
              ({
                 from_room = player.room;
                 from_threshold = slot;
                 to_room = portal.World.to_room;
                 to_threshold = portal.World.twin;
                 onto;
               }
              :: trace)
            ~budget:(budget - 1)
    | Some _ ->
        (* Another doorway, and no allowance left to follow it through. Refuse
           what is left rather than apply it in a room it no longer belongs
           to. *)
        (player, pending, trace)
    | None ->
        if refuse ~dest then (player, pending, trace)
        else ({ player with pos = dest }, pending, trace)
  in
  let budget = Config.max_crossings_per_step in
  let after_x, pending, trace =
    step player ~leg:(Vec.make delta.x 0.) ~pending:(Vec.make 0. delta.y)
      ~trace:[] ~budget
  in
  let after_y, _, trace =
    step after_x ~leg:pending ~pending:(Vec.make 0. 0.) ~trace ~budget
  in
  { player = after_y; crossings = List.rev trace }

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
