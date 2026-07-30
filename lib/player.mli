(** The camera pose: where the player stands and which way they face.

    Instead of storing an angle and calling trigonometry once per screen
    column, the pose keeps two unit vectors — [dir], where the camera looks,
    and [right], a quarter turn clockwise from it:

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
  room : int;  (** the index of the room the player is standing in *)
  pos : Vec.t;  (** where they stand, in that room's own coordinates *)
  dir : Vec.t;  (** where the camera looks; unit length *)
  right : Vec.t;  (** a quarter turn clockwise from [dir]; unit length *)
  pitch : float;
      (** look up (+) or down (-), as a window-height fraction, clamped to
          [-Config.max_pitch .. Config.max_pitch] *)
}

val create : room:int -> pos:Vec.t -> angle:float -> t
(** A pose standing at [pos] in the room with that index, facing [angle]
    radians on {!Vec.of_angle}'s reckoning, looking level. [dir] and [right]
    are both derived from [angle], so they start unit and perpendicular, which
    every rotation after preserves. *)

val spawn : World.t -> t
(** The pose a world says to start in: {!World.spawn}'s room and position,
    facing an angle of [0.]. *)

val through : Transform.t -> room:int -> t -> t
(** Carry the pose into a neighbouring room's frame: the position moves with
    {!Transform.point}, the two basis vectors only rotate
    ({!Transform.direction} — they carry no position), and the room changes.
    [pitch] is untouched, because a rigid motion of the flat world is
    horizontal and cannot tilt the view.

    Nothing is renormalised, and nothing needs to be: the rotation is exact, so
    [dir] and [right] come out unit and perpendicular however many doorways
    have been walked through. Used both by {!slide}, when the player crosses,
    and by the renderer, when a ray looks through. *)

val turn : t -> radians:float -> t
(** Rotate the view by [radians], clockwise positive, both basis vectors
    together. *)

val pitch_by : t -> radians:float -> t
(** Tip the view up or down, clamped to {!Config.max_pitch} so it never tips
    past where the sheared image stops looking right. *)

type crossing = {
  from_room : int;
  from_threshold : int;  (** which of that room's thresholds was gone through *)
  to_room : int;
  to_threshold : int;  (** the same doorway, numbered from the other side *)
  onto : Transform.t;  (** the frame change that was applied on the way *)
}
(** One doorway, gone through. Thresholds are given as indices and not as
    values: an index is what survives {!World.replace_room}, and it is already
    how a portal names its twin, so a game that wants to lock the door it has
    just come through has the two numbers it needs to change both sides.

    [onto] is what the pose was carried by. A game keeping a return route
    stacks these and walks them back through {!Transform.inverse}, which is
    exact — so a route home through a loop that could not exist still
    arrives. *)

type movement = { player : t; crossings : crossing list }
(** Where a step ended, and every doorway it went through on the way, in the
    order they were crossed. Most frames cross nothing and the list is
    empty. *)

val crossed : movement -> bool
(** Whether the step went through a doorway, which is not the same question as
    whether it ended in a different room. A single step can round a jamb — out
    through an opening and back in through its twin — and end where it
    started; so can one that goes all the way round a loop of rooms. Comparing
    [movement.player.room] against the room it set out from calls both of
    those nothing happening, and the crossings are the only place they are
    written down.

    That is what this is for: a game that builds its world as it is walked
    through asks this and not the room index. {!Engine.run_world} asks it on a
    game's behalf, and a game that has outgrown that wrapper and moved to
    {!Engine.run} asks it here rather than working it out again. *)

val slide : World.t -> t -> Vec.t -> movement
(** Move by that many cells, resolving the two axes independently so that
    walking into a wall at an angle keeps the component that is still free —
    you slide along the wall instead of sticking to it. {!World.passable}
    sweeps the player's {!Config.collision_padding} disc along each of the two
    steps, so a step it refuses leaves that axis where it was.

    A leg that reaches a doorway is {e clipped} there, carried across with
    {!through}, and walked on from the other side — repeatedly, bounded by
    {!Config.max_crossings_per_step}, so every part of it is measured against
    the walls of the room that part of it is actually in. The crossings come
    back in the order they were made; see {!type-crossing} for what a game
    does with them. *)

val traverse : World.t -> t -> forward:float -> strafe:float -> movement
(** The two movement axes of a first person camera: [forward] cells along
    [dir], [strafe] along [right]. Both vectors are unit length, so a step of
    the same size costs the same distance whichever way it points — and the
    sum is clamped back to the longer of the two axes, so holding both does
    not walk [sqrt 2] times faster than either alone.

    A step that went through a doorway comes back from {!slide} already in the
    room on the other side, pose and all, with the doorways it went through
    alongside. *)
