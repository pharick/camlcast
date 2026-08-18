(** The camera pose: where the player stands and which way they face.

    Instead of storing an angle and calling trigonometry once per screen column,
    the pose keeps two unit vectors: [dir], where the camera looks, and [right],
    a quarter turn clockwise from it.

    {v
                  dir
                   ^
                   |
        [player]   +-----> right
    v}

    {!Viewport} scales [right] by the half width of the projection screen to
    build the ray for a column; that is where the field of view enters. Turning
    rotates both vectors, so they stay perpendicular by construction — the
    property {!Ray} needs to report a distance free of fish-eye distortion.

    Looking up and down is a separate matter. A raycaster has no true vertical
    rotation, so [pitch] is not part of the [dir] / [right] basis at all: it is
    a fraction that {!Viewport} shears the image by. It is kept here, clamped,
    so the game loop carries it alongside the rest of the pose. *)

type t = private {
  room : int;  (** the index of the room the player is standing in *)
  pos : Vec.t;  (** where they stand, in that room's own coordinates *)
  dir : Vec.t;  (** where the camera looks; unit length *)
  right : Vec.t;  (** a quarter turn clockwise from [dir]; unit length *)
  pitch : float;
      (** look up (+) or down (-), as a window-height fraction, clamped to
          [-Config.max_pitch .. Config.max_pitch] *)
}

val make : room:int -> pos:Vec.t -> angle:float -> t
(** A pose standing at [pos] in the room with that index, facing [angle] radians
    on {!Vec.of_angle}'s reckoning, looking level. [dir] and [right] are both
    derived from [angle], so they start unit and perpendicular, and every
    rotation after preserves that. This is why the record is private: a [dir]
    written by hand is a [dir] whose [right] silently no longer agrees with it,
    and every column of every frame is built from the pair.

    For that to mean anything the angle has to be a number, and the numbers the
    private record promises are the ones the whole picture is measured from. A
    non-finite angle is therefore refused here rather than believed and handed
    on. {!Vec} deliberately refuses nothing itself: the refusing belongs where a
    direction is first promised to be one, and this is where.

    @raise Invalid_argument
      if [angle] is not finite. The check is negated, so a [nan] is refused with
      it. *)

val spawn : ?angle:float -> World.t -> t
(** The pose a world says to start in: {!World.spawn}'s room and position,
    facing [angle] radians on {!Vec.of_angle}'s reckoning, or [0.] if
    unspecified. The angle is the player's to choose rather than the world's to
    dictate, which is why it is not part of {!World.make}'s [spawn].

    @raise Invalid_argument if [angle] is not finite, as {!make} does. *)

val through : Transform.t -> room:int -> t -> t
(** Carry the pose into a neighbouring room's frame. The position moves with
    {!Transform.point}; the two basis vectors only rotate
    ({!Transform.direction} — they carry no position); the room changes. [pitch]
    is untouched, because a rigid motion of the flat world is horizontal and
    cannot tilt the view.

    Nothing is renormalised, and nothing needs to be. The rotation is exact, so
    [dir] and [right] come out unit and perpendicular however many doorways have
    been walked through. Used both by {!slide}, when the player crosses, and by
    the renderer, when a ray looks through. *)

val turn : t -> radians:float -> t
(** Rotate the view by [radians], clockwise positive, both basis vectors
    together.

    @raise Invalid_argument
      if [radians] is not finite; such a rotation would turn the basis into a
      pair of nans and leave nothing unit about it. The check is negated, so a
      [nan] is refused with it. *)

val pitch_by : t -> fraction:float -> t
(** Tip the view up or down by that fraction of the window height, clamped to
    {!Config.max_pitch} so it never tips past where the sheared image stops
    looking right.

    {b Not radians, whatever the name pitch suggests.} There is no
    vertical rotation to measure: {!Viewport.make} takes the pitch straight into
    [horizon = height/2 + pitch*height], so [0.1] slides the horizon a tenth of
    the window and means nothing in particular in degrees. Everything else that
    handles the number says so:

    - {!Config.pitch_speed} and {!Config.pitch_sensitivity} are quoted in window
      heights;
    - {!Binding.motion} hands one back in them;
    - {!type-t}'s own [pitch] field is documented as one.

    The label here was the single dissenter, sitting directly under {!turn},
    which really is radians and really is a rotation. The two look like one kind
    of thing and are not; mistaking which is which makes the walls lean as the
    player looks up.

    @raise Invalid_argument
      if [fraction] is not finite. The clamp is why this one matters:
      [Float.min] and [Float.max] propagate a nan rather than pinning it, so the
      pitch this type says lies inside the limit would be a number that compares
      false with both ends of it. The check is negated, so a [nan] is refused
      with it. *)

type crossing = {
  from_room : int;
  from_threshold : int;  (** which of that room's thresholds was gone through *)
  to_room : int;
  to_threshold : int;  (** the same doorway, numbered from the other side *)
  onto : Transform.t;  (** the frame change that was applied on the way *)
}
(** One doorway, gone through. Thresholds are given as indices and not as
    values: an index survives {!World.replace_room}, and it is already how a
    portal names its twin. A game that wants to lock the door it has just come
    through therefore has the two numbers it needs to change both sides.

    [onto] is what the pose was carried by. A game keeping a return route stacks
    these and walks them back through {!Transform.inverse}, which is exact — so
    a route home through a loop that could not exist still arrives. *)

type movement = { player : t; crossings : crossing list }
(** Where a step ended, and every doorway it went through on the way, in the
    order they were crossed. Most frames cross nothing and the list is empty. *)

val crossed : movement -> bool
(** Whether the step went through a doorway. That is not the same question as
    whether it ended in a different room. A single step can round a jamb — out
    through an opening and back in through its twin — and end where it started;
    so can one that goes all the way round a loop of rooms. Comparing
    [movement.player.room] against the room it set out from calls both of those
    nothing happening, and the crossings are the only place they are written
    down.

    A game that builds its world as it is walked through asks this and not the
    room index. {!Engine.run_world} asks it on a game's behalf; a game that has
    outgrown that wrapper and moved to {!Engine.run} asks it here rather than
    working it out again. *)

val slide : World.t -> t -> Vec.t -> movement
(** Move by that many cells, resolving the two axes independently. Walking into
    a wall at an angle keeps the component that is still free, so the player
    slides along the wall instead of sticking to it. {!World.passable} sweeps
    the player's {!Config.collision_padding} disc along each of the two steps; a
    step it refuses leaves that axis where it was.

    A leg that reaches a doorway is {e clipped} there, carried across with
    {!through}, and walked on from the other side — repeatedly, bounded by
    {!Config.max_crossings_per_step} — so every part of it is measured against
    the walls of the room that part of it is actually in. The crossings come
    back in the order they were made; see {!type-crossing} for what a game does
    with them. *)

val traverse : World.t -> t -> forward:float -> strafe:float -> movement
(** The two movement axes of a first person camera: [forward] cells along [dir],
    [strafe] along [right]. Both vectors are unit length, so a step of the same
    size costs the same distance whichever way it points. The sum is clamped
    back to the longer of the two axes, so holding both does not walk [sqrt 2]
    times faster than either alone.

    A step that went through a doorway comes back from {!slide} already in the
    room on the other side, pose and all, with the doorways it went through
    alongside.

    @raise Invalid_argument
      if either is not finite. That clamp is written as [length > limit], which
      is false of a nan, so an unrefused nan step would reach {!slide} unclamped
      and end at a position that is nowhere. Nothing in {!Engine} can ask for
      one — {!Clock.frame_time} is bounded at both ends — so this fires on a
      game whose own [update] worked one out, which is the authoring mistake it
      is for. *)
