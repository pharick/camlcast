(** Every tunable number of the engine, collected in one place so the rest of
    the code reads as logic rather than magic constants.

    {b Tunable} is narrower than "numeric". A value belongs here if a game or a
    player could reasonably want it different and still have a working engine:
    how fast the player walks, how wide the view is, how many rooms deep a
    doorway looks, how long a frame may take. Changing any of them gives a
    different game, not a broken one.

    The engine's geometric tolerances are not tunable and are not here. Each
    marks where float arithmetic stops carrying the quantity being measured and
    starts carrying rounding — a fact about the arithmetic and about the
    quantity. Each therefore lives with the thing it is a tolerance for, named,
    with its reasoning beside it:

    - {!Vec.parallel}: the sine below which two directions are one, shared by
      {!Ray.cast} and {!Room.segments_cross};
    - {!Plane.parallel}: the line of sight that runs along a plane instead of
      meeting it, shared by {!Plane.cast} and {!Renderer};
    - {!Ray.min_distance}: how near a wall may be and still be divided by;
    - {!World}'s [epsilon]: what the two sides of a link may disagree about,
      which {!Camlcast.Check} asks {!World} for rather than keeping its own
      copy.

    Moving those here would put them under this page's promise that changing a
    value is a choice. Changing a tolerance is a bug: a room whose corners leak,
    a band of pixels that is neither floor nor ceiling, a doorway one of two
    readers thinks is shut. They are collected in the sense that matters — each
    is defined once, and every caller reads that one — and the place they are
    collected is the module that owns the arithmetic. *)

val window_title : string
(** The window's title. The engine ships no content, and this is the one string
    in it that a player reads. A game that wants its own name on the title bar
    has to open its own window rather than change this. *)

val initial_width : int
(** Width the window opens at. The window is resizable and can go fullscreen, so
    this is a starting point; nothing may assume it afterwards — see
    {!Viewport}. *)

val initial_height : int
(** Height the window opens at, on the same terms as {!initial_width}. The two
    together are {!reference_aspect}. *)

val fov : float
(** Horizontal field of view at {!reference_aspect}, {b in radians}. A third of
    pi — 60 degrees — is the classic Wolfenstein 3D value; wider angles start to
    look like a fish-eye lens. *)

val reference_aspect : float
(** The window shape {!fov} is quoted for. Reshaping the window keeps the
    vertical angle this implies and lets the horizontal one follow. A wider
    window therefore shows more of the world, not a stretched version of it. *)

val move_speed : float
(** How fast the player walks, in map cells per second. {!Binding} scales it by
    the length of the frame, so walking speed is a property of the world and not
    of how long a frame took to render. *)

val turn_speed : float
(** How fast the player turns, in radians per second — {!move_speed} for yaw,
    scaled by the frame in the same way. This is the speed of the {e turn} axis
    of a {!Binding.t}, which both the arrow keys and the mouse feed. *)

val collision_padding : float
(** How far ahead of the player a step probes for walls, in map cells. The
    player is a point and not a circle; without this padding the player could
    press right up against a wall and see through the corner. *)

val eye_height : float
(** The player's eye height: half a cell above the floor, the middle of a normal
    one-cell wall. Walls are projected relative to this height, so a taller wall
    rises further above the horizon and a shorter one drops below the top of its
    neighbours. See {!Viewport} and {!World}. *)

val look_sensitivity : float
(** Sideways mouse look: radians of yaw per pixel of horizontal mouse motion.
    The mouse reading is a displacement and not a rate, so this is {e not}
    scaled by the frame again — see {!Binding}. *)

val pitch_sensitivity : float
(** Vertical mouse look: the fraction of the window height that the horizon
    shears by per pixel of vertical mouse motion. Not scaled by the frame, for
    the same reason as {!look_sensitivity}. *)

val pitch_speed : float
(** How fast the up and down arrows tilt the view, in fractions of the window's
    height per second. Pitch is faked by shearing the image rather than by a
    real rotation (see {!max_pitch}), so it is measured in screen height and not
    in radians. That makes it the one look axis whose unit is not the same as
    {!turn_speed}'s. *)

val max_pitch : float
(** How far the view may tip up or down, as the fraction of the window height
    the horizon may slide from the middle. Pitch is faked by shearing the image
    vertically rather than by a real rotation, so past this limit it starts to
    look visibly wrong. *)

val max_render_height : int
(** Row cap for software rendering. The floor and ceiling are cast one pixel at
    a time in software (see {!Renderer}), so their cost grows with the number of
    pixels. Rendering is capped to this many rows and the GPU scales the result
    up to the window, which keeps the frame rate steady no matter how large the
    window is. *)

val sprite_near_clip : float
(** How near a sprite may come to the eye and still exist, in cells, measured
    along the view the same way its depth is.

    A sprite is a billboard scaled by one over its distance, so as that distance
    approaches zero its box grows without bound. Past one point it covers the
    screen; past another it is millions of pixels across and the arithmetic
    placing it is worth nothing. Sprites are not collision geometry — nothing in
    {!Player} knows about them — so the player may walk into one and reach that
    point. This value is where the engine stops trying.

    Read by {!Renderer}, which will not draw a sprite nearer than this, and by
    {!Sight}, which will not report one. It is one number rather than two on
    purpose: a sprite the crosshair could pick but the frame does not show is a
    target the player cannot see, and a ring drawn round nothing. It sits well
    above {!Ray.min_distance}, the floor for walls: a wall fills its column
    however close it is, so it needs no more than a guard against dividing by
    zero, and {!collision_padding} keeps the player off it besides. *)

val max_portal_depth : int
(** How many rooms deep the {!Renderer} will look through a line of open
    doorways. Rooms are free to form a cycle — two facing each other is enough —
    so nothing about the world's shape makes the portal recursion terminate;
    this budget is what does. Raising it costs another full column of drawing
    per doorway still in view and buys very little: by the third room an opening
    is a few pixels across. Beyond the budget the doorway is filled with the
    world's {!Atmosphere.haze}, the colour its distance fog already fades into,
    so running out reads as depth rather than as a hole.

    {!Sight} reads it too, as the default reach of the crosshair's ray. What can
    be picked then stays what can be seen at every depth — including the last,
    where both give up on the same opening.

    A world that grows as the player walks reads this too. The generator has to
    keep every room within this many doorways of the player built; otherwise the
    player would watch a blank wall turn into a doorway as they approached it.
*)

val max_crossings_per_step : int
(** How many doorways one leg of a step may be resolved through.

    {!Player.slide} walks a leg opening by opening: it clips the leg at each
    one, has the world vouch for that much of it, and carries what is left into
    the room on the other side. A step is therefore only ever measured against
    the walls of a room it is actually in. Rooms may form a cycle, and a leg may
    round a jamb back through the doorway it came out of, so nothing about the
    world's shape makes that walk terminate. This budget is what does, exactly
    as {!max_portal_depth} is for the recursion that looks through the same
    doorways.

    Deliberately far above what a frame can reach. A leg is at most
    {!move_speed} times {!max_frame_time} long — a third of a cell — so crossing
    even two openings takes rooms thinner than the player is wide. Running out
    therefore means a caller has driven {!Player.slide} directly with a step no
    frame would ask for. The rest of the leg is then refused, which leaves the
    player standing in a doorway the world has already vouched for. *)

val frame_budget : float
(** Seconds a frame is allowed to take (~60 FPS). {!Engine} sleeps off whatever
    is left of this after rendering, so a cheap frame does not spin the CPU. A
    frame that overruns the budget is simply late and sleeps not at all. *)

val max_frame_time : float
(** The longest frame the simulation will believe. A frame that took longer —
    the window was dragged, the machine stalled, the program was suspended — is
    treated as this long. The player then stutters instead of taking one
    enormous step across the level. *)
