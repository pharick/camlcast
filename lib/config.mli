(** Every tunable number of the engine, in one place, so that the rest of the
    code reads as logic instead of magic constants. *)

val window_title : string

val initial_width : int
(** Size the window opens at. It is resizable and can go fullscreen, so this is
    a starting point and nothing may assume it afterwards — see {!Viewport}. *)

val initial_height : int

val fov : float
(** Horizontal field of view at {!reference_aspect}. 60 degrees is the classic
    Wolfenstein 3D value: wider angles start to look like a fish-eye lens. *)

val reference_aspect : float
(** The window shape [fov] is quoted for. Reshaping the window keeps the
    vertical angle this implies and lets the horizontal one follow, so a wider
    window shows more of the world rather than a stretched version of it. *)

val move_speed : float
(** Movement is expressed in map cells per second, rotation in radians per
    second. {!Input} scales both by the length of the frame, so how fast the
    player walks is a property of the world and not of how long a frame took to
    render. *)

val rot_speed : float

val collision_padding : float
(** The player is a point, not a circle, so we probe for walls slightly ahead of
    the point we want to move to. Without this you can press your nose flat
    against a wall and see through the corner. *)

val eye_height : float
(** The player's eye sits half a cell above the floor — the middle of a normal
    one-cell wall. Walls are projected relative to this height, so a taller wall
    rises further above the horizon and a shorter one drops below the top of its
    neighbours. See {!Viewport} and {!World}. *)

val look_sensitivity : float
(** Mouse look. [look_sensitivity] turns one pixel of horizontal mouse motion
    into radians of yaw; [pitch_sensitivity] turns one pixel of vertical motion
    into a fraction of the window height that the horizon shears by. *)

val pitch_sensitivity : float

val pitch_speed : float
(** Keyboard look speed, for the arrow keys: radians (yaw, see {!rot_speed}) and
    window-height fractions (pitch) per second. *)

val max_pitch : float
(** How far the view may tip up or down, as the fraction of the window height
    the horizon is allowed to slide from the middle. The pitch is faked by
    shearing the image vertically rather than by a real rotation, so past this
    it starts to look visibly wrong. *)

val max_render_height : int
(** The floor and ceiling are cast one pixel at a time in software (see
    {!Renderer}), so their cost grows with the number of pixels. Rendering is
    capped to this many rows and the result is scaled up to the window by the
    GPU, which keeps the frame rate steady no matter how large the window is. *)

val max_portal_depth : int
(** How many rooms deep the {!Renderer} will look through a line of open
    doorways. Rooms are free to form a cycle — two facing each other is enough —
    so nothing about the world's shape makes the portal recursion terminate;
    this budget is what does. Raising it costs another full column of drawing
    per doorway still in view, and buys very little: by the third room an
    opening is a few pixels across. Beyond it the doorway is filled with the
    world's {!Atmosphere.haze}, the colour its distance fog already fades into,
    so running out reads as depth rather than as a hole.

    A world that grows as the player walks reads this too: the generator has to
    keep every room within this many doorways of the player built, or the player
    would watch a blank wall turn into a doorway as they approached it. *)

val max_crossings_per_step : int
(** How many doorways one leg of a step may be resolved through.

    {!Player.slide} walks a leg opening by opening — clipping it at each one,
    having the world vouch for that much of it, and carrying what is left into
    the room on the other side — so a step is only ever measured against the
    walls of a room it is actually in. Rooms may form a cycle and a leg may
    round a jamb back through the doorway it came out of, so nothing about the
    world's shape makes that walk terminate; this budget is what does, exactly
    as {!max_portal_depth} is for the recursion that looks through the same
    doorways.

    Deliberately far above what a frame can reach. A leg is at most
    {!move_speed} times {!max_frame_time} long — a third of a cell — so crossing
    even two openings takes rooms thinner than the player is wide, and running
    out means a caller has driven {!Player.slide} directly with a step no frame
    would ask for. What happens then is that the rest of the leg is refused,
    which leaves the player standing in a doorway the world has already vouched
    for. *)

val frame_budget : float
(** Seconds a frame is allowed to take (~60 FPS). {!Engine} sleeps off whatever
    is left of this after rendering, so a cheap frame does not spin the CPU; a
    frame that overruns it is simply late and sleeps not at all. *)

val max_frame_time : float
(** The longest frame the simulation will believe. A frame that took longer —
    the window was dragged, the machine stalled, the program was suspended — is
    treated as this long, so the player stutters instead of taking one enormous
    step across the level. *)
