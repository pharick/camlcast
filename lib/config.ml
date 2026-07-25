(** Every tunable number of the engine, in one place, so that the rest of the
    code reads as logic instead of magic constants. *)

let window_title = "OCaml Raycaster"

(** Size the window opens at. It is resizable and can go fullscreen, so this is
    a starting point and nothing may assume it afterwards — see {!Viewport}. *)
let initial_width = 1024

let initial_height = 768

(** Horizontal field of view at {!reference_aspect}. 60 degrees is the classic
    Wolfenstein 3D value: wider angles start to look like a fish-eye lens. *)
let fov = Float.pi /. 3.

(** The window shape [fov] is quoted for. Reshaping the window keeps the
    vertical angle this implies and lets the horizontal one follow, so a wider
    window shows more of the world rather than a stretched version of it. *)
let reference_aspect = 4. /. 3.

(** Movement is expressed in map cells per second, rotation in radians per
    second. {!Input} scales both by the length of the frame, so how fast the
    player walks is a property of the world and not of how long a frame took to
    render. *)
let move_speed = 3.6

let rot_speed = 2.1

(** The player is a point, not a circle, so we probe for walls slightly ahead of
    the point we want to move to. Without this you can press your nose flat
    against a wall and see through the corner. *)
let collision_padding = 0.15

(** The player's eye sits half a cell above the floor — the middle of a normal
    one-cell wall. Walls are projected relative to this height, so a taller wall
    rises further above the horizon and a shorter one drops below the top of its
    neighbours. See {!Viewport} and {!World}. *)
let eye_height = 0.5

(** Mouse look. [look_sensitivity] turns one pixel of horizontal mouse motion
    into radians of yaw; [pitch_sensitivity] turns one pixel of vertical motion
    into a fraction of the window height that the horizon shears by. *)
let look_sensitivity = 0.0025

let pitch_sensitivity = 0.0025

(** Keyboard look speed, for the arrow keys: radians (yaw, see {!rot_speed}) and
    window-height fractions (pitch) per second. *)
let pitch_speed = 1.2

(** How far the view may tip up or down, as the fraction of the window height
    the horizon is allowed to slide from the middle. The pitch is faked by
    shearing the image vertically rather than by a real rotation, so past this
    it starts to look visibly wrong. *)
let max_pitch = 0.75

(** The floor and ceiling are cast one pixel at a time in software (see
    {!Renderer}), so their cost grows with the number of pixels. Rendering is
    capped to this many rows and the result is scaled up to the window by the
    GPU, which keeps the frame rate steady no matter how large the window is. *)
let max_render_height = 480

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
let max_portal_depth = 3

(** Seconds a frame is allowed to take (~60 FPS). {!Engine} sleeps off whatever
    is left of this after rendering, so a cheap frame does not spin the CPU; a
    frame that overruns it is simply late and sleeps not at all. *)
let frame_budget = 1. /. 60.

(** The longest frame the simulation will believe. A frame that took longer —
    the window was dragged, the machine stalled, the program was suspended — is
    treated as this long, so the player stutters instead of taking one enormous
    step across the level. *)
let max_frame_time = 0.1
