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

(** Movement is expressed in map cells per frame, rotation in radians per frame.
    At ~60 FPS this is roughly 3.6 cells and 2 radians per second. *)
let move_speed = 0.06

let rot_speed = 0.035

(** The player is a point, not a circle, so we probe for walls slightly ahead of
    the point we want to move to. Without this you can press your nose flat
    against a wall and see through the corner. *)
let collision_padding = 0.15

(** Walls fade to [min_brightness] once they are [fog_distance] cells away. This
    is pure eye candy, but it also gives a strong sense of depth in a world made
    of flat colours. *)
let fog_distance = 12.

let min_brightness = 0.25

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

(** Keyboard look speed, for the arrow keys: radians (yaw) and window-height
    fractions (pitch) per frame. *)
let pitch_speed = 0.02

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

(** Milliseconds slept at the end of each frame (~60 FPS). *)
let frame_delay = 16l
