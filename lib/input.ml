(** What the hardware is reporting, and nothing about what it means. Nothing
    here knows about positions or walls, and — since {!Binding} — nothing here
    knows which key walks forward either: this module reads the keyboard and the
    mouse, counts the edges and the holds, and hands the result on for somebody
    else to interpret. *)

open Tsdl

type motion = {
  forward : float;  (** cells to walk along [dir] this frame *)
  strafe : float;  (** cells to walk along [right] this frame *)
  turn : float;  (** radians to rotate this frame, + clockwise *)
  pitch : float;  (** change in view pitch this frame, + looks up *)
}
(** What the player is asking for over one frame, as finished per-frame deltas,
    so {!Engine} can apply it straight to the {!Player}. {!Binding.motion} is
    what produces one; the type lives here because it is what a frame of input
    amounts to. *)

let still = { forward = 0.; strafe = 0.; turn = 0.; pitch = 0. }

(** How far the mouse moved since the last call. Relative mouse mode (enabled by
    {!Engine}) reports these deltas and keeps the cursor pinned, so it never
    runs into a screen edge.

    SDL accumulates the delta until somebody reads it, so this has to be called
    every frame whether the answer is wanted or not — see {!Engine.loop}. *)
let mouse_delta () =
  let _buttons, (dx, dy) = Sdl.get_relative_mouse_state () in
  (float_of_int dx, float_of_int dy)

type button =
  | Left
  | Middle
  | Right
      (** The mouse buttons a game may bind. SDL knows about more; these are the
          three every mouse has. *)

type control =
  | Key of Key.t
  | Button of button
      (** Something the player can hold down. Keys are places on the keyboard
          rather than letters, so a binding stays where it is under a different
          layout — see {!module-Key}.

          The engine has no opinion about what any of them mean. "Interact",
          "chalk", "journal" are a game's words for its own table from controls
          to actions; what the engine owns is the mechanism underneath — when a
          control went down, when it came up, and how long it was held. Even
          walking is a game's table now: {!Binding} holds it, and the engine's
          only part in that is a default. *)

(** A source that reports a number rather than being down or not, for the half
    of the input a control cannot express. A gamepad's sticks and triggers would
    join this list. *)
type analog =
  | Mouse_x  (** pixels the mouse moved right during this frame *)
  | Mouse_y  (** pixels it moved down *)

type reading =
  | Rate
      (** a fraction of full speed, in [-1, 1]: a stick pushed halfway asks to
          walk at half pace. Scaled by the frame's length, exactly as a held
          control is. *)
  | Displacement
      (** how far the thing has already moved during this frame, in its own
          units. Already a per-frame quantity, so scaling it by the frame's
          length again would be counting the frame twice. *)

(** What an analog source's number means, which is the one thing {!Binding}
    needs in order to scale it. The mouse reports where it has been; a stick
    reports how hard it is being pushed, and the two are not the same kind of
    number however alike they look. *)
let reads = function Mouse_x | Mouse_y -> Displacement

(* Controls are counted off into one flat range, a contiguous block per device,
   so that a frame's worth of them is three arrays and an index rather than a
   lookup structure. Each block begins where the one before it ends, so another
   device is another offset here and nothing above it moves. *)
let buttons = 3
let first_key = 0
let first_button = first_key + Key.count
let controls = first_button + buttons
let button_index = function Left -> 0 | Middle -> 1 | Right -> 2

let index = function
  | Key key -> first_key + Key.to_scancode key
  | Button button -> first_button + button_index button

let control_of_index i =
  if i < first_button then Key (Key.of_scancode (i - first_key))
  else
    Button (match i - first_button with 0 -> Left | 1 -> Middle | _ -> Right)

type actions = {
  down : bool array;  (** what is held now, indexed by [index] *)
  was_down : bool array;  (** what was held when the previous frame asked *)
  held : float array;  (** seconds each control has been held for *)
  mouse : float * float;
      (** how far the mouse moved during this frame, in pixels: what {!Mouse_x}
          and {!Mouse_y} report *)
  pointer : int * int;
      (** where the cursor is. Meaningful only while the game has asked for a
          free cursor — under mouse look it is pinned and says nothing. *)
}
(** What the player is holding, what they have just done, and what the analog
    sources read. Read it through {!val-down}, {!pressed}, {!released},
    {!held_for} and {!value} rather than through the arrays: the layout is an
    implementation detail of the counting above. *)

(** Nothing held, nothing pressed, nothing moved, the cursor at the origin. The
    arrays in it are never written to — {!advance} builds new ones every frame —
    so sharing this one value between runs is safe. *)
let untouched =
  {
    down = Array.make controls false;
    was_down = Array.make controls false;
    held = Array.make controls 0.;
    mouse = (0., 0.);
    pointer = (0, 0);
  }

let down actions control = actions.down.(index control)

(** Whether [control] went down this frame. This is the edge, not the state: it
    is true for exactly one frame however long the control is then held, which
    is what a game wants for anything that should happen once per press. *)
let pressed actions control =
  let i = index control in
  actions.down.(i) && not actions.was_down.(i)

(** Whether [control] came up this frame — the other edge. *)
let released actions control =
  let i = index control in
  (not actions.down.(i)) && actions.was_down.(i)

(** How long [control] has been held, in seconds.

    It counts from the frame {e after} the press, so {!pressed} and a duration
    of zero arrive together, and it keeps its final value for the one frame on
    which {!released} is true. That last part is what lets a game tell a tap
    from a deliberate hold at the moment the control comes up: a door that opens
    on a press and commits on a long hold reads [released] and asks this how
    long the hold lasted. *)
let held_for actions control = actions.held.(index control)

(** What an analog source read over this frame. *)
let value actions = function
  | Mouse_x -> fst actions.mouse
  | Mouse_y -> snd actions.mouse

(** Roll [previous] forward onto what is held at this instant: [down] answers
    for each control, [mouse] is how far the mouse moved, and the frame being
    asked about lasted [dt] seconds.

    This is the whole of the edge detection and the hold timer, and none of it
    touches SDL, so it is where the behaviour is tested. *)
let advance previous ~down ~mouse ~pointer ~dt =
  let now = Array.init controls (fun i -> down (control_of_index i)) in
  let held =
    Array.init controls (fun i ->
        match (now.(i), previous.down.(i)) with
        | true, true -> previous.held.(i) +. dt
        | true, false -> 0.
        (* Held for as long as it was held, for the frame it comes up on. *)
        | false, true -> previous.held.(i)
        | false, false -> 0.)
  in
  { down = now; was_down = previous.down; held; mouse; pointer }

(** [previous] again, with nothing having changed and no time having passed:
    what a frame the window spent out of focus is worth.

    It is {!advance} handed back what was already held, over a frame of zero
    seconds, so the edges and the hold timer stay stated in exactly one place.
    [down] and [was_down] come out equal, so neither {!pressed} nor {!released}
    fires; a control held across the pause keeps the total it had rather than
    growing; and a control let go of while nobody was looking arrives as an
    ordinary {!released} on the first frame the window is back, which is the
    frame a game could have done anything about it.

    The mouse is set back to nothing rather than carried, because unlike the
    controls it is a movement and not a state: keeping the previous frame's
    would report the same inch of desk twice.

    {!Engine.simulate} already stops the clock and drops the motion of an
    unfocused frame. This is the third of the three, and it has to happen where
    the sampling does: by the time [simulate] has the actions the seconds have
    been counted, and no amount of suppression downstream can un-count them. *)
let freeze previous =
  advance previous
    ~down:(fun control -> previous.down.(index control))
    ~mouse:(0., 0.) ~pointer:previous.pointer ~dt:0.

(** Read the keyboard and the mouse buttons as they are now, and roll [previous]
    forward onto them. [mouse] comes in from {!mouse_delta}, which {!Engine}
    calls whether this is reached or not.

    The keyboard array SDL hands back is its own and it changes underneath us,
    so [advance] copies out of it rather than keeping it. The cursor comes out
    in window coordinates; {!Engine} scales it into the framebuffer's, being the
    one that knows how the two compare. *)
let sample previous ~mouse ~dt =
  let keys = Sdl.get_keyboard_state () in
  let held, pointer = Sdl.get_mouse_state () in
  let mask = function
    | Left -> Sdl.Button.lmask
    | Middle -> Sdl.Button.mmask
    | Right -> Sdl.Button.rmask
  in
  let down = function
    | Key key -> Bigarray.Array1.get keys (Key.to_scancode key) = 1
    | Button button -> Int32.logand held (mask button) <> 0l
  in
  advance previous ~down ~mouse ~pointer ~dt

(** Drain the event queue, and say whether the window system asked the program
    to stop while we were reading it — the close button, or Cmd-Q, which reaches
    SDL by the same road.

    Draining is the part that has to happen: the queue must be pumped every
    frame even when nothing in it interests us, or the window stops responding.
    Answering one question about what went past is what the caller gets for it.

    No key is in here, not even the two the engine acts on itself. Fullscreen
    and leaving the run are read as ordinary state from {!Binding}, like
    anything a game binds: {!pressed} is already true for exactly one frame per
    press, which is the whole of what watching for the event and filtering out
    the auto-repeat used to buy. That is why this asks about the window and
    nothing else — what a {e player} presses is {!sample}'s business. *)
let rec quit_requested ?(quit = false) event =
  if not (Sdl.poll_event (Some event)) then quit
  else
    let quit =
      match Sdl.Event.(enum (get event typ)) with `Quit -> true | _ -> quit
    in
    quit_requested ~quit event
