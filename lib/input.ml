(** Turns SDL input into engine-level intent. Nothing here knows about positions
    or walls: it only reports what the user asked for. *)

open Tsdl

type motion = {
  forward : float;  (** cells to walk along [dir] this frame *)
  strafe : float;  (** cells to walk along [right] this frame *)
  turn : float;  (** radians to rotate this frame, + clockwise *)
  pitch : float;  (** change in view pitch this frame, + looks up *)
}

let still = { forward = 0.; strafe = 0.; turn = 0.; pitch = 0. }

(** How far the mouse moved since the last call. Relative mouse mode (enabled by
    {!Engine}) reports these deltas and keeps the cursor pinned, so it never
    runs into a screen edge. *)
let mouse_delta () =
  let _buttons, (dx, dy) = Sdl.get_relative_mouse_state () in
  (float_of_int dx, float_of_int dy)

(** What the user is asking for over a frame of [dt] seconds. Movement is read
    from the keyboard as a state snapshot — holding a key should move
    continuously, and events would only report the moment it was pressed — while
    looking around blends the mouse motion in with the arrow keys. Each field
    comes out as a finished per-frame delta, so {!Engine} can apply it straight
    to the {!Player}.

    Held keys ask for a speed, in cells or radians per second (see {!Config}),
    and are scaled by [dt] into the distance that speed covers over this frame:
    the player walks at the same pace whether the machine renders 30 frames a
    second or 300. The mouse needs no such scaling — its deltas are already
    everything that has happened since the last frame. *)
let motion ~dt =
  let keys = Sdl.get_keyboard_state () in
  let down scancode = Bigarray.Array1.get keys scancode = 1 in
  (* An axis comes out as the distance its speed covers over the frame. *)
  let axis ~speed ~positive ~negative =
    let held scancodes = if List.exists down scancodes then 1. else 0. in
    (held positive -. held negative) *. speed *. dt
  in
  let mouse_dx, mouse_dy = mouse_delta () in
  {
    forward =
      axis ~speed:Config.move_speed
        ~positive:Sdl.Scancode.[ w ]
        ~negative:Sdl.Scancode.[ s ];
    strafe =
      axis ~speed:Config.move_speed
        ~positive:Sdl.Scancode.[ d ]
        ~negative:Sdl.Scancode.[ a ];
    turn =
      axis ~speed:Config.rot_speed
        ~positive:Sdl.Scancode.[ right ]
        ~negative:Sdl.Scancode.[ left ]
      +. (mouse_dx *. Config.look_sensitivity);
    (* Mouse up is a negative delta but should look up, hence the subtraction. *)
    pitch =
      axis ~speed:Config.pitch_speed
        ~positive:Sdl.Scancode.[ up ]
        ~negative:Sdl.Scancode.[ down ]
      -. (mouse_dy *. Config.pitch_sensitivity);
  }

type button =
  | Left
  | Middle
  | Right
      (** The mouse buttons a game may bind. SDL knows about more; these are the
          three every mouse has. *)

type control =
  | Key of Sdl.scancode
  | Button of button
      (** Something the player can hold down. Keys are named by [Sdl.Scancode] —
          scancodes and not keycodes, so a binding is a place on the keyboard
          rather than a letter, and it stays where it is under a different
          layout.

          The engine has no opinion about what any of them mean. "Interact",
          "chalk", "journal" are a game's words for its own table from controls
          to actions; what the engine owns is the mechanism underneath — when a
          control went down, when it came up, and how long it was held. *)

(* Controls are counted off into one flat range so that a frame's worth of them
   is three arrays and an index, rather than a lookup structure. The keyboard
   occupies its own scancodes and the buttons follow it. *)
let controls = Sdl.Scancode.num_scancodes + 3
let button_index = function Left -> 0 | Middle -> 1 | Right -> 2

let index = function
  | Key scancode -> scancode
  | Button button -> Sdl.Scancode.num_scancodes + button_index button

let control_of_index i =
  if i < Sdl.Scancode.num_scancodes then Key i
  else
    Button
      (match i - Sdl.Scancode.num_scancodes with
      | 0 -> Left
      | 1 -> Middle
      | _ -> Right)

type actions = {
  down : bool array;  (** what is held now, indexed by [index] *)
  was_down : bool array;  (** what was held when the previous frame asked *)
  held : float array;  (** seconds each control has been held for *)
  pointer : int * int;
      (** where the cursor is. Meaningful only while the game has asked for a
          free cursor — under mouse look it is pinned and says nothing. *)
}
(** What the player is holding, and what they have just done. Read it through
    {!val-down}, {!pressed}, {!released} and {!held_for} rather than through the
    arrays: the layout is an implementation detail of the counting above. *)

(** Nothing held, nothing pressed, the cursor at the origin. The arrays in it
    are never written to — {!advance} builds new ones every frame — so sharing
    this one value between runs is safe. *)
let untouched =
  {
    down = Array.make controls false;
    was_down = Array.make controls false;
    held = Array.make controls 0.;
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

(** Roll [previous] forward onto what is held at this instant: [down] answers
    for each control, and the frame being asked about lasted [dt] seconds.

    This is the whole of the edge detection and the hold timer, and none of it
    touches SDL, so it is where the behaviour is tested. *)
let advance previous ~down ~pointer ~dt =
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
  { down = now; was_down = previous.down; held; pointer }

(** [previous] again, with nothing having changed and no time having passed:
    what a frame the window spent out of focus is worth.

    It is {!advance} handed back what was already held, over a frame of zero
    seconds, so the edges and the hold timer stay stated in exactly one place.
    [down] and [was_down] come out equal, so neither {!pressed} nor {!released}
    fires; a control held across the pause keeps the total it had rather than
    growing; and a control let go of while nobody was looking arrives as an
    ordinary {!released} on the first frame the window is back, which is the
    frame a game could have done anything about it.

    {!Engine.simulate} already stops the clock and drops the motion of an
    unfocused frame. This is the third of the three, and it has to happen where
    the sampling does: by the time [simulate] has the actions the seconds have
    been counted, and no amount of suppression downstream can un-count them. *)
let freeze previous =
  advance previous
    ~down:(fun control -> previous.down.(index control))
    ~pointer:previous.pointer ~dt:0.

(** Read the keyboard and the mouse buttons as they are now, and roll [previous]
    forward onto them.

    The keyboard array SDL hands back is its own and it changes underneath us,
    so [advance] copies out of it rather than keeping it. The cursor comes out
    in window coordinates; {!Engine} scales it into the framebuffer's, being the
    one that knows how the two compare. *)
let sample previous ~dt =
  let keys = Sdl.get_keyboard_state () in
  let buttons, pointer = Sdl.get_mouse_state () in
  let mask = function
    | Left -> Sdl.Button.lmask
    | Middle -> Sdl.Button.mmask
    | Right -> Sdl.Button.rmask
  in
  let down = function
    | Key scancode -> Bigarray.Array1.get keys scancode = 1
    | Button button -> Int32.logand buttons (mask button) <> 0l
  in
  advance previous ~down ~pointer ~dt

type request = { quit : bool; toggle_fullscreen : bool }
(** What the window system has asked of the program this frame, as opposed to
    what the player has asked of the game. Both of these are the engine's to act
    on; everything a game binds is read as state instead, by {!sample}. *)

let nothing = { quit = false; toggle_fullscreen = false }

(** Drain the event queue into a single request. The queue has to be pumped
    every frame even when nothing in it interests us, otherwise the window stops
    responding.

    Escape is not in here. It is a key like any other as far as the engine is
    concerned, because what it means is a game's to decide — closing a screen
    when one is open and quitting when none is (see {!Engine.run}, which is
    where the demo's own "Escape quits" lives). *)
let rec poll ?(request = nothing) event =
  if not (Sdl.poll_event (Some event)) then request
  else
    let request =
      match Sdl.Event.(enum (get event typ)) with
      | `Quit -> { request with quit = true }
      (* Holding a key down repeats it many times a second; a toggle must
         only see the press that started it. *)
      | `Key_down when Sdl.Event.(get event keyboard_repeat) = 0 ->
          if Sdl.Event.(get event keyboard_scancode) = Sdl.Scancode.f11 then
            (* Two presses in one frame cancel out, as they should. *)
            { request with toggle_fullscreen = not request.toggle_fullscreen }
          else request
      | _ -> request
    in
    poll ~request event
