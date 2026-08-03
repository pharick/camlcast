(* Implementation of {!Camlcast.Input}; the interface carries the prose. *)

open Tsdl

type motion = {
  forward : float;  (** cells to walk along [dir] this frame *)
  strafe : float;  (** cells to walk along [right] this frame *)
  turn : float;  (** radians to rotate this frame, + clockwise *)
  pitch : float;  (** change in view pitch this frame, + looks up *)
}

let still = { forward = 0.; strafe = 0.; turn = 0.; pitch = 0. }

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

let untouched =
  {
    down = Array.make controls false;
    was_down = Array.make controls false;
    held = Array.make controls 0.;
    mouse = (0., 0.);
    pointer = (0, 0);
  }

let down actions control = actions.down.(index control)

let pressed actions control =
  let i = index control in
  actions.down.(i) && not actions.was_down.(i)

let released actions control =
  let i = index control in
  (not actions.down.(i)) && actions.was_down.(i)

let held_for actions control = actions.held.(index control)

let value actions = function
  | Mouse_x -> fst actions.mouse
  | Mouse_y -> snd actions.mouse

let pointer actions = actions.pointer
let with_pointer actions pointer = { actions with pointer }

let advance ?(tapped = fun _ -> false) previous ~down ~mouse ~pointer ~dt =
  let now =
    Array.init controls (fun i ->
        let control = control_of_index i in
        down control || tapped control)
  in
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

module Runtime = struct
  let mouse_delta () =
    let _buttons, (dx, dy) = Sdl.get_relative_mouse_state () in
    (float_of_int dx, float_of_int dy)

  let freeze previous =
    advance previous
      ~down:(fun control -> previous.down.(index control))
      ~mouse:(0., 0.) ~pointer:previous.pointer ~dt:0.

  type queue = {
    quit : bool;  (** whether the window system asked the program to stop *)
    tapped : bool array;
        (** which controls went down while the queue was being read, indexed by
            [index] *)
  }

  let quiet = { quit = false; tapped = Array.make controls false }
  let closed queue = queue.quit

  let drain event =
    let tapped = Array.make controls false in
    (* SDL is not trusted to report a scancode inside the range the flat array was
       sized for, for the same reason {!Key.of_scancode} range-checks: the key at
       [Key.count] would be the left mouse button and would be believed. *)
    let mark_key scancode =
      if scancode >= 0 && scancode < Key.count then
        tapped.(first_key + scancode) <- true
    in
    let mark_button button =
      if button = Sdl.Button.left then tapped.(first_button + 0) <- true
      else if button = Sdl.Button.middle then tapped.(first_button + 1) <- true
      else if button = Sdl.Button.right then tapped.(first_button + 2) <- true
    in
    let rec pump quit =
      if not (Sdl.poll_event (Some event)) then quit
      else
        let quit =
          match Sdl.Event.(enum (get event typ)) with
          | `Quit -> true
          | `Key_down ->
              mark_key Sdl.Event.(get event keyboard_scancode);
              quit
          | `Mouse_button_down ->
              mark_button Sdl.Event.(get event mouse_button_button);
              quit
          | _ -> quit
        in
        pump quit
    in
    let quit = pump false in
    { quit; tapped }

  let sample previous queue ~mouse ~dt =
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
    let tapped control = queue.tapped.(index control) in
    advance ~tapped previous ~down ~mouse ~pointer ~dt
end
