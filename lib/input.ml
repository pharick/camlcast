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

(** What the user is asking for this frame. Movement is read from the keyboard
    as a state snapshot — holding a key should move continuously, and events
    would only report the moment it was pressed — while looking around blends
    the mouse motion in with the arrow keys. Each field comes out as a finished
    per-frame delta, already scaled by its {!Config} speed, so {!Engine} can
    apply it straight to the {!Player}. *)
let motion () =
  let keys = Sdl.get_keyboard_state () in
  let down scancode = Bigarray.Array1.get keys scancode = 1 in
  let axis ~positive ~negative =
    let held scancodes = if List.exists down scancodes then 1. else 0. in
    held positive -. held negative
  in
  let mouse_dx, mouse_dy = mouse_delta () in
  {
    forward =
      axis ~positive:Sdl.Scancode.[ w ] ~negative:Sdl.Scancode.[ s ]
      *. Config.move_speed;
    strafe =
      axis ~positive:Sdl.Scancode.[ d ] ~negative:Sdl.Scancode.[ a ]
      *. Config.move_speed;
    turn =
      axis ~positive:Sdl.Scancode.[ right ] ~negative:Sdl.Scancode.[ left ]
      *. Config.rot_speed
      +. (mouse_dx *. Config.look_sensitivity);
    (* Mouse up is a negative delta but should look up, hence the subtraction. *)
    pitch =
      axis ~positive:Sdl.Scancode.[ up ] ~negative:Sdl.Scancode.[ down ]
      *. Config.pitch_speed
      -. (mouse_dy *. Config.pitch_sensitivity);
  }

type request = { quit : bool; toggle_fullscreen : bool }
(** One-off actions the event queue asked for this frame, as opposed to the
    continuous state {!val-motion} reads. *)

let nothing = { quit = false; toggle_fullscreen = false }

(** Drain the event queue into a single request. The queue has to be pumped
    every frame even when nothing in it interests us, otherwise the window stops
    responding. *)
let rec poll ?(request = nothing) event =
  if not (Sdl.poll_event (Some event)) then request
  else
    let request =
      match Sdl.Event.(enum (get event typ)) with
      | `Quit -> { request with quit = true }
      (* Holding a key down repeats it many times a second; a toggle must
         only see the press that started it. *)
      | `Key_down when Sdl.Event.(get event keyboard_repeat) = 0 ->
          let key = Sdl.Event.(get event keyboard_scancode) in
          if key = Sdl.Scancode.escape then { request with quit = true }
          else if key = Sdl.Scancode.f11 then
            (* Two presses in one frame cancel out, as they should. *)
            { request with toggle_fullscreen = not request.toggle_fullscreen }
          else request
      | _ -> request
    in
    poll ~request event
