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
