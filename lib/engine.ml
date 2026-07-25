(** Window lifetime and the game loop. Every SDL call can fail, so the whole
    module is written inside the [Result] monad and the first error aborts the
    frame — and with it the program. *)

open Tsdl
open Result_ext

type context = {
  renderer : Sdl.renderer;
  window : Sdl.window;
  world : World.t;
  event : Sdl.event;
  framebuffer : Framebuffer.t ref;
}
(** The things a frame needs that never change during one. The window size is
    deliberately not among them: it can change at any moment, so {!Renderer}
    asks for it per frame and resizes the framebuffer to match. *)

(** Advance the simulation by one frame. Pure: input in, new player out. The
    motion already carries finished per-frame deltas (see {!Input.val-motion}),
    so this only decides the order — turn and pitch before walking, so a frame
    that both turns and moves walks in the direction it ends up facing. *)
let step world player (motion : Input.motion) =
  player
  |> Player.turn ~radians:motion.turn
  |> Player.pitch_by ~delta:motion.pitch
  |> Player.walk world ~forward:motion.forward ~strafe:motion.strafe

(** SDL only offers "set", not "toggle", so the loop carries the current state
    and returns the new one.

    [fullscreen_desktop] stretches the window over the desktop instead of
    changing the display mode: switching is instant, other windows keep their
    places, and nothing has to be restored if we crash. The renderer notices the
    new size on the next frame by itself. *)
let set_fullscreen window enabled =
  let+ () =
    Sdl.set_window_fullscreen window
      (if enabled then Sdl.Window.fullscreen_desktop else Sdl.Window.windowed)
  in
  enabled

(** The clock the loop paces itself by, in seconds. SDL's high resolution
    counter, rather than its millisecond one: a frame is only some sixteen
    milliseconds long, so counting in whole milliseconds would quantise it
    badly. *)
let seconds () =
  Int64.to_float (Sdl.get_performance_counter ())
  /. Int64.to_float (Sdl.get_performance_frequency ())

(** How long the frame starting at [now] should advance the simulation by, given
    that the previous one started at [previous]. Speeds are quoted per second
    (see {!Config}), so measuring the frame is what keeps the player walking at
    the same pace on a machine that renders slowly as on one that races.

    A frame longer than {!Config.max_frame_time} is capped at it. Those come
    from the program being held up rather than from the world moving — the
    window was dragged, the machine swapped — and honouring one would move the
    player further in a single step than any collision test is meant to cope
    with. *)
let frame_time ~previous ~now =
  Float.min Config.max_frame_time (Float.max 0. (now -. previous))

(** What is left of {!Config.frame_budget} for a frame that has spent [spent]
    seconds getting here — the time to sleep before starting the next one. A
    frame that overran its budget gets nothing: it is late already, and
    {!frame_time} has the simulation keep pace with it rather than slow down. *)
let idle_time ~spent = Float.max 0. (Config.frame_budget -. spent)

let rec loop ctx ~player ~fullscreen ~previous =
  let request = Input.poll ctx.event in
  if request.Input.quit then Ok ()
  else
    let* fullscreen =
      if request.Input.toggle_fullscreen then
        set_fullscreen ctx.window (not fullscreen)
      else Ok fullscreen
    in
    let now = seconds () in
    let player =
      step ctx.world player (Input.motion ~dt:(frame_time ~previous ~now))
    in
    let* () = Renderer.render ctx.renderer ctx.framebuffer ctx.world player in
    let idle = idle_time ~spent:(seconds () -. now) in
    Sdl.delay (Int32.of_float (idle *. 1000.));
    (* Frames are timed start to start, so the sleep above counts towards the
       next one's length rather than falling outside every frame. *)
    loop ctx ~player ~fullscreen ~previous:now

(** Acquire a resource, use it, and release it even if the body raises. *)
let with_resource acquire release use =
  let* resource = acquire () in
  Fun.protect ~finally:(fun () -> release resource) (fun () -> use resource)

let run world =
  with_resource
    (fun () -> Sdl.init Sdl.Init.(video + events))
    (fun () -> Sdl.quit ())
  @@ fun () ->
  with_resource
    (fun () ->
      Sdl.create_window Config.window_title ~w:Config.initial_width
        ~h:Config.initial_height
        Sdl.Window.(shown + resizable))
    Sdl.destroy_window
  @@ fun window ->
  with_resource (fun () -> Sdl.create_renderer window) Sdl.destroy_renderer
  @@ fun renderer ->
  (* Relative mouse mode hides and pins the cursor and hands us bare motion
     deltas — what a first person camera wants from the mouse. *)
  with_resource
    (fun () -> Sdl.set_relative_mouse_mode true)
    (fun () -> ignore (Sdl.set_relative_mouse_mode false))
  @@ fun () ->
  (* The framebuffer is resized to the window as it changes, so [ensure] may
     replace the one in the ref; the finaliser frees whichever is current. *)
  let width, height =
    Renderer.internal_size ~width:Config.initial_width
      ~height:Config.initial_height
  in
  let* initial = Framebuffer.create renderer ~width ~height in
  let framebuffer = ref initial in
  Fun.protect
    ~finally:(fun () -> Framebuffer.destroy !framebuffer)
    (fun () ->
      loop
        { renderer; window; world; event = Sdl.Event.create (); framebuffer }
        ~player:(Player.spawn world) ~fullscreen:false ~previous:(seconds ()))
