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

let rec loop ctx ~player ~fullscreen =
  let request = Input.poll ctx.event in
  if request.Input.quit then Ok ()
  else
    let* fullscreen =
      if request.Input.toggle_fullscreen then
        set_fullscreen ctx.window (not fullscreen)
      else Ok fullscreen
    in
    let player = step ctx.world player (Input.motion ()) in
    let* () = Renderer.render ctx.renderer ctx.framebuffer ctx.world player in
    Sdl.delay Config.frame_delay;
    loop ctx ~player ~fullscreen

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
        ~player:(Player.spawn world) ~fullscreen:false)
