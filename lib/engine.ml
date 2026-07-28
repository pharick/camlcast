(* Implementation of {!Camlcast.Engine}; the interface carries the prose. *)

open Tsdl
open Result_ext

type ending = Closed | Left

type 'a game = {
  update : 'a -> dt:float -> motion:Input.motion -> actions:Input.actions -> 'a;
  view : 'a -> World.t * Player.t;
  overlay : Framebuffer.t -> 'a -> unit;
  pointing : 'a -> bool;
  finished : 'a -> bool;
  bindings : Binding.t;
}

type 'a context = {
  renderer : Sdl.renderer;
  window : Sdl.window;
  event : Sdl.event;
  framebuffer : Framebuffer.t ref;
  game : 'a game;
}
(* The things a frame needs that never change during one. Two are deliberately
    not among them. The window size can change at any moment, so {!Renderer}
    asks for it per frame and resizes the framebuffer to match. And the game
    state changes every frame by definition, so the loop threads it beside the
    context rather than holding it fixed here. *)

let move world player (motion : Input.motion) =
  player
  |> Player.turn ~radians:motion.turn
  |> Player.pitch_by ~radians:motion.pitch
  |> Player.traverse world ~forward:motion.forward ~strafe:motion.strafe

let step world player (motion : Input.motion) =
  (move world player motion).Player.player

(* SDL only offers "set", not "toggle", so the loop carries the current state
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

(* The same again for relative mouse mode, which pins the cursor out of sight
    and hands the camera bare deltas. Releasing it puts a real cursor back on
    the screen — what a game wants while the player is pointing at something it
    has drawn. Setting it to what it already is is not free (SDL warps the
    cursor), so [current] is checked first. *)
let set_relative_mouse ~current enabled =
  if enabled = current then Ok current
  else
    let+ () = Sdl.set_relative_mouse_mode enabled in
    enabled

(* Whether the window is the one the keyboard is talking to. A window that has
    lost focus is behind another one or on another desktop, and nobody is at the
    controls of what it shows. *)
let has_focus window =
  Sdl.Window.(test (Sdl.get_window_flags window) input_focus)

let simulate game state ~focused ~pointing ~dt ~motion ~actions =
  let dt = if focused then dt else 0. in
  let motion = if focused && not pointing then motion else Input.still in
  game.update state ~dt ~motion ~actions

(* Where the cursor is in the coordinates the overlay draws in. SDL reports it
    in window coordinates, and the framebuffer is a fraction of that size (see
    {!Renderer.internal_size}), so a game that wants to know what its own
    drawing the player is pointing at has to be told in the buffer's terms. *)
let in_framebuffer window framebuffer (x, y) =
  let width, height = Sdl.get_window_size window in
  if width <= 0 || height <= 0 then (x, y)
  else
    ( x * framebuffer.Framebuffer.width / width,
      y * framebuffer.Framebuffer.height / height )

let rec loop ctx ~state ~actions ~fullscreen ~relative ~previous =
  if Input.quit_requested ctx.event then Ok (state, Closed)
  else
    let now = Clock.now () in
    let dt = Clock.frame_time ~previous ~now in
    (* Drained every frame, focused or not: the delta accumulates until somebody
       reads it, so a paused frame that skipped the read would hand the whole
       idle spell to the frame that resumes. An unfocused frame reads it and
       throws it away, which is what {!Input.freeze} does with it. *)
    let mouse = Input.mouse_delta () in
    let focused = has_focus ctx.window in
    (* Read as state while the window has focus, and frozen while it has not:
       the hold timer runs on this [dt] and not on the one {!simulate} zeroes,
       so a frame nobody was there for has to be kept out of it here. The cursor
       is rescaled inside the same branch because {!Input.freeze} carries the
       previous frame's forward, and that one has been scaled already. *)
    let actions =
      if focused then
        let sampled = Input.sample actions ~mouse ~dt in
        {
          sampled with
          Input.pointer =
            in_framebuffer ctx.window !(ctx.framebuffer) sampled.Input.pointer;
        }
      else Input.freeze actions
    in
    (* Walking and looking are read out of the same frame of controls as
       everything else, through the game's table. The engine's part in it is a
       default table and no key of its own. *)
    let bindings = ctx.game.bindings in
    let motion = Binding.motion bindings actions ~dt in
    let* fullscreen =
      if Binding.taken bindings.Binding.fullscreen actions then
        set_fullscreen ctx.window (not fullscreen)
      else Ok fullscreen
    in
    let state =
      simulate ctx.game state ~focused ~pointing:(ctx.game.pointing state) ~dt
        ~motion ~actions
    in
    if ctx.game.finished state then Ok (state, Left)
    else if Binding.taken bindings.Binding.leave actions then Ok (state, Left)
    else
      (* Which of the two things the mouse is for is the game's to say and the
         engine's to carry out, and the state it has just become is the one that
         says it — a screen opened this frame wants its cursor this frame. *)
      let* relative =
        set_relative_mouse ~current:relative (not (ctx.game.pointing state))
      in
      let world, player = ctx.game.view state in
      let* () =
        Renderer.render ctx.renderer ctx.framebuffer
          ~overlay:(fun fb -> ctx.game.overlay fb state)
          world player
      in
      let idle = Clock.idle_time ~spent:(Clock.now () -. now) in
      Sdl.delay (Int32.of_float (idle *. 1000.));
      (* Frames are timed start to start, so the sleep above counts towards the
         next one's length rather than falling outside every frame. *)
      loop ctx ~state ~actions ~fullscreen ~relative ~previous:now

let run ~update ~view ?(overlay = fun _ _ -> ()) ?(pointing = fun _ -> false)
    ?(finished = fun _ -> false) ?(bindings = Binding.default) state =
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
        {
          renderer;
          window;
          event = Sdl.Event.create ();
          framebuffer;
          game = { update; view; overlay; pointing; finished; bindings };
        }
        ~state ~actions:Input.untouched ~fullscreen:false ~relative:true
        ~previous:(Clock.now ()))

let run_world ?(extend = fun world _ -> world)
    ?(bindings = Binding.make ~leave:[ Input.Key Key.escape ] ()) world =
  let update (world, player) ~dt:_ ~motion ~actions:_ =
    let moved = move world player motion in
    let player = moved.Player.player in
    (* Every frame that crosses nothing is a look at an empty list, which is
       almost all of them. *)
    ((if Player.crossed moved then extend world player else world), player)
  in
  let+ _, ending =
    run ~update ~view:Fun.id ~bindings (world, Player.spawn world)
  in
  ending
