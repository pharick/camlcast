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

let game ?(overlay = fun _ _ -> ()) ?(pointing = fun _ -> false)
    ?(finished = fun _ -> false) ?(bindings = Binding.default) ~update ~view ()
    =
  { update; view; overlay; pointing; finished; bindings }

type window = {
  handle : Sdl.window;
  renderer : Sdl.renderer;
  event : Sdl.event;
  framebuffer : Framebuffer.t ref;
  mutable fullscreen : bool;
  mutable relative : bool;
}
(* The things a frame needs that do not change during one, and that no longer
    change between runs either. SDL only offers "set" for fullscreen and
    relative mouse mode — never "toggle", and never an answer — so what they are
    is written down here, beside the window they are true of. Mutable, because
    they outlive the loop that changes them: a run started on this window has to
    be told what the last one left, or it would turn the cursor loose and never
    notice.

    Two things are deliberately not in here. The window size can change at any
    moment, so {!Renderer} asks for it per frame and resizes the framebuffer to
    match. And the game is per run rather than per window, so the loop takes it
    alongside rather than holding it fixed here. *)

let move world player (motion : Input.motion) =
  player
  |> Player.turn ~radians:motion.turn
  |> Player.pitch_by ~radians:motion.pitch
  |> Player.traverse world ~forward:motion.forward ~strafe:motion.strafe

let step world player (motion : Input.motion) =
  (move world player motion).Player.player

(* SDL only offers "set", not "toggle", so the window carries the current state
    and this hands back the new one to write there.

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
    drawing the player is pointing at has to be told in the buffer's terms.

    The buffer this is measured against has already been fitted to the window
    this frame — see the call in {!loop} — so the two sides of the ratio are the
    same window's. *)
let in_framebuffer window framebuffer (x, y) =
  let width, height = Sdl.get_window_size window in
  if width <= 0 || height <= 0 then (x, y)
  else
    ( x * framebuffer.Framebuffer.width / width,
      y * framebuffer.Framebuffer.height / height )

let rec loop window game ~state ~actions ~previous =
  (* Drained before anything else and whether or not the window has focus, both
     because a queue nobody pumps stops the window responding and because what
     went past in it is half of the frame's input: a control tapped between two
     frames is only in here. *)
  let queue = Input.drain window.event in
  if Input.closed queue then Ok (state, Closed)
  else
    let now = Clock.now () in
    let dt = Clock.frame_time ~previous ~now in
    (* Drained every frame, focused or not: the delta accumulates until somebody
       reads it, so a paused frame that skipped the read would hand the whole
       idle spell to the frame that resumes. An unfocused frame reads it and
       throws it away, which is what {!Input.freeze} does with it. *)
    let mouse = Input.mouse_delta () in
    let focused = has_focus window.handle in
    (* The window can have changed shape since the last frame, so the buffer is
       fitted to it here and not left to {!Renderer.render} at the end. The
       cursor is put into the buffer's coordinates a few lines below, and a
       buffer resized after that would be a buffer the pointer was never
       measured against: for one frame a game would hit-test the old layout
       while the new one was on the screen. [render] fits again, which is then a
       comparison of two equal sizes and nothing else.

       The fullscreen toggle further down stays where it is, after the read,
       because it is that read that triggers it. So the frame the key is pressed
       on reports the cursor in the layout it was pressed in, which is the
       layout the player was pointing at. *)
    let* () = Renderer.fit window.renderer window.framebuffer in
    (* Read as state while the window has focus, and frozen while it has not:
       the hold timer runs on this [dt] and not on the one {!simulate} zeroes,
       so a frame nobody was there for has to be kept out of it here. The cursor
       is rescaled inside the same branch because {!Input.freeze} carries the
       previous frame's forward, and that one has been scaled already. *)
    let actions =
      if focused then
        let sampled = Input.sample actions queue ~mouse ~dt in
        Input.with_pointer sampled
          (in_framebuffer window.handle !(window.framebuffer)
             (Input.pointer sampled))
      else Input.freeze actions
    in
    (* Walking and looking are read out of the same frame of controls as
       everything else, through the game's table. The engine's part in it is a
       default table and no key of its own. *)
    let bindings = game.bindings in
    let motion = Binding.motion bindings actions ~dt in
    let* () =
      if Binding.taken bindings.Binding.fullscreen actions then
        let+ enabled = set_fullscreen window.handle (not window.fullscreen) in
        window.fullscreen <- enabled
      else Ok ()
    in
    let state =
      simulate game state ~focused ~pointing:(game.pointing state) ~dt ~motion
        ~actions
    in
    if game.finished state then Ok (state, Left)
    else if Binding.taken bindings.Binding.leave actions then Ok (state, Left)
    else
      (* Which of the two things the mouse is for is the game's to say and the
         engine's to carry out, and the state it has just become is the one that
         says it — a screen opened this frame wants its cursor this frame. *)
      let* () =
        let+ enabled =
          set_relative_mouse ~current:window.relative
            (not (game.pointing state))
        in
        window.relative <- enabled
      in
      let world, player = game.view state in
      let* () =
        Renderer.render window.renderer window.framebuffer
          ~overlay:(fun fb -> game.overlay fb state)
          world player
      in
      let idle = Clock.idle_time ~spent:(Clock.now () -. now) in
      Sdl.delay (Int32.of_float (idle *. 1000.));
      (* Frames are timed start to start, so the sleep above counts towards the
         next one's length rather than falling outside every frame. *)
      loop window game ~state ~actions ~previous:now

let with_window ?(title = Config.window_title) ?(width = Config.initial_width)
    ?(height = Config.initial_height) use =
  with_resource
    (fun () -> Sdl.init Sdl.Init.(video + events))
    (fun () -> Sdl.quit ())
  @@ fun () ->
  with_resource
    (fun () ->
      Sdl.create_window title ~w:width ~h:height Sdl.Window.(shown + resizable))
    Sdl.destroy_window
  @@ fun handle ->
  with_resource (fun () -> Sdl.create_renderer handle) Sdl.destroy_renderer
  @@ fun renderer ->
  (* Relative mouse mode hides and pins the cursor and hands us bare motion
     deltas — what a first person camera wants from the mouse. *)
  with_resource
    (fun () -> Sdl.set_relative_mouse_mode true)
    (fun () -> ignore (Sdl.set_relative_mouse_mode false))
  @@ fun () ->
  (* SDL switches text input on when video starts, which leaves the window an
     active text client. On macOS that is what makes press and hold open the
     accent picker over the game instead of repeating the key. Nothing here
     reads typed characters — the keyboard is scancode state — so say so. *)
  Sdl.stop_text_input ();
  (* The framebuffer is resized to the window as it changes, so [ensure] may
     replace the one in the ref; the finaliser frees whichever is current. It
     nests inside the renderer's release above because the texture behind it
     belongs to that renderer and must go first. *)
  let buffer_width, buffer_height = Renderer.internal_size ~width ~height in
  let* initial =
    Framebuffer.make renderer ~width:buffer_width ~height:buffer_height
  in
  let framebuffer = ref initial in
  Fun.protect
    ~finally:(fun () -> Framebuffer.destroy !framebuffer)
    (fun () ->
      use
        {
          handle;
          renderer;
          event = Sdl.Event.create ();
          framebuffer;
          fullscreen = false;
          relative = true;
        })

let run window (game : 'a game) state =
  (* What was held when the last run ended is not a press in this one. Starting
     from {!Input.untouched} would state every key as newly down, so a player
     still holding the one that chose this game would have it read as pressed on
     its first frame; {!Input.freeze} states [down] and [was_down] alike, and no
     edge fires. *)
  let actions =
    Input.freeze
      (Input.sample Input.untouched Input.quiet ~mouse:(0., 0.) ~dt:0.)
  in
  (* The window arrives however the last run left it, and only the game about to
     be played knows what it wants the mouse for. *)
  let* () =
    let+ enabled =
      set_relative_mouse ~current:window.relative (not (game.pointing state))
    in
    window.relative <- enabled
  in
  (* SDL keeps adding up the relative delta until somebody reads it, and between
     two runs nobody was. Read it here and drop it, or every inch of desk
     crossed on the way in would swing the camera on the first frame. After the
     line above and not before it: taking the mouse warps the cursor, and that
     warp is itself a delta waiting to be read. *)
  ignore (Input.mouse_delta ());
  loop window game ~state ~actions ~previous:(Clock.now ())

let grow ?(extend = fun world _ -> world) world player motion =
  let moved = move world player motion in
  let player = moved.Player.player in
  (* Once, on the whole frame. Every frame that crosses nothing is a look at an
     empty list, which is almost all of them; a frame that crosses several is
     still one look and one call, with the pose it finished in. *)
  ((if Player.crossed moved then extend world player else world), player)

let run_world window ?extend
    ?(bindings = Binding.make ~leave:[ Input.Key Key.escape ] ()) world =
  let update (world, player) ~dt:_ ~motion ~actions:_ =
    grow ?extend world player motion
  in
  let+ _, ending =
    run window
      (game ~update ~view:Fun.id ~bindings ())
      (world, Player.spawn world)
  in
  ending
