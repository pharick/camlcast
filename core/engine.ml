(* Implementation of {!Camlcast.Engine}; the interface carries the prose. *)

open Tsdl
open Result_ext

type ending = Closed | Returned

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
    change between runs either. SDL offers no {e toggle} for fullscreen or
    relative mouse mode, only a set, so flipping one requires the current value
    in hand. What is written here is the value that came back from the last set
    rather than the one asked for, which is the part that matters when a
    desktop refuses. Mutable, because they outlive the loop that changes them:
    a run started on this window has to be told what the last one left, or it
    would turn the cursor loose without noticing.

    {b SDL would answer both questions if asked}, and this comment used to
    claim otherwise. [Sdl.get_relative_mouse_mode] returns a bool, and
    fullscreen is a flag in [Sdl.get_window_flags], which [has_focus] below
    already calls — so the claim was contradicted twenty lines further down
    this file. These fields are a cache of an answer SDL has. They are kept
    because the alternative is two queries per frame on the path that runs
    every frame, and because the fullscreen flag is really two ([fullscreen]
    and [fullscreen_desktop]) and only one of them is the mode this engine ever
    sets. A maintainer who reads the old reason and goes looking for the
    missing getter will find it.

    Two things are deliberately not in here. The window size can change at any
    moment, so {!Renderer} asks for it per frame and resizes the framebuffer to
    match. And the game is per run rather than per window, so the loop takes it
    as an argument rather than holding it fixed here. *)

let move world player (motion : Input.motion) =
  player
  |> Player.turn ~radians:motion.turn
  |> Player.pitch_by ~fraction:motion.pitch
  |> Player.traverse world ~forward:motion.forward ~strafe:motion.strafe

let step world player (motion : Input.motion) =
  (move world player motion).Player.player

(* SDL only offers "set", not "toggle", so the window carries the current state
    and this hands back the new one to write there.

    [fullscreen_desktop] stretches the window over the desktop instead of
    changing the display mode: switching is instant, other windows keep their
    places, and nothing has to be restored after a crash. The renderer notices
    the new size on the next frame by itself.

    The mode is asked for and not required: the answer is what holds
    afterwards, not whether the asking worked. A window manager is free to
    refuse this, and a tiling one routinely does. A game that failed there
    would quit when the player presses a key some desktops do not honour. On
    refusal the window keeps the size it already had, [current] comes back,
    and the run carries on in it. See {!with_window} for where the line is. *)
let set_fullscreen window ~current enabled =
  match
    Sdl.set_window_fullscreen window
      (if enabled then Sdl.Window.fullscreen_desktop else Sdl.Window.windowed)
  with
  | Ok () -> enabled
  | Error _ -> current

(* The same again for relative mouse mode, which pins the cursor out of sight
    and hands the camera bare deltas. Releasing it puts a real cursor back on
    the screen, which is what a game wants while the player is pointing at
    something it has drawn. Setting it to what it already is is not free (SDL
    warps the cursor), so [current] is checked first.

    Best-effort on the same terms, and the case for it is stronger. A
    compositor that will not hand over the pointer leaves a game whose mouse
    look is worse: {!Input.Runtime.mouse_delta} still reports motion, now
    stopping at the edges of the screen. A game that never wanted the pointer
    in the first place is entirely unaffected. Neither is a reason to fail.

    On refusal, [current] comes back unchanged, so the next frame that still
    wants the other state asks again: one SDL call a frame on a desktop that
    keeps refusing, and a pointer that is taken the moment one stops. *)
let set_relative_mouse ~current enabled =
  if enabled = current then current
  else
    match Sdl.set_relative_mouse_mode enabled with
    | Ok () ->
        (* Taking the pointer warps the cursor, and giving it back puts it
           where it was. SDL counts either as motion and adds it to the delta
           nobody has read yet, where the next frame finds it and turns it into
           a look. The delta is dropped here rather than at the call sites,
           because the rule belongs to the thing that causes it: {!run} knew to
           do this after capturing the mouse and the loop did not, so closing
           an in-game screen swung the camera on the frame after, by however
           far the cursor had been left from the middle of the window.

           Dropped on the change and not on every call, since a mode set to
           what it already is neither warps nor is asked for. *)
        ignore (Input.Runtime.mouse_delta ());
        enabled
    | Error _ -> current

(* Whether the window has keyboard focus. A window that has lost focus is
    behind another one or on another desktop, and no one is controlling what
    it shows. *)
let has_focus window =
  Sdl.Window.(test (Sdl.get_window_flags window) input_focus)

let simulate game state ~focused ~pointing ~dt ~motion ~actions =
  let dt = if focused then dt else 0. in
  let motion = if focused && not pointing then motion else Input.still in
  game.update state ~dt ~motion ~actions

(* Where the cursor is in the coordinates the overlay draws in. SDL reports it
    in window coordinates, and the framebuffer is a fraction of that size (see
    {!Renderer.internal_size}), so a game hit-testing its own drawing against
    the cursor has to be told the cursor in the buffer's terms.

    The buffer this is measured against has already been fitted to the window
    this frame (see the call in {!loop}), so the two sides of the ratio belong
    to the same window. *)
let in_framebuffer window framebuffer (x, y) =
  let width, height = Sdl.get_window_size window in
  if width <= 0 || height <= 0 then (x, y)
  else
    ( x * framebuffer.Framebuffer.width / width,
      y * framebuffer.Framebuffer.height / height )

let rec loop window game ~state ~actions ~previous =
  (* Drained before anything else, focused or not, for two reasons: a queue
     nobody pumps stops the window responding, and what went past in it is half
     of the frame's input — a control tapped between two frames is only in
     here. *)
  let queue = Input.Runtime.drain window.event in
  if Input.Runtime.closed queue then Ok (state, Closed)
  else
    let now = Clock.now () in
    let dt = Clock.frame_time ~previous ~now in
    (* Drained every frame, focused or not: the delta accumulates until
       somebody reads it, so a paused frame that skipped the read would hand
       the whole idle period to the frame that resumes. An unfocused frame
       reads it and discards it, which is what {!Input.Runtime.freeze} does
       with it. *)
    let mouse = Input.Runtime.mouse_delta () in
    let focused = has_focus window.handle in
    (* The window can have changed shape since the last frame, so the buffer is
       fitted to it here and not left to {!Renderer.render} at the end. The
       cursor is put into the buffer's coordinates a few lines below, and a
       buffer resized after that would be a buffer the pointer was never
       measured against: for one frame a game would hit-test the old layout
       while the new one was on the screen. [render] fits again, which is then
       a comparison of two equal sizes and nothing else.

       The fullscreen toggle further down stays after the input read, because
       that read is what triggers it. So the frame the key is pressed on
       reports the cursor in the layout it was pressed in, which is the layout
       the player was pointing at. *)
    let* () = Renderer.fit window.renderer window.framebuffer in
    (* Read as state while the window has focus, and frozen while it has not:
       the hold timer runs on this [dt] and not on the one {!simulate} zeroes,
       so a frame nobody was there for has to be kept out of it here.

       The cursor is converted inside the same branch, and only there, to keep
       the invariant that {e every} pointer in an [actions] is already in the
       buffer's coordinates. SDL reports it in the window's, so the frame that
       reads it is the frame that converts it, and {!Input.Runtime.freeze} only
       carries along one that was converted when it was read. {!run} converts
       the frame it seeds the loop with for that reason and no other: it is
       the one pointer here that does not come from the branch below. *)
    let actions =
      if focused then
        let sampled = Input.Runtime.sample actions queue ~mouse ~dt in
        Input.with_pointer sampled
          (in_framebuffer window.handle !(window.framebuffer)
             (Input.pointer sampled))
      else Input.Runtime.freeze actions
    in
    (* Walking and looking are read out of the same frame of controls as
       everything else, through the game's binding table. The engine supplies
       a default table and owns no key of its own. *)
    let bindings = game.bindings in
    let motion = Binding.motion bindings actions ~dt in
    if Binding.taken bindings.Binding.fullscreen actions then
      window.fullscreen <-
        set_fullscreen window.handle ~current:window.fullscreen
          (not window.fullscreen);
    let state =
      simulate game state ~focused ~pointing:(game.pointing state) ~dt ~motion
        ~actions
    in
    (* Checked before the frame is drawn because leaving is a thing the player
       did with the controls, and this is where the controls are read. The
       game's own end condition is checked further down, after the drawing. *)
    if Binding.taken bindings.Binding.leave actions then Ok (state, Returned)
    else begin
      (* Whether the mouse looks or points is the game's to say and the
         engine's to carry out. The state it has just become is the one that
         says it, because a screen opened this frame wants its cursor this
         frame. *)
      window.relative <-
        set_relative_mouse ~current:window.relative (not (game.pointing state));
      let world, player = game.view state in
      let* () =
        Renderer.render window.renderer window.framebuffer
          ~overlay:(fun fb -> game.overlay fb state)
          world player
      in
      (* Checked after the frame has been presented, so the state a game ends
         on is the last one the player sees rather than one nobody was shown.
         A game says it is over by describing an ending; checking before the
         drawing would stop the loop with that ending frame never drawn. *)
      if game.finished state then Ok (state, Returned)
      else begin
        let idle = Clock.idle_time ~spent:(Clock.now () -. now) in
        Sdl.delay (Int32.of_float (idle *. 1000.));
        (* Frames are timed start to start, so the sleep above counts towards
           the next one's length rather than falling outside every frame. *)
        loop window game ~state ~actions ~previous:now
      end
    end

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
  (* Relative mouse mode hides and pins the cursor and hands over bare motion
     deltas, which is what a first person camera wants from the mouse. It is
     asked for here rather than acquired as a resource. Asking now suits most
     games and saves the first frame a warp, but a window does not depend on
     it, and as a resource it made a desktop that will not give up the pointer
     a desktop with no window at all — including for a game that frees the
     cursor on its first frame and never asks again. What was actually granted
     is written down, and {!run} settles it per run and the loop per frame
     from what the game says.

     The release is unconditional and its failure ignored, as before: giving
     back a pointer that was never granted is a no-op, and the one thing worse
     than not having the cursor is exiting with it still captured. *)
  let relative = set_relative_mouse ~current:false true in
  Fun.protect ~finally:(fun () -> ignore (Sdl.set_relative_mouse_mode false))
  @@ fun () ->
  (* SDL switches text input on when video starts, which leaves the window an
     active text client. On macOS that is what makes press and hold open the
     accent picker over the game instead of repeating the key. Nothing here
     reads typed characters (the keyboard is scancode state), so text input is
     switched off. *)
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
          relative;
        })

let run window (game : 'a game) state =
  (* What was held when the last run ended is not a press in this one. Starting
     from {!Input.untouched} would mark every held key as newly down, so a
     player still holding the key that chose this game would have it read as
     pressed on its first frame. {!Input.Runtime.freeze} sets [down] and
     [was_down] alike, and no edge fires. *)
  let actions =
    Input.Runtime.freeze
      (Input.Runtime.sample Input.untouched Input.Runtime.quiet ~mouse:(0., 0.)
         ~dt:0.)
  in
  (* And into the buffer's coordinates, which is where {!Input.pointer} says a
     cursor is and where the loop puts every one it reads. Not a formality: the
     loop only converts on the frames it samples, because
     {!Input.Runtime.freeze} hands the previous frame's pointer along and that
     one was converted when it was read. A seed left in the window's
     coordinates would be the exception that breaks the invariant, and it
     would last as long as the window stayed unfocused, since every unfocused
     frame freezes it forward again. A run that starts behind another window
     and is clicked into is the ordinary way to hit this, and it would see a
     cursor reported some whole multiple too far out. *)
  let actions =
    Input.with_pointer actions
      (in_framebuffer window.handle !(window.framebuffer)
         (Input.pointer actions))
  in
  (* The window arrives however the last run left it, and only the game about to
     be played knows what it wants the mouse for. *)
  window.relative <-
    set_relative_mouse ~current:window.relative (not (game.pointing state));
  (* SDL keeps accumulating the relative delta until somebody reads it, and
     between two runs nobody did. Read it here and drop it, or all the mouse
     motion made on the way in would swing the camera on the first frame.

     This stays even after the line above, which drops a delta of its own:
     that drop only happens if the mouse mode changed, and a run that inherits
     the window in the state it wants is exactly a run where it did not. *)
  ignore (Input.Runtime.mouse_delta ());
  loop window game ~state ~actions ~previous:(Clock.now ())

let grow ?(extend = fun world _ -> world) world player motion =
  let moved = move world player motion in
  let player = moved.Player.player in
  (* Called once, on the whole frame. Every frame that crosses nothing is one
     check of an empty list, which is almost all of them; a frame that crosses
     several is still one check and one call, with the pose it finished in. *)
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
