(** Window lifetime and the game loop. Every SDL call can fail, so the whole
    module is written inside the [Result] monad and the first error aborts the
    frame — and with it the program. *)

open Tsdl
open Result_ext

type 'a game = {
  update : 'a -> dt:float -> motion:Input.motion -> actions:Input.request -> 'a;
      (** the next state, given how long the frame lasted, the movement asked
          for over it and the one-off actions that arrived with it *)
  view : 'a -> World.t * Player.t;  (** what this frame is drawn from *)
  overlay : Framebuffer.t -> 'a -> unit;
      (** anything drawn over the finished world, before it reaches the screen *)
  finished : 'a -> bool;  (** asked after every update; [true] ends the run *)
}
(** How the loop reaches a game whose state it knows nothing else about. ['a] is
    the game's own — phases, doors, journal, whatever it keeps — and the engine
    only ever hands it back to these four functions. What it needs from a game
    is small: something to advance, a world and a player to draw it from, an
    optional layer over the top, and an answer to whether it is over. *)

type 'a context = {
  renderer : Sdl.renderer;
  window : Sdl.window;
  event : Sdl.event;
  framebuffer : Framebuffer.t ref;
  game : 'a game;
}
(** The things a frame needs that never change during one. Two are deliberately
    not among them. The window size can change at any moment, so {!Renderer}
    asks for it per frame and resizes the framebuffer to match. And the game
    state changes every frame by definition, so the loop threads it beside the
    context rather than holding it fixed here. *)

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

(** Whether the window is the one the keyboard is talking to. A window that has
    lost focus is behind another one or on another desktop, and nobody is at the
    controls of what it shows. *)
let has_focus window =
  Sdl.Window.(test (Sdl.get_window_flags window) input_focus)

(** Advance the game by one frame, with nothing drawn: this is everything the
    loop does between reading the input and rendering the result, and it is a
    pure function of the state and that input.

    An unfocused window advances by nothing at all. Without this, a game left
    behind another window for a minute would be handed that minute the moment it
    came back — a minute of burnt lamp oil, of whatever else runs on the clock,
    none of which the player was there for. The loop keeps timing frames while
    paused, so focus returns at an ordinary frame's length rather than a jump.

    Motion is dropped rather than scaled down by the zero [dt], because the
    mouse is not scaled by [dt] in the first place: its deltas are already
    everything that has happened since the last read (see {!Input.val-motion}),
    so a paused frame that passed them on would still turn the camera. *)
let simulate game state ~focused ~dt ~motion ~actions =
  let dt = if focused then dt else 0. in
  let motion = if focused then motion else Input.still in
  game.update state ~dt ~motion ~actions

let rec loop ctx ~state ~fullscreen ~previous =
  let request = Input.poll ctx.event in
  if request.Input.quit then Ok state
  else
    let* fullscreen =
      if request.Input.toggle_fullscreen then
        set_fullscreen ctx.window (not fullscreen)
      else Ok fullscreen
    in
    let now = seconds () in
    let dt = frame_time ~previous ~now in
    (* The mouse is read every frame, focused or not: its delta accumulates
       until somebody reads it, so a paused frame that skipped the read would
       hand the whole idle spell to the frame that resumes. *)
    let motion = Input.motion ~dt in
    let state =
      simulate ctx.game state
        ~focused:(has_focus ctx.window)
        ~dt ~motion ~actions:request
    in
    if ctx.game.finished state then Ok state
    else
      let world, player = ctx.game.view state in
      let* () =
        Renderer.render ctx.renderer ctx.framebuffer
          ~overlay:(fun fb -> ctx.game.overlay fb state)
          world player
      in
      let idle = idle_time ~spent:(seconds () -. now) in
      Sdl.delay (Int32.of_float (idle *. 1000.));
      (* Frames are timed start to start, so the sleep above counts towards the
         next one's length rather than falling outside every frame. *)
      loop ctx ~state ~fullscreen ~previous:now

(** Acquire a resource, use it, and release it even if the body raises. *)
let with_resource acquire release use =
  let* resource = acquire () in
  Fun.protect ~finally:(fun () -> release resource) (fun () -> use resource)

(** Open a window and run [state] through the loop, returning what it has become
    when the game says it is finished or the player quits.

    The engine knows nothing about the state but these callbacks. [update] is
    given the frame's length, the movement asked for over it and the one-off
    actions that arrived with it, and returns the next state. [view] says which
    world and which player to draw the frame from — a game keeps a great deal
    more than those two, and this is the part of it the renderer understands.
    [overlay] draws over the finished world before it reaches the screen, and
    [finished] is asked after every update.

    Time passes only while the window has focus; see {!simulate}. *)
let run_state ~update ~view ?(overlay = fun _ _ -> ())
    ?(finished = fun _ -> false) state =
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
          game = { update; view; overlay; finished };
        }
        ~state ~fullscreen:false ~previous:(seconds ()))

(** Open a window on [world] and run it until the player quits.

    [grow] is called whenever the player walks from one room into another, with
    the world and the player's new position, and returns the world to draw from
    now on. A fixed level needs none; a house that is generated as it is
    explored uses it to build far enough ahead that the player never sees the
    edge — {!Config.max_portal_depth} doorways, since that is exactly how deep
    the renderer looks. It runs on a room change and not per frame, so a
    generator may take its time.

    This is {!run_state} over the only state the engine used to be able to hold:
    the world and the player, advanced by {!step}, drawn as they are, and never
    finished by anything but quitting. *)
let run ?(grow = fun world _ -> world) world =
  let update (world, player) ~dt:_ ~motion ~actions:_ =
    let moved = step world player motion in
    (* Walking through a doorway is the one moment the horizon can have moved,
       so it is the only moment worth asking the world to grow. Every other
       frame this is a comparison of two ints. *)
    let world =
      if moved.Player.room <> player.Player.room then grow world moved
      else world
    in
    (world, moved)
  in
  let+ _ = run_state ~update ~view:Fun.id (world, Player.spawn world) in
  ()
