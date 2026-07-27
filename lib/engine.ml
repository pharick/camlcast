(** Window lifetime and the game loop. Every SDL call can fail, so the whole
    module is written inside the [Result] monad and the first error aborts the
    frame — and with it the program. *)

open Tsdl
open Result_ext

type 'a game = {
  update : 'a -> dt:float -> motion:Input.motion -> actions:Input.actions -> 'a;
      (** the next state, given how long the frame lasted, the movement asked
          for over it and what the player is pressing and holding through it *)
  view : 'a -> World.t * Player.t;  (** what this frame is drawn from *)
  overlay : Framebuffer.t -> 'a -> unit;
      (** anything drawn over the finished world, before it reaches the screen *)
  pointing : 'a -> bool;
      (** whether the player is working a cursor over something the game has
          drawn, rather than looking around with the mouse *)
  finished : 'a -> bool;  (** asked after every update; [true] ends the run *)
}
(** How the loop reaches a game whose state it knows nothing else about. ['a] is
    the game's own — phases, doors, journal, whatever it keeps — and the engine
    only ever hands it back to these five functions. What it needs from a game
    is small: something to advance, a world and a player to draw it from, an
    optional layer over the top, which of the two things the mouse is for, and
    an answer to whether it is over. *)

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

(** Advance the simulation by one frame. Pure: input in, new player out, along
    with every doorway the frame went through. The motion already carries
    finished per-frame deltas (see {!Input.val-motion}), so this only decides
    the order — turn and pitch before walking, so a frame that both turns and
    moves walks in the direction it ends up facing.

    That ordering is the reason this exists rather than a game calling
    {!Player.traverse} itself: it is a rule about a frame, and there should be
    one copy of it. *)
let advance world player (motion : Input.motion) =
  player
  |> Player.turn ~radians:motion.turn
  |> Player.pitch_by ~delta:motion.pitch
  |> Player.traverse world ~forward:motion.forward ~strafe:motion.strafe

(** {!advance} for a caller with nothing to do with the doorways it crossed. *)
let step world player (motion : Input.motion) =
  (advance world player motion).Player.player

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

(** The same again for relative mouse mode, which pins the cursor out of sight
    and hands the camera bare deltas. Releasing it puts a real cursor back on
    the screen — what a game wants while the player is pointing at something it
    has drawn. Setting it to what it already is is not free (SDL warps the
    cursor), so [current] is checked first. *)
let set_relative_mouse ~current enabled =
  if enabled = current then Ok current
  else
    let+ () = Sdl.set_relative_mouse_mode enabled in
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
    so a paused frame that passed them on would still turn the camera.

    A frame the player spends [pointing] at something the game has drawn drops
    the motion too, for the same reason — the cursor is loose, and every inch of
    it would otherwise also swing the camera. The clock keeps running through
    one, though: a screen the game opens over its world is its own business, and
    whether time stops for it is a question only the game can answer.

    The actions are not suppressed here, and want no suppression: an unfocused
    frame's have already been frozen by {!loop}, through {!Input.freeze}, which
    is the only place they still could be. Their hold timer is fed the real
    length of the frame at the moment they are sampled, so by the time they
    arrive here the seconds are counted and nothing done to them would give
    those back. *)
let simulate game state ~focused ~pointing ~dt ~motion ~actions =
  let dt = if focused then dt else 0. in
  let motion = if focused && not pointing then motion else Input.still in
  game.update state ~dt ~motion ~actions

(** Where the cursor is in the coordinates the overlay draws in. SDL reports it
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
    let focused = has_focus ctx.window in
    (* Read as state while the window has focus, and frozen while it has not:
       the hold timer runs on this [dt] and not on the one {!simulate} zeroes,
       so a frame nobody was there for has to be kept out of it here. The cursor
       is rescaled inside the same branch because {!Input.freeze} carries the
       previous frame's forward, and that one has been scaled already. *)
    let actions =
      if focused then
        let sampled = Input.sample actions ~dt in
        {
          sampled with
          Input.pointer =
            in_framebuffer ctx.window !(ctx.framebuffer) sampled.Input.pointer;
        }
      else Input.freeze actions
    in
    let state =
      simulate ctx.game state ~focused
        ~pointing:(ctx.game.pointing state)
        ~dt ~motion ~actions
    in
    if ctx.game.finished state then Ok state
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
      let idle = idle_time ~spent:(seconds () -. now) in
      Sdl.delay (Int32.of_float (idle *. 1000.));
      (* Frames are timed start to start, so the sleep above counts towards the
         next one's length rather than falling outside every frame. *)
      loop ctx ~state ~actions ~fullscreen ~relative ~previous:now

(** Acquire a resource, use it, and release it even if the body raises. It lives
    in {!Result_ext} now, because {!Surface} needs it too and sits far below this
    module; the name stays here for the windows and renderers that were its only
    callers when it was written. *)
let with_resource = Result_ext.with_resource

(** Open a window and run [state] through the loop, returning what it has become
    when the game says it is finished or the player quits.

    The engine knows nothing about the state but these callbacks. [update] is
    given the frame's length, the movement asked for over it and the one-off
    actions that arrived with it, and returns the next state. [view] says which
    world and which player to draw the frame from — a game keeps a great deal
    more than those two, and this is the part of it the renderer understands.
    [overlay] draws over the finished world before it reaches the screen.
    [pointing] says whether the mouse is working a cursor over what the game has
    drawn instead of looking around, and the engine releases and recaptures the
    cursor to match. [finished] is asked after every update.

    Time passes only while the window has focus; see {!simulate}. *)
let run_state ~update ~view ?(overlay = fun _ _ -> ())
    ?(pointing = fun _ -> false) ?(finished = fun _ -> false) state =
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
          game = { update; view; overlay; pointing; finished };
        }
        ~state ~actions:Input.untouched ~fullscreen:false ~relative:true
        ~previous:(seconds ()))

(** The world to draw from now on, given the world a frame began with and what
    that frame did.

    [grow] runs when the player has gone {e through a doorway}, which is not the
    same question as whether they have finished the frame in a different room. A
    single frame can round a jamb — out through an opening and back in through
    its twin — and end where it started; so can a step all the way round a loop
    of rooms. The room index calls both of those nothing happening. The horizon
    moved in each of them, and the crossings are the only place that is written
    down, which is why this reads them and not [moved.player.room].

    Once per frame however many doorways it went through, and with the pose it
    ended in: that is the only one that means anything by then, and what [grow]
    is being asked for is the world to draw from {e now}.

    Split out of {!run} because {!run} needs a window and this does not. *)
let grown ~grow world (moved : Player.movement) =
  match moved.Player.crossings with
  | [] -> world
  | _ :: _ -> grow world moved.Player.player

(** Open a window on [world] and run it until the player quits.

    [grow] is called whenever the player goes through a doorway, with the world
    and the player's new position, and returns the world to draw from now on —
    see {!grown}. A fixed level needs none; a house that is generated as it is
    explored uses it to build far enough ahead that the player never sees the
    edge — {!Config.max_portal_depth} doorways, since that is exactly how deep
    the renderer looks. It runs on a crossing and not per frame, so a generator
    may take its time.

    This is {!run_state} over the only state the engine used to be able to hold:
    the world, the player, and whether Escape has been pressed. Escape quitting
    is this function's rule and not the engine's — a game with screens in it
    wants that key for closing them (see {!Input.poll}) — but a bare world has
    no other way out, so the third of the three is here to carry it. *)
let run ?(grow = fun world _ -> world) world =
  let update (world, player, _) ~dt:_ ~motion ~actions =
    let moved = advance world player motion in
    (* Walking through a doorway is the one moment the horizon can have moved,
       so it is the only moment worth asking the world to grow. Every other
       frame this is a look at an empty list. *)
    ( grown ~grow world moved,
      moved.Player.player,
      Input.pressed actions (Input.Key Sdl.Scancode.escape) )
  in
  let+ _ =
    run_state ~update
      ~view:(fun (world, player, _) -> (world, player))
      ~finished:(fun (_, _, quit) -> quit)
      (world, Player.spawn world, false)
  in
  ()
