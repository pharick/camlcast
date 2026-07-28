(** Window lifetime and the game loop. Every SDL call can fail, so the whole
    module is written inside the [Result] monad and the first error aborts the
    frame — and with it the program. *)

open Tsdl
open Result_ext

(** How a run came to an end.

    Only something that opens one window after another has any use for the
    difference, which is why it took until there was a launcher to write it
    down. The demo browser shows its menu again when a demo is [Left] and stops
    altogether when one is [Closed], so that shutting the window — or Cmd-Q,
    which reaches SDL by the same road — ends the program instead of bouncing
    back to the list. *)
type ending =
  | Closed  (** the window was shut, or the desktop asked the program to stop *)
  | Left
      (** the run ended on its own terms: [finished] said so, or the player
          pressed something the game's {!Binding.t} listed as leaving it *)

type 'a game = {
  update : 'a -> dt:float -> motion:Input.motion -> actions:Input.actions -> 'a;
      (** the next state, given how long the frame lasted, the movement asked
          for over it and what the player is pressing and holding through it *)
  view : 'a -> World.t * Player.t;  (** what this frame is drawn from *)
  overlay : Framebuffer.t -> 'a -> unit;
      (** anything drawn over the finished world, before it reaches the screen
      *)
  pointing : 'a -> bool;
      (** whether the player is working a cursor over something the game has
          drawn, rather than looking around with the mouse *)
  finished : 'a -> bool;  (** asked after every update; [true] ends the run *)
  bindings : Binding.t;
      (** what the player's controls are for: which of them walk, which look,
          which toggles fullscreen, and which — if any — ends the run. The
          engine names no key of its own; {!Binding.default} is a default and
          not a rule. *)
}
(** How the loop reaches a game whose state it knows nothing else about. ['a] is
    the game's own — phases, doors, journal, whatever it keeps — and the engine
    only ever hands it back to these six things. What it needs from a game is
    small: something to advance, a world and a player to draw it from, an
    optional layer over the top, which of the two things the mouse is for, an
    answer to whether it is over, and the table its controls are read through.
*)

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

(** Move the player through one frame. Pure: input in, new player out, along
    with every doorway the frame went through. The motion already carries
    finished per-frame deltas (see {!Binding.motion}), so this only decides the
    order — turn and pitch before walking, so a frame that both turns and moves
    walks in the direction it ends up facing.

    That ordering is the reason this exists rather than a game calling
    {!Player.traverse} itself: it is a rule about a frame, and there should be
    one copy of it. *)
let move world player (motion : Input.motion) =
  player
  |> Player.turn ~radians:motion.turn
  |> Player.pitch_by ~radians:motion.pitch
  |> Player.traverse world ~forward:motion.forward ~strafe:motion.strafe

(** {!move} for a caller with nothing to do with the doorways it crossed. *)
let step world player (motion : Input.motion) =
  (move world player motion).Player.player

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
    everything that has happened since the last read (see
    {!Input.Displacement}), so a paused frame that passed them on would still
    turn the camera.

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

    [bindings] is what the player's controls are for, {!Binding.default} unless
    a game says otherwise. Note what that default does {e not} include: no key
    ends the run. A run with no other way out has to ask for one — see
    {!Binding.default} for why the engine will not assume it, and {!run_world}
    for the one place it does.

    Returns the state the game reached and how it got there; see {!ending}.

    Time passes only while the window has focus; see {!simulate}. *)
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

(** Open a window on [world] and run it until the player quits.

    [extend] is called whenever the player goes through a doorway, with the
    world and the player's new position, and returns the world to draw from now
    on. A fixed level needs none; a level that is generated as it is explored
    uses it to build far enough ahead that the player never sees the edge —
    {!Config.max_portal_depth} doorways, since that is exactly how deep the
    renderer looks. It runs on a crossing and not per frame, so a generator may
    take its time.

    Going through a doorway is the one moment the horizon can have moved, and
    {!Player.crossed} is what says whether a frame did — not the room the player
    ended in, which calls a step round a jamb or all the way round a loop of
    rooms nothing happening. A game that has outgrown this wrapper and moved to
    {!run} asks {!Player.crossed} for itself rather than working the rule out
    again.

    This is {!run} over the state the engine can hold on a game's behalf: the
    world and the player. Escape leaving is this function's rule and not the
    engine's — a game with screens in it wants that key for closing them, which
    is why {!Binding.default} binds no such key at all — but a bare world has
    nothing else to end it with, so this is where the default table is asked for
    one. A caller with its own idea passes [~bindings] and gets that instead.

    Reports how the run ended, which is what a launcher needs to tell "back to
    the menu" from "close the program". A program with only one world to show
    has no use for the answer and can [ignore] it. *)
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
