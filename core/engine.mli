(** Window lifetime and the game loop. Every SDL call can fail, so the whole
    module is written inside the [Result] monad and the first error aborts the
    frame — and with it the program. Which calls are held to that is a decision
    and not a reflex: see "What ends a run and what does not" under
    {!with_window}, where the two that are asked for on the player's behalf, and
    refused without ending anything, are set out.

    {!with_window} opens a window. {!run} is the loop, played on one.
    {!run_world} is that loop over the only state the engine can hold on a
    game's behalf — a world and the player walking it — and is what a game
    reaches for until it keeps something else.

    A window and a run are two lifetimes and not one. A game with a single world
    to show opens a window, plays its run on it and closes it again, all in a
    line. A launcher plays run after run on the {e same} window, so that
    returning to its menu is a change of what is drawn and not a window that
    disappears and comes back at another size. *)

type window
(** A window, and everything behind it: SDL itself, the renderer, and the buffer
    a frame is drawn into. Made by {!with_window} and passed to every run played
    on it.

    It holds what SDL will not answer for. Fullscreen and relative mouse mode
    can be set but never asked about, so this is where what they are is written
    down — and because the window outlives the run, that is what carries them
    from one to the next. A player who goes fullscreen in a launcher's menu is
    still fullscreen in the demo they pick.

    What they {e are}, and not what was last asked for. Either can be refused by
    the desktop, and neither refusal ends a run — so what is written here is the
    answer that came back, which is what keeps the next frame from acting on a
    state the window is not in. See {!with_window}. *)

(** How a run came to an end.

    Only something that plays one run after another has any use for the
    difference, which is why it took until there was a launcher to write it
    down. The demo browser shows its menu again when a demo is [Left] and stops
    altogether when one is [Closed], so that shutting the window — or Cmd-Q,
    which reaches SDL by the same road — ends the program instead of bouncing
    back to the list. *)
type ending =
  | Closed
      (** the window was shut, or the desktop asked the program to stop. The
          window itself is still open until {!with_window} returns; what has
          ended is the run, and what the player asked for is the program. *)
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
          drawn, rather than looking around with the mouse. Asked every frame,
          so a game turns it on and off as freely as it opens and closes a
          screen; the cursor SDL warps on the way in and out of each is not
          reported to the camera as a look. *)
  finished : 'a -> bool;
      (** asked once the frame this state describes has been drawn; [true] ends
          the run, so the frame a game ends on is one the player saw *)
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

val game :
  ?overlay:(Framebuffer.t -> 'a -> unit) ->
  ?pointing:('a -> bool) ->
  ?finished:('a -> bool) ->
  ?bindings:Binding.t ->
  update:('a -> dt:float -> motion:Input.motion -> actions:Input.actions -> 'a) ->
  view:('a -> World.t * Player.t) ->
  unit ->
  'a game
(** A game from the two things every game has and the four it may not: an
    [update] and a [view], with the omitted rest meaning what omitting them has
    always meant — [overlay] draws nothing, [pointing] never, [finished] never
    of its own accord, and [bindings] {!Binding.default}. The defaults live
    here, so {!run} and {!simulate} agree about them by construction.

    The trailing [()] is what closes the optional arguments, as on
    {!Binding.make} and {!Font.make}: nothing positional follows them, so
    nothing else could. *)

val with_window :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  (window -> ('a, [ `Msg of string ]) result) ->
  ('a, [ `Msg of string ]) result
(** Open a window, hand it to the given function, and close it again when that
    function is done with it — however it is done, error or exception included.

    [title], [width] and [height] default to {!Config.window_title},
    {!Config.initial_width} and {!Config.initial_height}, which is what every
    call made before these existed still gets. They are here because a game
    could not previously name its own window: {!Config} is compile-time
    constants, and this function took no arguments at all, so every game built
    on this engine opened a window called CamlCast. The field of view is still
    in that position — it is read inside {!Viewport}, several calls below
    anything a game can reach — and is not fixed here.

    Everything a frame needs is acquired here and released in reverse: SDL, the
    window, the renderer, and the buffer frames are drawn into. Nothing inside
    is exposed, because nothing outside has any business freeing them; a caller
    holds the {!window} and plays runs on it.

    The result is the function's own, passed through untouched. The error is
    [`Msg] carrying SDL's own message, from whichever call failed first —
    starting SDL, opening the window, making the renderer, or making the buffer.

    {2 What ends a run and what does not}

    Those four are the list, and the rule behind it is worth stating because the
    obvious rule is the wrong one.
    {b A failure that stops a frame being drawn ends things; a failure of
       something asked for on the player's behalf does not.} There is no window
    without SDL, no frame without a renderer and a buffer, and nothing to play
    on without the window: those are the run.

    Relative mouse mode is not, and is asked for here rather than acquired. It
    is what most games want and taking it now saves the first frame a warp, but
    a compositor that will not hand over the pointer is a compositor where mouse
    look stops at the edges of the screen — not one where the game cannot open.
    Held as a resource it made that desktop a desktop with no window at all,
    including for a game that frees the cursor on its first frame and never asks
    for it again. The window records what was actually granted; {!run} settles
    it per run and the loop per frame, from what the game says it wants.

    The other one is the fullscreen key, which is {!run}'s. A window manager
    refusing it leaves the window the size it already was and the run going. *)

val run : window -> 'a game -> 'a -> ('a * ending, [ `Msg of string ]) result
(** [run window game state] runs a state through the loop on a window, returning
    what it has become when the game says it is finished or the player quits.

    The game is the {!type-game} record, usually built with {!val-game} — the
    engine knows nothing about the state but its six callbacks, and the record
    is the same one {!simulate} reads, so the loop and a test drive one
    description of the game rather than two spellings of it.

    Note what {!val-game}'s defaults do {e not} include: no key ends the run. A
    run with no other way out has to ask for one — see {!Binding.default} for
    why the engine will not assume it, and {!run_world} for the one place it
    does. Omitting both [finished] and a leaving key is a window the player can
    only close.

    The window may have been played on already, so a run does not assume it
    arrives at a fresh one. It takes the mouse as this game wants it, from
    [pointing] rather than by supposing it was left captured; it drops the mouse
    movement that piled up while no run was reading it, which would otherwise
    swing the camera on the first frame; and it starts from the controls as they
    physically are, so a key the player is still holding from whatever chose
    this game is held rather than newly pressed. That first frame's cursor is
    put into the buffer's coordinates, where {!Input.pointer} says a cursor
    always is — the loop converts each one as it reads it and an unfocused frame
    carries the last along, so a frame nobody converted would be one every
    unfocused frame after it repeated. What it does {e not} do is empty the
    event queue: a window shut during the handover still ends the program.

    Returns the state the game reached and how it got there; see {!ending}. The
    error is [`Msg] carrying SDL's own message, from whichever frame failed —
    the window's own making having been {!with_window}'s to report.

    A frame fails by failing to be drawn: sizing the buffer to the window, or
    the render and present at the end. The two things a frame asks for on the
    player's behalf are not among them, on the rule {!with_window} sets out.
    Pressing the fullscreen key on a desktop that refuses it leaves the window
    as it was and the run going, rather than quitting the game over a key some
    window managers do not honour; and taking or releasing the pointer as
    [pointing] changes is attempted every frame it differs and never insisted
    on, so it is also retried, and comes right of its own accord on a desktop
    that stops saying no.

    Time passes only while the window has focus; see {!simulate}.

    {2 What the result does not cover}

    The game's own six callbacks. An exception out of [update], [view],
    [overlay], [pointing], [finished] or [bindings] passes straight through this
    and out of the run, and is not turned into an [Error].

    That is the rule and not a gap in it. The [`Msg] is for what the world
    outside would not do — SDL refusing a texture, a window that will not resize
    — and a game's own callback raising is the other kind of mistake entirely:
    an [Invalid_argument] from a wall of no length, a [Not_found] from a lookup
    that should not have missed, a game's own exception meaning its own thing.
    Wrapping those in [`Msg] would flatten a bug in the game into a condition
    the game is expected to handle, at the cost of the backtrace that says where
    it happened. The engine is not the place that knows better.

    What the loop does guarantee across one is that it does not leave the
    machine wedged: unwinding passes back out through {!with_window}, whose
    pointer release, renderer and buffer teardown are all [Fun.protect]ed rather
    than done on the way out. A game that wants a raise reported rather than
    propagated catches it in its own callback, where it still knows what it was
    doing; the demos do exactly that, one exception of their own, and
    [demo/reading.ml] is that seam written down. *)

val run_world :
  window ->
  ?extend:(World.t -> Player.t -> World.t) ->
  ?bindings:Binding.t ->
  World.t ->
  (ending, [ `Msg of string ]) result
(** Run a world on a window until the player quits. The player starts where the
    world says — {!Player.spawn}, which is the [~spawn] given to {!World.make} —
    since a bare world has nobody in it yet.

    [extend] is called on a frame the player went through a doorway on, with the
    world and where the player ended up, and returns the world to draw from now
    on. Left out, the world never changes. A level that is generated as it is
    explored uses it to build far enough ahead that the player never sees the
    edge — {!Config.max_portal_depth} doorways, since that is exactly how deep
    the renderer looks. It runs on a frame that crossed and not on every frame,
    which is almost all of them, so a generator may take its time.

    {b Once per frame, not once per doorway.} A single step can go through
    several — {!Player.slide} clips its leg at each opening and carries the rest
    of it through, up to {!Config.max_crossings_per_step} times per axis — and
    all of them are one call, with the pose the player finished in and the room
    they finished in. That is what a generator wants: building ahead of where
    they now stand covers every room they passed through to get there, since the
    renderer looks no deeper than [max_portal_depth] from there either. A game
    that needs each crossing in turn — a trail of the way home, say — wants the
    list itself, which is {!move}'s [crossings] and what [demo/trail.ml] uses.

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
    has no use for the answer and can [ignore] it. The error is {!run}'s. *)

val step : World.t -> Player.t -> Input.motion -> Player.t
(** {!move} for a caller with nothing to do with the doorways it crossed, which
    is what an [update] writes unless it is growing the world. *)

val move : World.t -> Player.t -> Input.motion -> Player.movement
(** Move the player through one frame. Pure: input in, new player out, along
    with every doorway the frame went through. The motion already carries
    finished per-frame deltas (see {!Binding.motion}), so this only decides the
    order — turn and pitch before walking, so a frame that both turns and moves
    walks in the direction it ends up facing.

    That ordering is the reason this exists rather than a game calling
    {!Player.traverse} itself: it is a rule about a frame, and there should be
    one copy of it. Ask {!Player.crossed} of what comes back to know whether the
    frame went through a doorway. *)

val grow :
  ?extend:(World.t -> Player.t -> World.t) ->
  World.t ->
  Player.t ->
  Input.motion ->
  World.t * Player.t
(** {!move} and then the growing rule: one frame of a world that builds itself,
    the world and the player as they now are. This is exactly what
    {!run_world}'s own [update] is, and it is here because a game that has
    outgrown that wrapper — because it keeps a score, or doors, or a phase —
    still wants the rule rather than a second copy of it, and because a rule
    about a frame can then be driven through {!simulate} in a test with no
    window open.

    [extend] runs once on a frame that went through at least one doorway, and
    not at all on one that went through none. Not once per doorway: a step can
    clip its way through several, and the pose handed over is the one the frame
    finished in. What that is worth is in {!run_world}. *)

val simulate :
  'a game ->
  'a ->
  focused:bool ->
  pointing:bool ->
  dt:float ->
  motion:Input.motion ->
  actions:Input.actions ->
  'a
(** Advance the game by one frame, with nothing drawn: this is everything the
    loop does between reading the input and rendering the result, and it is a
    pure function of the state and that input. It needs no window, so a game can
    drive its own {!run} callbacks through it in a test and never open one.

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
    frame's have already been frozen by the loop, through {!Input.freeze}, which
    is the only place they still could be. Their hold timer is fed the real
    length of the frame at the moment they are sampled, so by the time they
    arrive here the seconds are counted and nothing done to them would give
    those back. *)
