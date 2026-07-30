(** Window lifetime and the game loop. Every SDL call can fail, so the whole
    module is written inside the [Result] monad and the first error aborts the
    frame — and with it the program.

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
    still fullscreen in the demo they pick. *)

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
  (window -> ('a, [ `Msg of string ]) result) -> ('a, [ `Msg of string ]) result
(** Open a window, hand it to the given function, and close it again when that
    function is done with it — however it is done, error or exception included.

    Everything a frame needs is acquired here and released in reverse: SDL, the
    window, the renderer, relative mouse mode, and the buffer frames are drawn
    into. Nothing inside is exposed, because nothing outside has any business
    freeing them; a caller holds the {!window} and plays runs on it.

    The result is the function's own, passed through untouched. The error is
    [`Msg] carrying SDL's own message, from whichever call failed first —
    starting SDL, opening the window, making the renderer, or making the buffer.
*)

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
    this game is held rather than newly pressed. What it does {e not} do is
    empty the event queue: a window shut during the handover still ends the
    program.

    Returns the state the game reached and how it got there; see {!ending}. The
    error is [`Msg] carrying SDL's own message, from whichever frame failed —
    the window's own making having been {!with_window}'s to report.

    Time passes only while the window has focus; see {!simulate}. *)

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
