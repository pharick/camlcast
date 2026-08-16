(** Window lifetime and the game loop. Every SDL call can fail, so the whole
    module is written inside the [Result] monad; the first error aborts the
    frame, and with it the program. Not every call is held to that rule: two
    requests made on the player's behalf are refused without ending anything —
    see "What ends a run and what does not" under {!with_window}, where both are
    set out.

    {!with_window} opens a window. {!run} plays the loop on one. {!run_world} is
    that loop over the only state the engine can hold on a game's behalf — a
    world and the player walking it. A game uses it until it keeps something
    else.

    A window and a run are two lifetimes, not one. A game with a single world to
    show opens a window, plays its run on it and closes it again, all in a line.
    A launcher plays run after run on the {e same} window, so returning to its
    menu changes what is drawn rather than closing a window and reopening it at
    another size. *)

type window
(** A window, and everything behind it: SDL itself, the renderer, and the buffer
    a frame is drawn into. Made by {!with_window} and passed to every run played
    on it.

    It holds fullscreen and relative mouse mode. The window outlives the run, so
    the window is what carries them from one run to the next: a player who goes
    fullscreen in a launcher's menu is still fullscreen in the demo they pick.

    The stored values are what the modes {e are}, not what was last asked for.
    The desktop can refuse either request, and neither refusal ends a run.
    Recording the answer that came back keeps the next frame from acting on a
    state the window is not in. See {!with_window}.

    SDL is not silent about these modes: it has a getter for each, and the loop
    below already calls one of the two for focus. They are held here because SDL
    offers no toggle — flipping a mode needs the current value — and because
    reading them back would be two calls on the path that runs every frame. They
    are held values, not the only copy. *)

(** How a run came to an end.

    Only a caller that plays one run after another needs the difference, which
    is why it was only written down once there was a launcher. The demo browser
    shows its menu again when a demo is [Returned] and stops altogether when one
    is [Closed]. So shutting the window — or Cmd-Q, which reaches SDL by the
    same road — ends the program instead of bouncing back to the list. *)
type ending =
  | Closed
      (** the window was shut, or the desktop asked the program to stop. The
          window itself stays open until {!with_window} returns; the run is what
          has ended, and the program is what the player asked to end. *)
  | Returned
      (** the run ended on its own terms: [finished] said so, or the player
          pressed something the game's {!Binding.t} listed as leaving it. The
          window is still open and whatever started the run has it back — hence
          the name.

          {b This constructor was [Left].} That name read better against
          {!Binding.t}'s [leave] — the field that produces it — but shared a
          spelling with {!Input.Left}, the mouse button, one facade away.
          Neither could be confused into a working program: both are nullary
          constructors of unrelated types, so every mix-up is a compile error
          that names both. Removing a homograph from the one API a game reads
          was still worth more than that, and this half of the pair was the half
          that could move. *)

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
          screen. The cursor SDL warps on the way in and out of each mode is not
          reported to the camera as a look. *)
  finished : 'a -> bool;
      (** asked once the frame this state describes has been drawn; [true] ends
          the run, so the frame a game ends on is one the player saw *)
  bindings : Binding.t;
      (** what the player's controls are for: which of them walk, which look,
          which toggles fullscreen, and which — if any — ends the run. The
          engine names no key of its own; {!Binding.default} is a default, not a
          rule. *)
}
(** How the loop reaches a game whose state it knows nothing else about. ['a] is
    the game's own — phases, doors, journal, whatever it keeps — and the engine
    only ever hands it back to these six things. What the loop needs from a game
    is small:
    - something to advance;
    - a world and a player to draw it from;
    - an optional layer over the top;
    - which of the two things the mouse is for;
    - an answer to whether it is over;
    - the table its controls are read through. *)

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
    [update] and a [view], with the rest defaulted. Omission means what it has
    always meant — [overlay] draws nothing, [pointing] is never true, [finished]
    never ends the run of its own accord, and [bindings] is {!Binding.default}.
    The defaults live here so {!run} and {!simulate} agree about them by
    construction.

    The trailing [()] closes the optional arguments, as on {!Binding.make} and
    {!Font.make}: nothing positional follows them, so nothing else could. *)

val with_window :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  (window -> ('a, [ `Msg of string ]) result) ->
  ('a, [ `Msg of string ]) result
(** Open a window, hand it to the given function, and close it again when that
    function is done with it — however it is done, error or exception included.

    [title], [width] and [height] default to {!Config.window_title},
    {!Config.initial_width} and {!Config.initial_height}, so every call made
    before these parameters existed still gets the same window. They exist
    because a game previously could not name its own window: {!Config} is
    compile-time constants, and this function took no arguments at all, so every
    game built on this engine opened a window called CamlCast. The field of view
    is still in that position — it is read inside {!Viewport}, several calls
    below anything a game can reach — and is not settable here.

    Everything a frame needs is acquired here and released in reverse: SDL, the
    window, the renderer, and the buffer frames are drawn into. Nothing inside
    is exposed, because nothing outside has any business freeing them; a caller
    holds the {!window} and plays runs on it.

    The result is the function's own, passed through untouched. The error is
    [`Msg] carrying SDL's own message, from whichever call failed first —
    starting SDL, opening the window, making the renderer, or making the buffer.

    {2 What ends a run and what does not}

    Those four are the complete list. The rule behind it is not the obvious one:
    {b A failure that stops a frame being drawn ends things; a failure of
       something asked for on the player's behalf does not.} There is no window
    without SDL, no frame without a renderer and a buffer, and nothing to play
    on without the window: those are the run.

    Relative mouse mode is not part of the run, and is asked for here rather
    than acquired. Most games want it, and taking it now saves the first frame a
    warp. But a compositor that will not hand over the pointer is a compositor
    where mouse look stops at the edges of the screen — not one where the game
    cannot open. Held as a resource, it made that desktop a desktop with no
    window at all, including for a game that frees the cursor on its first frame
    and never asks for it again. The window records what was actually granted;
    {!run} settles it per run and the loop per frame, from what the game says it
    wants.

    The other request made on the player's behalf is the fullscreen key, which
    is {!run}'s. A window manager refusing it leaves the window the size it
    already was and the run going. *)

val run : window -> 'a game -> 'a -> ('a * ending, [ `Msg of string ]) result
(** [run window game state] runs a state through the loop on a window. It
    returns what the state has become when the game says it is finished or the
    player quits.

    The game is the {!type-game} record, usually built with {!val-game}. The
    engine knows nothing about the state but its six callbacks, and the record
    is the same one {!simulate} reads, so the loop and a test drive one
    description of the game rather than two spellings of it.

    {!val-game}'s defaults include no key that ends the run. A run with no other
    way out has to ask for one — see {!Binding.default} for why the engine will
    not assume it, and {!run_world} for the one place it does. Omitting both
    [finished] and a leaving key is a window the player can only close.

    The window may have been played on already, so a run does not assume it
    arrives at a fresh one:
    - it takes the mouse as this game wants it, from [pointing], rather than
      supposing it was left captured;
    - it drops the mouse movement that piled up while no run was reading it,
      which would otherwise swing the camera on the first frame;
    - it starts from the controls as they physically are, so a key the player is
      still holding from whatever chose this game is held rather than newly
      pressed.

    That first frame's cursor is put into the buffer's coordinates, where
    {!Input.pointer} says a cursor always is. The loop converts each cursor as
    it reads it, and an unfocused frame carries the last one along; a first
    cursor nobody converted would therefore be repeated by every unfocused frame
    after it. The handover does {e not} empty the event queue: a window shut
    during it still ends the program.

    Returns the state the game reached and how it got there; see {!ending}. The
    error is [`Msg] carrying SDL's own message, from whichever frame failed —
    the window's own making was {!with_window}'s to report.

    A frame fails by failing to be drawn: sizing the buffer to the window, or
    the render and present at the end. The two things a frame asks for on the
    player's behalf are not among them, by the rule {!with_window} sets out.
    Pressing the fullscreen key on a desktop that refuses it leaves the window
    as it was and the run going, rather than quitting the game over a key some
    window managers do not honour. Taking or releasing the pointer as [pointing]
    changes is attempted on every frame where they differ and never insisted on;
    it is therefore retried, and comes right of its own accord on a desktop that
    stops saying no.

    Time passes only while the window has focus; see {!simulate}.

    {2 What the result does not cover}

    The game's own six callbacks. An exception out of [update], [view],
    [overlay], [pointing], [finished] or [bindings] passes straight through this
    and out of the run; it is not turned into an [Error].

    That is the rule, not a gap in it. The [`Msg] is for what the world outside
    would not do — SDL refusing a texture, a window that will not resize. A
    game's own callback raising is the other kind of mistake entirely: an
    [Invalid_argument] from a wall of no length, a [Not_found] from a lookup
    that should not have missed, a game's own exception meaning its own thing.
    Wrapping those in [`Msg] would flatten a bug in the game into a condition
    the game is expected to handle, and would cost the backtrace that says where
    it happened. The engine has no better idea what such an exception means.

    What the loop does guarantee across one is that it does not leave the
    machine wedged. Unwinding passes back out through {!with_window}, whose
    pointer release, renderer and buffer teardown are all [Fun.protect]ed rather
    than done only on the way out. A game that wants a raise reported rather
    than propagated catches it in its own callback, where it still knows what it
    was doing; the demos do exactly that, one exception of their own, and
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
    on. Left out, the world never changes. A level generated as it is explored
    uses [extend] to build {!Config.max_portal_depth} doorways ahead, because
    that is exactly how deep the renderer looks; the player then never sees the
    edge. It runs only on a frame that crossed, not on every frame — which is
    almost all of them — so a generator may take its time.

    {b Once per frame, not once per doorway.} A single step can go through
    several: {!Player.slide} clips its leg at each opening and carries the rest
    of it through, up to {!Config.max_crossings_per_step} times per axis. All of
    them are one call, with the pose the player finished in and the room they
    finished in. That is what a generator wants: building ahead of where they
    now stand covers every room they passed through to get there, since the
    renderer looks no deeper than [max_portal_depth] from there either. A game
    that needs each crossing in turn — a trail of the way home, say — wants the
    list itself, which is {!move}'s [crossings] and what [demo/trail.ml] uses.

    Going through a doorway is the one moment the horizon can have moved, and
    {!Player.crossed} is what says whether a frame did. Comparing the room the
    player ended in is the wrong test: it calls a step round a jamb, or all the
    way round a loop of rooms, nothing happening. A game that has outgrown this
    wrapper and moved to {!run} should ask {!Player.crossed} for itself rather
    than working the rule out again.

    This is {!run} over the state the engine can hold on a game's behalf: the
    world and the player. Escape leaving is this function's rule, not the
    engine's. A game with screens in it wants that key for closing them, which
    is why {!Binding.default} binds no such key at all; a bare world has nothing
    else to end it with, so this is where the default table is asked for one. A
    caller with its own idea passes [~bindings] and gets that instead.

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
    order: turn and pitch before walking, so a frame that both turns and moves
    walks in the direction it ends up facing.

    That ordering is why this exists rather than a game calling
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
    returning the world and the player as they now are. This is exactly
    {!run_world}'s own [update]. It is exposed because a game that has outgrown
    that wrapper — because it keeps a score, or doors, or a phase — still wants
    the rule rather than a second copy of it, and because a rule about a frame
    can then be driven through {!simulate} in a test with no window open.

    [extend] runs once on a frame that went through at least one doorway, and
    not at all on one that went through none. Not once per doorway: a step can
    clip its way through several, and the pose handed over is the one the frame
    finished in. See {!run_world} for what that contract is worth. *)

val simulate :
  'a game ->
  'a ->
  focused:bool ->
  pointing:bool ->
  dt:float ->
  motion:Input.motion ->
  actions:Input.actions ->
  'a
(** Advance the game by one frame, with nothing drawn. This is everything the
    loop does between reading the input and rendering the result, and it is a
    pure function of the state and that input. It needs no window, so a test can
    drive a game's {!run} callbacks through it and never open one.

    An unfocused window advances by nothing at all. Without this, a game left
    behind another window for a minute would be handed that minute the moment it
    came back — a minute of burnt lamp oil, of whatever else runs on the clock,
    none of which the player was there for. The loop keeps timing frames while
    paused, so focus returns at an ordinary frame's length rather than a jump.

    Motion is dropped rather than scaled down by the zero [dt], because the
    mouse is not scaled by [dt] in the first place. Its deltas are already
    everything that has happened since the last read (see
    {!Input.Displacement}), so a paused frame that passed them on would still
    turn the camera.

    A frame the player spends [pointing] at something the game has drawn drops
    the motion too, for the same reason: the cursor is loose, and every inch of
    it would otherwise also swing the camera. The clock keeps running through
    one, though. A screen the game opens over its world is its own business, and
    only the game can answer whether time stops for it.

    The actions are not suppressed here, and need no suppression. An unfocused
    frame's actions have already been frozen by the loop, through
    {!Input.Runtime.freeze}, which is the only place they still could be. Their
    hold timer is fed the real length of the frame at the moment they are
    sampled, so by the time they arrive here the seconds are counted and nothing
    done to them would give those back. *)
