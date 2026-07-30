(** What the hardware is reporting, and nothing about what it means. Nothing
    here knows about positions or walls, and — since {!Binding} — nothing here
    knows which key walks forward either: this module reads the keyboard and the
    mouse, counts the edges and the holds, and hands the result on for somebody
    else to interpret.

    A game reads one frame's worth of it — an {!actions} — through {!val-down},
    {!pressed}, {!released}, {!held_for} and {!value}, and never through the
    arrays inside: how controls are numbered is this module's own business, and
    the numbering is not in the interface. *)

type motion = {
  forward : float;  (** cells to walk along [dir] this frame *)
  strafe : float;  (** cells to walk along [right] this frame *)
  turn : float;  (** radians to rotate this frame, + clockwise *)
  pitch : float;  (** change in view pitch this frame, + looks up *)
}
(** What the player is asking for over one frame, as finished per-frame deltas,
    so {!Engine} can apply it straight to the {!Player}. {!Binding.motion} is
    what produces one; the type lives here because it is what a frame of input
    amounts to. *)

val still : motion
(** Asking for nothing: all four zero. What an unfocused frame is worth, and the
    base a game builds its own motion on top of. *)

val mouse_delta : unit -> float * float
(** How far the mouse moved since the last call. Relative mouse mode (enabled by
    {!Engine}) reports these deltas and keeps the cursor pinned, so it never
    runs into a screen edge.

    SDL accumulates the delta until somebody reads it, so this has to be called
    every frame whether the answer is wanted or not, focused or not. The loop
    does; a game has no reason to. *)

type button =
  | Left
  | Middle
  | Right
      (** The mouse buttons a game may bind. SDL knows about more; these are the
          three every mouse has. *)

type control =
  | Key of Key.t
  | Button of button
      (** Something the player can hold down. Keys are places on the keyboard
          rather than letters, so a binding stays where it is under a different
          layout — see {!module-Key}.

          The engine has no opinion about what any of them mean. "Interact",
          "chalk", "journal" are a game's words for its own table from controls
          to actions; what the engine owns is the mechanism underneath — when a
          control went down, when it came up, and how long it was held. Even
          walking is a game's table now: {!Binding} holds it, and the engine's
          only part in that is a default. *)

(** A source that reports a number rather than being down or not, for the half
    of the input a control cannot express. A gamepad's sticks and triggers would
    join this list. *)
type analog =
  | Mouse_x  (** pixels the mouse moved right during this frame *)
  | Mouse_y  (** pixels it moved down *)

(** Which kind of number an {!analog} source reports. *)
type reading =
  | Rate
      (** a fraction of full speed, in [-1, 1]: a stick pushed halfway asks to
          walk at half pace. Scaled by the frame's length, exactly as a held
          control is. *)
  | Displacement
      (** how far the thing has already moved during this frame, in its own
          units. Already a per-frame quantity, so scaling it by the frame's
          length again would be counting the frame twice. *)

val reads : analog -> reading
(** What an analog source's number means, which is the one thing {!Binding}
    needs in order to scale it. The mouse reports where it has been; a stick
    reports how hard it is being pushed, and the two are not the same kind of
    number however alike they look. *)

type actions
(** What the player is holding, what they have just done, and what the analog
    sources read. Read it through {!val-down}, {!pressed}, {!released},
    {!held_for}, {!value} and {!val-pointer}.

    Abstract, and of everything here that is the one worth arguing for. Inside
    are three arrays indexed by control, and neither how they are indexed nor
    that they exist is a caller's business — but the reason is sharper than
    that. The frame handed to a game {e is} the frame the next one is built
    from: {!advance} makes the new frame's [was_down] the previous frame's
    [down], the same array and not a copy, and {!untouched} is one value the
    whole process starts every run from. Were those arrays reachable, a single
    write from a game's [update] would rewrite the edges of the frame after it,
    and a write that reached [untouched] would outlive the run that made it.
    None of that is a rule a caller could be asked to keep, so the type does not
    ask.

    What that costs is one function, {!with_pointer}, for the one update from
    outside this module that a frame of input legitimately has. *)

val untouched : actions
(** Nothing held, nothing pressed, nothing moved, the cursor at the origin.
    Nothing outside this module can write to what is in it and {!advance} builds
    new arrays every frame, so sharing this one value between runs is safe.
    Where a run starts, and what a test starts from. *)

val down : actions -> control -> bool
(** Whether [control] is held down right now. The state, not the edge — reach
    for {!pressed} where it is the moment of pressing that matters. *)

val pressed : actions -> control -> bool
(** Whether [control] went down this frame. This is the edge, not the state: it
    is true for exactly one frame however long the control is then held, which
    is what a game wants for anything that should happen once per press. *)

val released : actions -> control -> bool
(** Whether [control] came up this frame — the other edge. *)

val held_for : actions -> control -> float
(** How long [control] has been held, in seconds.

    It counts from the frame {e after} the press, so {!pressed} and a duration
    of zero arrive together, and it keeps its final value for the one frame on
    which {!released} is true. That last part is what lets a game tell a tap
    from a deliberate hold at the moment the control comes up: a door that opens
    on a press and commits on a long hold reads [released] and asks this how
    long the hold lasted. *)

val value : actions -> analog -> float
(** What an analog source read over this frame, in its own units — pixels, for
    both of the two that exist. *)

val pointer : actions -> int * int
(** Where the cursor is, in the framebuffer's coordinates. Meaningful only while
    the game has asked for a free cursor — under mouse look it is pinned and
    says nothing. *)

val with_pointer : actions -> int * int -> actions
(** The same frame with the cursor said to be somewhere else. Everything about
    what is held, what has just been pressed and how long it has been held is
    the frame's own and comes through untouched.

    {!Engine} is what this is for, and its one use of it. {!sample} reports the
    cursor where SDL does, in the window's coordinates; the framebuffer is a
    fraction of that size, so the loop puts the cursor into the buffer's
    coordinates before a game is handed the frame — which is the one thing about
    a frame of input that somebody other than this module knows better. The only
    function here that builds an [actions] out of another one without rolling a
    frame forward. *)

val advance :
  ?tapped:(control -> bool) ->
  actions ->
  down:(control -> bool) ->
  mouse:float * float ->
  pointer:int * int ->
  dt:float ->
  actions
(** Roll a frame forward onto what is held at this instant: [down] answers for
    each control, [mouse] is how far the mouse moved, [pointer] is where the
    cursor is, and the frame being asked about lasted [dt] seconds.

    [tapped] is for the control that went down and came back up in the gap
    between two frames, which [down] has no way to report: by the time anybody
    asks, it is up again. Answering [true] for one counts it as down for this
    frame, so {!pressed} is true now and {!released} is true on the frame after,
    and the hold reads the [0.] a tap is worth. A control that is both held and
    tapped is just held, and nothing says it twice. Say nothing and nothing was
    tapped, which is what {!freeze} wants.

    This is the whole of the edge detection and the hold timer, and none of it
    touches SDL — hand it a [down] of your own and a frame of input is a value
    you can write down, which is how everything here is tested and how a game
    can test its own controls without a window. {!sample} is this with the real
    keyboard supplied. *)

val freeze : actions -> actions
(** The same frame again, with nothing having changed and no time having passed:
    what a frame the window spent out of focus is worth.

    It is {!advance} handed back what was already held, over a frame of zero
    seconds, so the edges and the hold timer stay stated in exactly one place.
    [down] and [was_down] come out equal, so neither {!pressed} nor {!released}
    fires; a control held across the pause keeps the total it had rather than
    growing; and a control let go of while nobody was looking arrives as an
    ordinary {!released} on the first frame the window is back, which is the
    frame a game could have done anything about it.

    The mouse is set back to nothing rather than carried, because unlike the
    controls it is a movement and not a state: keeping the previous frame's
    would report the same inch of desk twice.

    {!Engine.simulate} already stops the clock and drops the motion of an
    unfocused frame. This is the third of the three, and it has to happen where
    the sampling does: by the time [simulate] has the actions the seconds have
    been counted, and no amount of suppression downstream can un-count them. *)

type queue
(** What went past in the event queue during one frame: whether the window
    system asked the program to stop, and which controls were seen going down.
    Abstract for the same reason {!actions} is — the second of those is an
    array, and it is nobody's to write into. *)

val drain : Tsdl.Sdl.event -> queue
(** Drain the event queue into one of those, reading into the caller's event
    record rather than allocating one per event.

    Draining is the part that has to happen: the queue must be pumped every
    frame even when nothing in it interests us, or the window stops responding.
    What the caller gets for it is what went past.

    Two kinds of thing are kept. The window asking to close — the close button,
    or Cmd-Q, which reaches SDL by the same road — comes back out of {!closed}.
    And every key and mouse button seen going {e down} is remembered for
    {!sample}, because a control the player taps inside a single frame is up
    again by the time the keyboard is read, and reading state alone loses it
    entirely: no press, no release, nothing at all. Coming {e up} is not
    remembered, having no need to be — the device already reports a control that
    is up as up. Nor is auto-repeat filtered out, because a repeat means the key
    is held, and the mark it leaves is one the state was going to make anyway.

    What is still lost is the control released {e and} pressed again inside one
    frame: it is down at both ends of this frame and was down at both ends of
    the last, so there is no edge to be had without keeping the whole queue in
    order — a great deal of machinery for a sixteen millisecond double tap
    nobody can perform.

    No key is read here as a {e binding}, not even the two the engine acts on
    itself. Fullscreen and leaving the run are ordinary state from {!Binding},
    like anything a game binds: {!pressed} is already true for exactly one frame
    per press, which is the whole of what watching for the event used to buy.
    What a {e player} means by a control is {!sample}'s business; what the
    {e window} meant is this one's. *)

val closed : queue -> bool
(** Whether the window system asked the program to stop while the queue was
    being read. {!Engine}'s loop asks this first, and leaves if it is true. *)

val quiet : queue
(** A frame in which nothing whatever went past. What the first sample of a run
    starts from, there being no queue to drain before the run has begun. *)

val sample : actions -> queue -> mouse:float * float -> dt:float -> actions
(** Read the keyboard and the mouse buttons as they are now, add what the
    frame's [queue] saw going down, and roll a frame forward onto the pair.
    [mouse] comes in from {!mouse_delta}, which {!Engine} calls whether this is
    reached or not.

    The keyboard array SDL hands back is its own and it changes underneath us,
    so {!advance} copies out of it rather than keeping it. The cursor comes out
    in window coordinates; {!Engine} scales it into the framebuffer's with
    {!with_pointer}, being the one that knows how the two compare.

    The loop's seam, and the one thing here that needs SDL running. What it
    makes of the two is {!advance}'s [down] and [tapped], which need nothing. *)
