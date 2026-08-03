(** The table from what the player does to what the game is asked for.

    {!Input} reports controls; this says what they are for. Walking, looking,
    fullscreen and leaving the run all come from a value of this type, and a
    game hands one to {!Engine.run}. {!default} is the engine's own, and it is a
    default and not a rule: the engine has no keys of its own left.

    {1 Rates and displacements}

    An axis adds up terms. A term is a source and a signed weight, and the two
    kinds of source are added up differently, which is the one subtlety here.

    A {b rate} — a held control, or an analog source that {!Input.reads} as
    {!Input.Rate} — says how hard the player is asking, between [-1] and [1].
    Its terms are summed, clamped to that range, and multiplied by the axis's
    [speed] and by the length of the frame. The clamp is what stops two keys
    bound to the same axis from walking twice as fast as one.

    A {b displacement} — the mouse — says how far something has already moved
    during this frame. It is added on as it stands, because scaling it by the
    frame's length would count the frame twice: the mouse has already reported
    everything that happened since the last read.

    So a stick bound to [turn] turns at [speed] radians a second when pushed all
    the way over, and a mouse bound to the same axis turns by its weight in
    radians per pixel. Both land on one number. *)

(** Where one term's number comes from. A {!Hold} is a rate and a {!Read} is
    whichever {!Input.reads} says it is — which is what decides how the term is
    added up. *)
type source =
  | Hold of Input.control  (** 1 while it is down, 0 otherwise *)
  | Read of Input.analog  (** whatever it reports *)

type term = { source : source; weight : float }
(** One source and what it contributes. [weight] is signed: the same source at
    [-1.] pushes the other way. For a rate it is usually [1.] or [-1.]; for a
    displacement it is the sensitivity, in the axis's units per unit the source
    moved. *)

type axis = { terms : term list; speed : float }
(** One thing the player can ask for, and everything that asks for it. The
    [terms] are summed — rates first, clamped, then displacements added as they
    stand. [speed] is what a rate of 1 asks for, per second: cells for
    {!t.forward} and {!t.strafe}, radians for {!t.turn}, fractions of the
    window's height for {!t.pitch}. It does not touch the displacement terms.

    An axis with no terms never moves, which is how a game turns one off. *)

type t = {
  forward : axis;  (** walking along the way the player is facing *)
  strafe : axis;  (** walking sideways *)
  turn : axis;  (** yaw, + clockwise *)
  pitch : axis;  (** looking up and down, + up *)
  fullscreen : Input.control list;  (** any of these toggles it *)
  leave : Input.control list;
      (** any of these ends the run, reported as {!Engine.Returned} *)
}
(** The whole of what the player's controls are for: four axes that produce an
    {!Input.motion}, and two lists of controls that the engine acts on itself. A
    game builds one with {!make} and hands it to the loop. *)

val make :
  ?forward:axis ->
  ?strafe:axis ->
  ?turn:axis ->
  ?pitch:axis ->
  ?fullscreen:Input.control list ->
  ?leave:Input.control list ->
  unit ->
  t
(** {!default}, with the given parts replaced. Each argument omitted keeps
    {!default}'s field of the same name, so a game that only wants its own way
    out of the window passes [~leave] and inherits WASD, the arrows, the mouse
    and F11 unchanged.

    Replacement and not merging: a given [~forward] is the whole of that axis,
    not extra terms added to the default's. *)

val default : t
(** WASD to walk, the arrow keys to turn and to look, the mouse to look, F11 for
    fullscreen — and {b no} key that leaves the run.

    That last one is deliberate. Whether Escape ends a run is not the engine's
    to decide: a game with screens in it wants that key for closing them, and
    would be poorly served by a window that quit underneath it. A run that has
    no other way out asks for one, as {!Engine.run_world} does. *)

val motion : t -> Input.actions -> dt:float -> Input.motion
(** [motion table actions ~dt] is what the player asked for over a frame of [dt]
    seconds, reading [actions] — one frame of keys, buttons and mouse — through
    [table].

    The four fields that come back are finished per-frame deltas, already scaled
    by [dt] where scaling applies, and each in its axis's own unit: cells for
    [forward] and [strafe], radians for [turn], fractions of the window's height
    for [pitch]. A caller adds them to a pose and does not scale them again —
    which is what {!Engine.move} does with one.

    Pure, and the frame's input is the whole of what it reads, so a game or a
    test can ask what a given frame of keys and mouse would have done without a
    window in sight. *)

val taken : Input.control list -> Input.actions -> bool
(** [taken controls actions] is whether any of [controls] went down during the
    frame [actions] describes. The edge and not the state, so a key held across
    a hundred frames answers on one of them — which is what [fullscreen] and
    [leave] both want, and what a game's own list of controls for one action
    usually wants too. Reach for {!Input.val-down} where it is the holding that
    matters. *)
