(** The table mapping player controls to game requests.

    {!Input} reports controls; this module says what they are for. Walking,
    looking, fullscreen and leaving the run all come from a value of this type,
    and a game hands one to {!Engine.run}. {!default} is the engine's own table,
    and it is a default, not a rule: the engine has no keys of its own left.

    {1 Rates and displacements}

    An axis sums terms. A term is a source and a signed weight, and the two
    kinds of source are summed differently; that is the one subtlety here.

    A {b rate} — a held control, or an analog source that {!Input.reads} as
    {!Input.Rate} — says how hard the player is asking, between [-1] and [1].
    Rate terms are summed, clamped to that range, and multiplied by the axis's
    [speed] and by the frame length. The clamp stops two keys bound to the same
    axis from moving twice as fast as one.

    A {b displacement} — the mouse — says how far something has already moved
    during this frame. It is added as-is: scaling it by the frame length would
    count the frame twice, since the mouse has already reported everything since
    the last read.

    So a stick bound to [turn] turns at [speed] radians per second when pushed
    fully over, and a mouse bound to the same axis turns by its weight in
    radians per pixel. Both land on one number. *)

(** The source of one term's number. A {!Hold} is a rate; a {!Read} is whichever
    kind {!Input.reads} reports, which decides how the term is summed. *)
type source =
  | Hold of Input.control  (** 1 while it is down, 0 otherwise *)
  | Read of Input.analog  (** whatever it reports *)

type term = { source : source; weight : float }
(** One source and its contribution. [weight] is signed: the same source at
    [-1.] pushes the other way. For a rate it is usually [1.] or [-1.]; for a
    displacement it is the sensitivity, in the axis's units per unit the source
    moved. *)

type axis = { terms : term list; speed : float }
(** One thing the player can ask for, and every source that asks for it. The
    [terms] are summed: rates first, clamped, then displacements added as they
    stand. [speed] is what a rate of 1 asks for, per second: cells for
    {!t.forward} and {!t.strafe}, radians for {!t.turn}, fractions of the
    window's height for {!t.pitch}. It does not affect displacement terms.

    An axis with no terms never moves; that is how a game disables one. *)

type t = {
  forward : axis;  (** walking along the way the player is facing *)
  strafe : axis;  (** walking sideways *)
  turn : axis;  (** yaw, + clockwise *)
  pitch : axis;  (** looking up and down, + up *)
  fullscreen : Input.control list;  (** any of these toggles it *)
  leave : Input.control list;
      (** any of these ends the run, reported as {!Engine.Returned} *)
}
(** The complete control table: four axes that produce an {!Input.motion}, and
    two lists of controls the engine acts on itself. A game builds one with
    {!make} and hands it to the loop. *)

val make :
  ?forward:axis ->
  ?strafe:axis ->
  ?turn:axis ->
  ?pitch:axis ->
  ?fullscreen:Input.control list ->
  ?leave:Input.control list ->
  unit ->
  t
(** {!default}, with the given parts replaced. Each omitted argument keeps
    {!default}'s field of the same name, so a game that only wants its own way
    out of the window passes [~leave] and inherits WASD, the arrows, the mouse
    and F11 unchanged.

    Replacement, not merging: a given [~forward] is the whole of that axis, not
    extra terms added to the default's. *)

val default : t
(** WASD to walk, the arrow keys to turn and look, the mouse to look, F11 for
    fullscreen — and {b no} key that leaves the run.

    The last is deliberate. Whether Escape ends a run is not the engine's to
    decide: a game with screens wants that key for closing them, and a window
    that quit underneath would break it. A run with no other way out asks for
    one, as {!Engine.run_world} does. *)

val motion : t -> Input.actions -> dt:float -> Input.motion
(** [motion table actions ~dt] is what the player asked for over a frame of [dt]
    seconds, reading [actions] — one frame of keys, buttons and mouse — through
    [table].

    The four returned fields are finished per-frame deltas, already scaled by
    [dt] where scaling applies, each in its axis's unit: cells for [forward] and
    [strafe], radians for [turn], fractions of the window's height for [pitch].
    A caller adds them to a pose without scaling them again; that is what
    {!Engine.move} does.

    Pure, and the frame's input is all it reads, so a game or a test can ask
    what a given frame of keys and mouse would have done without a window. *)

val taken : Input.control list -> Input.actions -> bool
(** [taken controls actions] is whether any of [controls] went down during the
    frame [actions] describes. The edge, not the state: a key held across a
    hundred frames answers on one of them. That is what [fullscreen] and [leave]
    both need, and what a game's own list of controls for one action usually
    needs. Use {!Input.val-down} where the holding matters. *)
