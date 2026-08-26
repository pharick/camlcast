(** The overlay itself: what it is showing, what is picked, and the one call a
    game makes to turn it on.

    {1 Two halves, and why they are two}

    A run has openings on the outside — {!Camlcast.Watch}, which reports what
    the reconciler did and lets a frame be changed on its way to being built —
    and the overlay is drawn on the inside, as part of the description, because
    that is where a HUD is. So a session is a value the game holds and hands to
    both: {!watch} to {!Camlcast.Run.on}, and {!overlay} into its description.

    {!play} does both and is what a game wants.

    {1 What it holds between frames}

    Everything that outlives a frame and is not the game's: which panel is up,
    what is picked, the edits not yet written, and the tree as the trace feed
    has built it. It is a mutable value rather than component state because
    half of what fills it arrives from outside the tree, where hooks do not
    reach.

    {1 The panels are tabs}

    Not a preference. A HUD is drawn in the framebuffer's pixels rather than
    the window's, and the engine renders at whatever whole-number fraction of
    the window keeps it under {!Camlcast.Config.max_render_height} — so there
    are at most forty-eight lines of six-by-ten type for everything, however
    large the display. {!Camlcast_edit.Panel} is where that is worked out.
    Two panels side by side would each get half of too little. *)

type panel =
  | Plan
  | Graph
  | Tree
      (** Which view is up. {!constructor-Plan} is one room from above, {!constructor-Graph} is how the rooms
    are joined, {!constructor-Tree} is what the description came to and what it is
    holding. *)

type t

val create :
  ?read:(string -> (string, [ `Msg of string ]) result) ->
  ?write:(string -> string -> (unit, [ `Msg of string ]) result) ->
  unit ->
  t
(** A session showing nothing yet.

    [read] and [write] default to the file system. A test gives its own and
    touches no disk. *)

val watch : t -> Camlcast.Watch.t
(** What to hand {!Camlcast.Run.on}: the trace this builds its tree from, and
    the patch that puts pending edits into each frame. *)

val pointer : t -> Camlcast.P.t
(** The pointer, while the overlay is up: {!Camlcast.P.cursor} when it is open
    and nothing when it is not.

    {b A child of the world rather than of the hud}, which is why it is a
    second thing to place rather than part of {!overlay}. Under it the mouse is
    loose and does not turn the eye, and {!Camlcast.Run.aiming} is already
    false — so dragging a corner cannot work the door the panel is drawn over,
    and nothing in the world is told the crosshair arrived on it. *)

val overlay : t -> ?font:Camlcast.Font.t -> unit -> Camlcast.P.t
(** The panel, to put in a description's {!Camlcast.P.hud}.

    {b It reads its own keys}, so a game places this and has an editor rather
    than an editor's parts. [F5] the plan, [F6] the graph, [F7] the tree, [F8]
    closes it, and [Tab] moves the plan to the next room; with something
    picked, [\[] and [\]] walk its fields and [-] and [=] move the chosen
    one, while [u] takes the last change back.

    {b F5 and not F1}, because those are the keys a run leaves free. Reading a
    key here does not take it from the loop: {!Camlcast.Controls.default} binds
    [F3] to the overhead map, and a panel on [F3] drew itself and the run's own
    map of the same room on the same frame. Escape, [E] and [F11] are spoken
    for as well, and [F1] through [F4] are left alone because a game is
    likelier to want them than [F5] through [F8].

    The four that choose a panel are read whether or not it is showing, because
    the one that opens it would otherwise only work while it was already open.

    Draws nothing at all while the session is closed, so a game may leave this
    in its description permanently and pay a match on a boolean for it.

    [font] is the game's, where it has one. Left out, the panel draws with
    {!Camlcast_edit.Typeface.builtin} — which is why this works in a game that
    is one room and no pictures, and why the overlay can be turned on at the
    first step of a guide rather than the thirteenth. *)

(** {1 What it is doing} *)

val open_ : t -> unit
val close : t -> unit
val is_open : t -> bool

val show : t -> panel -> unit
(** Put this panel up. Opens the session if it was closed, since asking for a
    panel is asking to see it. *)

val panel : t -> panel

val room : t -> int
(** Which room the plan is of. A world has no frame to draw two in — see
    {!Camlcast_edit.Sheet} — so a plan is of one, and this says which. *)

val show_room : t -> int -> unit

val selected : t -> Sheet.hit
(** What was last picked on the plan. *)

val pending : t -> int
(** How many edits are being previewed and not yet written. Above zero means
    the screen and the source agree and the built program does not yet. *)

val said : t -> string option
(** What the last thing it did came to — a file written, or why one was not.

    Held rather than printed, because an overlay drawn over a game is the only
    place a game developer is looking. *)

val undo : t -> (Revise.undone option, [ `Msg of string ]) result
(** Take back the last change written. *)

val lines : t -> Panel.line list
(** What the panel says, for the session as it stands.

    Exposed rather than only drawn so that a test can read the overlay without
    a window, which is the same reason {!Camlcast.Run.aiming} and
    {!Camlcast.Run.carry} are exposed. *)

val play :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?controls:Camlcast.Controls.t ->
  t ->
  Camlcast.P.t ->
  (Camlcast.Run.ending, [ `Msg of string ]) result
(** Open a window and play this description with the overlay watching it.

    The description is the game's own and is not wrapped: what this adds is the
    {!Camlcast.Watch} handlers. A game that wants the panel drawn puts
    {!overlay} in its own HUD, where it can decide what it sits over. *)
