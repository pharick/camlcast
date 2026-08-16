(** A leaf hung in a doorway: whether it stands across the opening, and what
    material it is made of.

    {b Two states, not three.} A locked door is a closed door the game will not
    let the player open; that is the whole difference. Nothing in the engine
    ever behaved differently for a locked door — it drew the same leaf and
    refused the same step — so carrying the distinction put a game rule
    somewhere that could not act on it. The engine owns what a door {e does}: it
    stands in the way, or it does not. Whether trying it achieves anything is
    the game's to decide and store.

    That division has a cost. A door has two sides, so a game that locks doors
    keeps two entries per door in its own record and must keep them
    synchronised. The engine keeps {e its} two sides synchronised — see
    {!World.set_door} — but only for the part it knows about.

    This is a separate module rather than another type inside {!Room}, beside
    {!Room.type-lintel}, for two reasons. A door has behaviour where a lintel is
    only a measurement: the state decides both what is drawn across the opening
    and whether a step is refused, and those two answers must never disagree.
    Also, {!Room} has already used the name [Open] for a ceiling with sky in it.
*)

(** Whether the leaf is standing in the opening. Two states, not three, for the
    reason above: locked is a game concept, and to the engine a locked door is a
    {!Closed} one the game declines to open. *)
type state =
  | Open  (** swung aside: not drawn, and not blocking *)
  | Closed  (** standing across the opening: drawn, and blocking *)

type t = private { state : state; material : Material.t }
(** Private: both fields stay readable, but a door is made by {!make} and
    changed by {!set_state}.

    Those are the only two operations allowed on a door. A door's material is
    chosen when it is hung and kept for the rest of the run, including while it
    stands open — see {!make} — and the state is the half a game drives. Writing
    the record by hand risks swapping the two by accident. *)

val make : ?state:state -> Material.t -> t
(** Hangs a leaf, closed unless told otherwise; closed is the default because
    opening is the interaction a door exists for.

    The material is what the leaf is drawn as, and it is kept while the door
    stands open: a door that is opened and shut again is the same door. The two
    sides of a link may be made of different materials while agreeing about the
    state (see {!World.link}). *)

val set_state : t -> state -> t
(** The same leaf, open or closed. The material is carried over unchanged. That
    is why this exists rather than a record update at the call site:
    {!World.set_door} changes both sides of a link at once and must not be able
    to change what either side is made of while doing it. *)

val leaf : t -> Material.t option
(** What to draw across the opening: [Some material] for a closed leaf, [None]
    for an open one. [None] draws nothing and the room beyond shows through.

    This lives here rather than in {!Renderer} so that it can be tested. The
    renderer needs a live SDL surface; choosing what to draw needs nothing, and
    the choice is what a door's state decides, not the pixels.

    It is also the whole of whether a door blocks a step: a leaf in the opening
    is what cannot be walked through, which is why {!Room.shut} asks this rather
    than reading the state a second time.

    It does {e not} decide whether the room beyond is drawn. That is the
    material's decision, exactly as for a wall: a see-through leaf is drawn over
    the neighbouring room rather than instead of it, and {!Sight} sees through
    it for the same reason — but it still refuses the step, as a grille wall
    does. *)
