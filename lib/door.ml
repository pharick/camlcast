(** A leaf hung in a doorway: whether it stands across the opening, and what it
    is made of.

    {b Two states, not three.} A locked door is a closed one that the game will
    not let the player open, and that is the whole of the difference. Nothing
    here ever behaved differently for one — it drew the same leaf and refused
    the same step — so carrying the distinction only put a game's rule somewhere
    that could not act on it. What the engine owns is what a door {e does}: it
    stands in the way, or it does not. Whether trying it achieves anything is a
    game's to decide and a game's to keep.

    That division has a price worth knowing before taking it. A door has two
    sides, so a game that locks doors keeps two entries per door in its own
    record and has to keep them in step. The engine keeps {e its} two sides in
    step for you — see {!World.set_door} — but only for the part it knows about.

    This is a module of its own rather than another type inside {!Room}, beside
    {!Room.type-lintel}, for two reasons. A door has behaviour where a lintel is
    only a measurement — the state decides both what is drawn across the opening
    and whether a step is refused, and those two answers must never be allowed
    to disagree. And {!Room} has already spent the name [Open] on a ceiling with
    sky in it. *)

type state =
  | Open  (** swung aside: nothing to see, and nothing to walk into *)
  | Closed  (** standing across the opening: drawn, and in the way *)

type t = { state : state; material : Material.t }

(** Hang a leaf. Shut unless told otherwise: a door is a thing you open.

    The material is what the leaf is drawn as, and it is kept while the door
    stands open — a door that is opened and shut again is the same door, and the
    two sides of a link may be made of different things while agreeing about the
    state (see {!World.link}). *)
let make ?(state = Closed) material = { state; material }

(** What to draw across the opening: a closed leaf, of its own material. An open
    one draws nothing and the room beyond shows through instead.

    This is here rather than in {!Renderer} so that it can be tested. The
    renderer needs a live SDL surface; choosing what to draw needs nothing, and
    it is the choice rather than the pixels that a door's state decides. *)
let leaf t = match t.state with Closed -> Some t.material | Open -> None

(** Does it stop a step? Exactly when there is a leaf across the opening to stop
    one — which is why this is asked of {!leaf} rather than of the state a
    second time. What you cannot see through is what you cannot walk through. *)
let shut t = Option.is_some (leaf t)
