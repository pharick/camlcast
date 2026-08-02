(** Where a node sits in the tree, and what to call it when something goes
    wrong.

    A path is two things at once, and it is worth being clear which is which.

    It is an {b identity}: the reconciler uses it to decide that the node it is
    looking at this frame is the same node it saw last frame, and so that the
    state hanging off it — hook slots, effects, the cached room it assembled —
    survives rather than being thrown away and built again. Two paths are
    {!equal} when they name the same place, and nothing about how that place is
    labelled enters into it.

    It is also a {b label}: when a level fails a check, the report has to say
    which part of a game wrote the offending geometry, and
    [plaza / gallery_wall] is an answer a person can act on where
    [World.make: threshold linked twice] is not. That is what {!to_string} is
    for, and the names it prints have no bearing on identity at all.

    {1 What makes two places the same place}

    A step is a child's position among its parent's children, and optionally the
    [key] the game gave it.
    {b A keyed step is identified by its key alone; an unkeyed one by its
       index.} That is React's rule and it is here for React's reason: a list of
    enemies that reorders between frames should carry each enemy's state along
    with it, and it can only do that if the enemy is known by its key rather
    than by where it currently stands. An unkeyed list that reorders does
    {e not} get that, which is why keys matter and why a game should give them
    to anything it can rearrange.

    A step's [name] is never part of this. Two paths that differ only in what
    their steps are called are the same path — the name comes from the
    component, and swapping one component for another at the same place is a
    change the reconciler decides by looking at the elements, not at the path.
*)

type step = {
  index : int;  (** position among its parent's children *)
  key : string option;
      (** what the game called this one, if it called it anything. Present, it
          replaces [index] as the step's identity. *)
  name : string option;
      (** the component's own name, for {!to_string}. Cosmetic: it takes no part
          in {!equal} or {!compare}. *)
}
(** One hop from a parent to one of its children.

    Concrete and open, because everything about it is read and nothing about it
    is an invariant — unlike {!t}, whose representation is its own business. *)

type t
(** A place in the tree, as the chain of steps that reaches it from the root.

    Abstract, for one reason: it is built by {!child} on the hot path — once per
    node per frame — and the representation exists so that adding a step is O(1)
    and shares everything above it with the path it was grown from. A caller
    handed the chain itself would find that sharing very easy to break. Ask
    {!steps} for a copy when a copy is what is wanted. *)

val root : t
(** The empty path: the tree's own root, above every component a game writes. *)

val child : t -> ?key:string -> ?name:string -> int -> t
(** [child parent ?key ?name index] is the path of [parent]'s [index]th child.

    O(1), and shares [parent] rather than copying it. *)

val depth : t -> int
(** How many steps from {!root}, which is [0] for {!root} itself. *)

val steps : t -> step list
(** The chain, outermost first — the order a person reads it in, and the reverse
    of the order it is built in. Freshly allocated, so it is the caller's. *)

val equal : t -> t -> bool
(** Whether two paths name the same place, by the rule at the top of this page:
    keyed steps by key, unkeyed steps by index, names ignored throughout. *)

val compare : t -> t -> int
(** A total order agreeing with {!equal}, so paths can key a [Map] or be sorted
    into a stable order for a report. The order itself means nothing — it is
    neither tree order nor draw order — and nothing should be read into it. *)

val to_string : t -> string
(** The path as a person should see it: named steps joined by [" / "], outermost
    first, a keyed step carrying its key in brackets after the name.

    {v plaza / enemies / goblin[patrol-3] v}

    Steps with neither a name nor a key are dropped rather than printed as bare
    numbers: a wall is the fourth child of its room, and saying so helps nobody.
    A path with nothing named anywhere along it prints as ["(root)"].

    This is the form to show a game developer, and it is lossy on purpose. Use
    {!to_debug_string} for one that is not. *)

val to_debug_string : t -> string
(** Every step, joined by ["/"], keeping the ones {!to_string} drops: a keyed
    step by its name and key, and an unkeyed one by its name and its index
    behind a ["#"] — either of which may be missing its name.

    {v plaza#0/#3/torch[north]/#0 v}

    Two different places never print the same way here, which is what a trace of
    a frame needs and what makes it worth having a second spelling at all. The
    index is on every unkeyed step for exactly that reason, including the named
    ones: [torch (); torch ()] is two places with one name between them, and
    [torch#0] and [torch#1] are what tell them apart. A key needs no index
    beside it, being unique among its siblings by the rule {!Element} enforces.

    It holds for any two paths, given component names with no ["/"] or ["#"] in
    them — the two characters the spelling is made of. Nothing enforces that,
    and nothing needs to while names are written the way {!Element.declare}'s
    examples write them; it is said because the promise above is otherwise not
    quite true. *)
