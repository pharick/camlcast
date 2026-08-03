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

type t
(** A place in the tree, as the chain of steps that reaches it from the root.

    Abstract, and the chain itself is not handed out. It is built by {!child} on
    the hot path — once per node per frame — and the representation exists so
    that adding a step is O(1) and shares everything above it with the path it
    was grown from, which a caller holding the chain would find very easy to
    break. The other half is that there is nothing to do with the steps that is
    not one of the four questions below, and a fifth export would mostly be an
    invitation to re-implement {!equal} by matching on them — which is how the
    keyed-versus-unkeyed rule ends up written down twice and then only fixed
    once. *)

val root : t
(** The empty path: the tree's own root, above every component a game writes. *)

val child : t -> ?key:string -> ?name:string -> int -> t
(** [child parent ?key ?name index] is the path of [parent]'s [index]th child:
    where it stands among its siblings, the [key] the game gave it if it gave it
    one, and the component's own [name] for the two spellings below.

    O(1), and shares [parent] rather than copying it. *)

val equal : t -> t -> bool
(** Whether two paths name the same place, by the rule at the top of this page:
    keyed steps by key, unkeyed steps by index, names ignored throughout.

    The only question about identity there is, and there is deliberately no
    ordering beside it. A total order would have to decide whether a keyed step
    sorts before an unkeyed one — a choice with nothing behind it, since the two
    are never the same place and no traversal visits them in that order — and
    then something would key a [Map] by it and inherit the arbitrary part. Paths
    are compared to each other and printed; nothing has needed them sorted. *)

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

    It holds for any two paths, given component names with no ["/"], ["#"] or
    ["["] in them and keys with no ["]"] or ["/"] — the characters the spelling
    is made of, which a name or key containing them can counterfeit. Nothing
    enforces that, and nothing needs to while names are written the way
    {!Element.declare}'s examples write them; it is said because the promise
    above is otherwise not quite true. *)
