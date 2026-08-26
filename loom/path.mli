(** Where a node sits in the tree, and what to call it when something goes
    wrong.

    A path plays two roles, and they are independent.

    It is an {b identity}. The reconciler uses it to decide that the node it
    sees this frame is the node it saw last frame, so that the state hanging off
    it — hook slots, effects, the cached room it assembled — survives rather
    than being thrown away and rebuilt. Two paths are {!equal} when they name
    the same place; how that place is labelled does not enter into it.

    It is also a {b label}. When a level fails a check, the report must say
    which part of a game wrote the offending geometry, and
    [plaza / gallery_wall] is an answer a person can act on where
    [World.make: threshold linked twice] is not. That is what {!to_string} is
    for, and the names it prints have no bearing on identity at all.

    {1 What makes two places the same place}

    A step is a child's position among its parent's children, and optionally the
    [key] the game gave it.
    {b A keyed step is identified by its key alone; an unkeyed one by its
       index.} That is React's rule, kept for React's reason: a list of enemies
    that reorders between frames should carry each enemy's state along with it,
    and that only works if the enemy is known by its key rather than by where it
    currently stands. An unkeyed list that reorders does {e not} get that. Give
    a key to anything the game can rearrange.

    A step's [name] is never part of identity. Two paths that differ only in
    what their steps are called are the same path. The name comes from the
    component, and swapping one component for another at the same place is a
    change the reconciler decides by looking at the elements, not at the path.
*)

type t
(** A place in the tree, as the chain of steps that reaches it from the root.

    Abstract, and the chain itself is not handed out. {!child} builds it on the
    hot path, once per node per frame; the representation makes adding a step
    O(1) and shares everything above it with the parent path. A caller holding
    the chain could easily break that sharing.

    There is also nothing to do with the steps beyond the questions below, and
    {!parent} is the shape any addition to them has to take: it answers a
    question {e about} a path rather than handing over what one is made of. An
    export that gave out the steps would mostly invite re-implementing {!equal}
    by matching on them, which is how the keyed-versus-unkeyed rule ends up
    written down twice and then only fixed once. *)

val root : t
(** The empty path: the tree's own root, above every component a game writes. *)

val child : t -> ?key:string -> ?name:string -> int -> t
(** [child parent ?key ?name index] is the path of [parent]'s [index]th child:
    where it stands among its siblings, the [key] the game gave it if any, and
    the component's own [name] for the two spellings below.

    O(1), and shares [parent] rather than copying it. *)

val parent : t -> t option
(** Where this sits inside, and [None] for {!root}.

    O(1), and the result shares this path's tail rather than copying it, for the
    reason {!child} does.

    This is what turns a flat report of paths back into a tree. {!Trace} says
    what happened at each place and not what contains what; its events do arrive
    in the order the reconciler walked, but reading structure out of an order is
    an assumption that holds until the first frame a render is refused, and
    {!Trace.Refused} exists because such frames report a walk rather than a
    tree. Asking a path what it sits inside is not an assumption.

    It hands out no step. Nothing here shows a caller a key or an index, so
    nothing here lets one re-decide what makes two places the same place — that
    question has one answer and it is {!equal}. *)

val equal : t -> t -> bool
(** Whether two paths name the same place, by the rule at the top of this page:
    keyed steps by key, unkeyed steps by index, names ignored throughout.

    This is the only question about identity, and there is deliberately no
    ordering beside it. A total order would have to decide whether a keyed step
    sorts before an unkeyed one — a choice with nothing behind it, since the two
    are never the same place and no traversal visits them in that order.
    Something would then key a [Map] by it and inherit the arbitrary part. Paths
    are compared to each other and printed; nothing has needed them sorted. *)

val to_string : t -> string
(** The path as a person should see it: named steps joined by [" / "], outermost
    first, a keyed step carrying its key in brackets after the name.

    {v plaza / enemies / goblin[patrol-3] v}

    Steps with neither a name nor a key are dropped rather than printed as bare
    numbers. A wall is the fourth child of its room, and saying so helps nobody.
    A path with nothing named anywhere along it prints as ["(root)"].

    This is the form to show a game developer, and it is lossy on purpose. Use
    {!to_debug_string} for one that is not. *)

val to_debug_string : t -> string
(** Every step, joined by ["/"], keeping the ones {!to_string} drops: a keyed
    step by its name and key, and an unkeyed one by its name and its index
    behind a ["#"] — either of which may be missing its name.

    {v plaza#0/#3/torch[north]/#0 v}

    Two different places never print the same way here. A trace of a frame needs
    that, and it is what makes a second spelling worth having at all. The index
    appears on every unkeyed step for exactly that reason, including the named
    ones: [torch (); torch ()] is two places with one name between them, and
    [torch#0] and [torch#1] are what tell them apart. A key needs no index
    beside it, being unique among its siblings by the rule {!Element} enforces.

    The never-prints-the-same promise holds for any two paths, given component
    names with no ["/"], ["#"] or ["["] in them and keys with no ["]"] or ["/"]
    — the characters the spelling is made of, which a name or key containing
    them can counterfeit. Nothing enforces that, and nothing needs to while
    names are written the way {!Element.declare}'s examples write them. It is
    stated because the promise is otherwise not quite true. *)
