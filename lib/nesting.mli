(** Applying {!Prim.may_contain} to a whole tree.

    {!Prim} says which nestings mean something and this walks a forest asking,
    because a rule with two readers needs the {e asking} shared as well as the
    rule. It did not use to be, and the two drifted exactly there: {!Host}
    checked only the four primitives that hold anything and let a child hung on
    a doorway or a camera through unlooked-at, while {!Check} recursed
    everywhere — so a description this engine ran happily was one the checker
    written to vet it called wrong. The hud was worse in the other direction:
    every descendant of it was asked about as though its parent were the hud
    itself, so a bar inside a rectangle passed there and failed in {!Check}.

    Nothing here decides anything. It reports pairs and leaves both readers to
    say what they do about them: {!Host.assemble} raises on the first, {!Check}
    turns every one into a diagnostic naming the component that wrote it. Those
    are the two jobs the rule always had; this is the one traversal they now
    share.

    Internal to the library — [camlcast.ml] does not re-export it. *)

val misplaced :
  parent:Prim.t ->
  Prim.t Camlcast_loom.Host.node ->
  (Prim.t Camlcast_loom.Host.node * Prim.t) list
(** [misplaced ~parent node] is every descendant of [node] that may not be where
    it is, each paired with the parent that may not hold it. [parent] is what
    [node] itself is, which the caller knows and the node does not carry.

    Empty for a tree that is nested correctly. A node's own misplaced children
    come before anything found under them, so the first pair is never a
    consequence of one above it — the mistake to report when only one is going
    to be. Between siblings the walk is depth-first all the same: the first
    branch's mistakes, however deep, come before the second branch's. *)
