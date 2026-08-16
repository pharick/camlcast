(** Applies {!Prim.may_contain} to a whole tree.

    {!Prim} states which nestings mean something; this module walks a forest
    asking that rule of every node. A rule with two readers needs the asking
    shared as well as the rule. The asking was once not shared, and the two
    readers drifted exactly there:

    - {!Host} checked only the four primitives that hold anything, while
      {!Check} recursed everywhere. A child hung on a doorway or a camera passed
      {!Host} unexamined, so a description this engine ran was one the checker
      written to vet it called wrong.
    - The hud drifted in the other direction: every descendant of the hud was
      asked about as though its parent were the hud itself. A bar inside a
      rectangle therefore passed {!Host} and failed in {!Check}.

    Nothing here decides anything. This module reports pairs and leaves both
    readers to act on them: {!Host.assemble} raises on the first pair, and
    {!Check} turns every pair into a diagnostic naming the component that wrote
    it. Those are the two jobs the rule always had; this is the one traversal
    they now share.

    Internal to the library — [camlcast.ml] does not re-export it. *)

val misplaced :
  parent:Prim.t ->
  Prim.t Camlcast_loom.Host.node ->
  (Prim.t Camlcast_loom.Host.node * Prim.t) list
(** [misplaced ~parent node] is every descendant of [node] that may not be where
    it is, each paired with the parent that may not hold it. [parent] is what
    [node] itself is; the caller knows that and the node does not carry it.

    Empty for a tree that is nested correctly. A node's own misplaced children
    come before anything found under them, so the first pair is never a
    consequence of one above it. That makes the first pair the mistake to report
    when only one is going to be. Between siblings the walk is depth-first all
    the same: the first branch's mistakes, however deep, come before the second
    branch's. *)
