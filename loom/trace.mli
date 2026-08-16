(** Events reporting what the reconciler did, for tests and debugging.

    Reconciliation is the part of this engine that is hardest to verify and
    easiest to get subtly wrong. A component torn down and rebuilt when it
    should have been kept still looks right on screen; the error only shows
    later, as state that does not persist. The reconciler therefore reports what
    it did, and a test can assert on the report directly rather than inferring
    it from the picture.

    Nothing here is on by default. Nothing here costs anything when no
    {!type-event} handler is installed. *)

(** What a reported node was. Fragments and {!Element.Empty} are not reported:
    neither survives committing, and a report that a fragment was kept carries
    no information. *)
type 'prim node =
  | Component of string  (** the component's name *)
  | Primitive of 'prim  (** the primitive as the game described it *)

type 'prim event =
  | Mounted of Path.t * 'prim node
      (** it was not there last frame; its state begins now *)
  | Updated of Path.t * 'prim node
      (** it was there last frame and is the same thing, so its state carried
          over. This is the event to assert on: it is the whole claim the
          reconciler makes. *)
  | Unmounted of Path.t * 'prim node
      (** it is gone, and its state went with it. Children are reported first,
          so the deepest node goes first — for a reader, and for a cleanup once
          there are effects to run. *)
  | Refused
      (** the render did not stand: the host would not build what it described,
          or the description itself would not finish — a duplicate key, a
          changed hook order, a component that raised. None of what was reported
          above it in the same render happened. The tree is the one from the
          frame before, with every component it held still standing and still
          holding its state.

          Emitted last, and once. The events before it record what the
          reconciler was doing when it met the refusal, which is worth having a
          trace of. Read as tree history they are not only incomplete but
          contradictory: a component reported [Unmounted] by a refused render is
          reported [Updated] by the render after, and {!Updated} means the state
          carried over. Both reports are true of the reconciler's walk; only the
          second is true of the tree. [Refused] is the marker that says which
          reading applies. *)

val to_string : ('prim -> string) -> 'prim event -> string
(** [to_string describe event] is one line, formatted to be read in a column:

    {v
    mount    plaza/torch
    update   plaza/torch/#0 : sprite
    unmount  plaza/enemies/goblin[patrol-3]
    v}

    The path is {!Path.to_debug_string} rather than {!Path.to_string}: two
    sibling walls must read differently here, and the friendly spelling drops
    exactly the steps that tell them apart.

    [describe] renders a primitive, since only the host knows how. *)
