(** What the reconciler did, for anyone watching.

    Reconciliation is the part of this engine that is hardest to be sure of and
    easiest to get subtly wrong: a component that is torn down and rebuilt when
    it should have been kept still looks right on screen, and only shows itself
    later as state that will not persist. So the reconciler will say what it
    did, and a test can assert on that directly rather than inferring it from
    the picture.

    Nothing here is on by default and nothing here costs anything when no
    {!type-event} handler is installed. *)

(** What a thing was. Fragments and {!Element.Empty} are not reported: neither
    survives committing, and a frame's worth of "a fragment was kept" says
    nothing anybody needs. *)
type 'prim node =
  | Component of string  (** the component's name *)
  | Primitive of 'prim  (** the primitive as the game described it *)

type 'prim event =
  | Mounted of Path.t * 'prim node
      (** it was not there last frame; its state begins now *)
  | Updated of Path.t * 'prim node
      (** it was there last frame and is the same thing, so its state carried
          over. This is the one to look for: it is the whole claim the
          reconciler makes. *)
  | Unmounted of Path.t * 'prim node
      (** it is gone, and its state went with it. Reported children first, so a
          reader — and, once there are effects to run, a cleanup — sees the
          deepest thing go first. *)

val to_string : ('prim -> string) -> 'prim event -> string
(** [to_string describe event] is one line, meant to be read in a column:

    {v
    mount    plaza/torch
    update   plaza/torch/#0 : sprite
    unmount  plaza/enemies/goblin[patrol-3]
    v}

    The path is {!Path.to_debug_string} rather than {!Path.to_string}: two
    sibling walls have to read differently here, and the friendly spelling drops
    exactly the steps that tell them apart.

    [describe] renders a primitive, since only the host knows how. *)
