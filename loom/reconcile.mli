(** Matching this frame's description against last frame's tree.

    A game hands over a whole {!Element.t} every frame. Somewhere behind it is a
    tree of instances that has been alive since the frame each of its parts
    first appeared, holding everything that has to outlive a description. This
    is what puts the two together: it walks them in step, decides at each place
    whether the thing described is the thing already there, and keeps or
    replaces accordingly.

    {1 What counts as the same thing}

    - A {b component} is the same when its [render] is physically the same
      closure and its key agrees. See {!Element} for why that is the only
      sensible test and what it asks of a game in return.
    - A {b primitive} is the same when its key agrees — {e and nothing more.}
      The runtime cannot tell a wall from a sprite, because [prim] is the host's
      own type and opaque here. So a wall replaced by a sprite at the same place
      is an update rather than a teardown, and any components underneath keep
      their state. Nothing is corrupted by this: primitives are inert
      descriptions with no state of their own and nothing to release. It is
      written down because it differs from React, where a [div] becoming a
      [span] tears the subtree down, and the difference is a deliberate
      consequence of keeping {!Host.HOST} to one function.
    - {!Element.Empty} and {!Element.Fragment} match themselves.

    Anything else is a replacement: the old subtree is unmounted deepest-first
    and the new one mounted in its place.

    {1 Children}

    Among one parent's children, keyed elements are matched to last frame's by
    key wherever they have moved to, and unkeyed ones are matched in order
    against the unkeyed ones that were there before. Leftovers are unmounted in
    the order they were declared, so that a trace of a frame reads the same way
    twice. *)

module Make (H : Host.HOST) : sig
  type element = H.prim Element.t
  (** {!Element.t} at this host's primitive, which is the type a game writes. *)

  type t
  (** A mounted root: the instance tree, and everything hanging off it.

      Made once and rendered into repeatedly. Two roots share nothing, which is
      what lets a test drive several in one process — and, later, what lets a
      launcher hold a menu and a world at the same time. *)

  val create : unit -> t
  (** An empty root. The first {!render} into it mounts everything. *)

  val render : ?trace:(H.prim Trace.event -> unit) -> t -> element -> H.scene
  (** [render root description] reconciles [description] against what [root]
      already holds, commits the result, and returns the scene the host
      assembled from it.

      [trace], if given, is called with every mount, update and unmount as they
      happen, in the order they happen. Left out, nothing is recorded and
      nothing is spent recording it. *)
end
