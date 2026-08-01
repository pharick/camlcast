(** The seam. Everything the runtime needs to know about the thing it is
    driving, and nothing more.

    This library reconciles trees and keeps state alive across frames. It has no
    opinion about what those trees describe — walls and sprites, a terminal, a
    test harness printing strings. A host supplies two types and one function,
    and in exchange the whole of {!Camlcast_loom} works on it.

    Two things are deliberately absent, and both are absent for the same reason:
    a host has no [create], [update] or [destroy], and it is handed the finished
    forest rather than a stream of mutations. React's host config is written the
    other way round because the DOM is a mutable tree that must be poked node by
    node. A {!Camlcast.World} is not: it is an immutable value assembled whole,
    and the primitives that go into it are cheap records. Handing over the
    finished description and asking for a scene is both simpler and a better fit
    — and it keeps the interface small enough that a mock host is a dozen lines,
    which is what makes the reconciler testable with nothing linked in behind
    it.

    Assembling from scratch every frame is a real cost and it is not being
    ignored; it is being deferred. The paths in the forest are stable across
    frames precisely so an assembler can cache what a subtree built and reuse it
    when nothing under that path changed. That is an optimisation to make when a
    benchmark asks for it, not a shape to design around before one exists. *)

type 'prim node = {
  path : Path.t;  (** where this sits, stable from frame to frame *)
  prim : 'prim;  (** what the game asked for here *)
  children : 'prim node list;  (** in the order the game wrote them *)
}
(** One primitive, in place, with what it contains.

    A forest of these is what a render comes to once every component in it has
    been called and every fragment flattened away: components have no
    representation here, because a component's whole job is to explain itself in
    terms of things a host understands, and by this point it has.

    The nesting is the game's own — a room holding its walls, a wall holding its
    decals — and it is preserved rather than flattened because that is the shape
    an assembler wants. The {!Path.t} would let a flat list be reassembled into
    this, but only by an assembler that reimplemented the reconciler's idea of
    parenthood, which is a thing to have one copy of. *)

module type HOST = sig
  type prim
  (** One primitive as a component returns it: a wall, a sprite, a run of text.
      Almost always a variant, since a game writes several kinds. *)

  type scene
  (** What a committed forest becomes — for the raycaster, a world, a camera and
      an overlay; for a test, a string. Whatever the loop hands on to be drawn.
  *)

  val assemble : prim node list -> scene
  (** Turn a frame's finished description into something drawable.

      Called once per frame with the roots of the forest, in the order the game
      wrote them. It should be a pure function of what it is given: the
      reconciler will call it with an equal forest and expect an equal scene,
      and a cache keyed on {!Path.t} is the supported way to make that fast.

      Errors in what a game described — a room that does not close, a doorway
      linked twice — are the host's to detect, since the runtime cannot tell a
      malformed wall from a well-formed one. How they are reported is the host's
      business too; raising from here aborts the frame. *)
end
