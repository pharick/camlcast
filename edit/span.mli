(** Finding an expression in a source file, and replacing the bytes it occupies.

    {1 Parsed to locate, spliced to change}

    Nothing here prints OCaml. A file is parsed only to find out {e where}
    things are, and a change is a replacement of a byte range in the text that
    was parsed. Everything not replaced survives exactly: comments, blank lines,
    alignment, and whatever [ocamlformat] had already made of it.

    That is worth stating as a rule rather than as an implementation note,
    because the obvious alternative — change the tree, print the tree — cannot
    keep any of it. It also holds the dependency on the compiler's own parser
    down to the one thing that has stayed put across releases: where an
    expression starts and stops. Nothing here reconstructs syntax, so nothing
    here has to be rewritten when the syntax grows.

    {1 What can be changed}

    A wall written [wall a b] with [a] computed by a function is a wall the
    editor can draw and cannot drag, and saying so is better than refusing the
    file. {!type-value} is that answer, per argument: numbers can be dragged, a
    name can be swapped for another name, and anything else can only be looked
    at. *)

type span = { start : int; stop : int }
(** A range of bytes in the file: [start] included, [stop] excluded. *)

type value =
  | Numbers of span list
      (** built out of numeric constants and nothing else, and these are where
          they are, in the order they are written.

          [Vec.make (-3.) (-4.)] is two of them. So is a bare [2.]. What makes
          this the editable case is that replacing those bytes with other
          numbers leaves a program that means the same kind of thing.

          {b A call counts only when its name is one that builds a value.}
          [Vec.make 1. 2.] and [plaza_corner 0] are the same shape — an
          identifier applied to numeric constants — and nothing available here
          tells them apart, this running long before types do. So a short list
          of names does: [Vec.make], [Plane.make], [Plane.horizontal],
          [Color.rgb], [Color.level]. Anything else applied to anything is
          {!Computed}.

          Getting that wrong in the other direction is the worst thing a tool
          which edits source can do. Read as a point, [plaza_corner 0] takes a
          coordinate written over the {e function's argument} —
          [plaza_corner (-2.5)] — which compiles, runs, and means something
          else. *)
  | Name of span
      (** a plain identifier — [Surfaces.stone], [flat]. Not a number and not
          computed: a thing referred to by name, which an editor can swap for
          another name out of the same palette but cannot drag. *)
  | Computed
      (** anything else: a call with arguments of its own, a conditional, a
          variable. It may differ from frame to frame, which is exactly why it
          cannot be written back to. See {!Link}. *)

type argument = { label : string option; span : span; value : value }
(** One argument of a call. [label] is [Some "height"] for [~height:2.] and
    [None] for a positional one. *)

type call = {
  callee : string;  (** as written: ["wall"] under an open, ["P.wall"] not *)
  span : span;  (** the whole application *)
  arguments : argument list;  (** in the order written *)
}

type t
(** A file, parsed. Holds the text as well, since a splice is against the very
    bytes that were parsed and against no other copy of them. *)

val parse : path:string -> string -> (t, [ `Msg of string ]) result
(** Parse this text. [path] is only what a syntax error names.

    An error is a condition and not a mistake in the program: the file being
    edited is the game developer's, and it may be halfway through being typed.
*)

val text : t -> string
(** The text as it was parsed. *)

val calls : t -> call list
(** Every application in the file whose head is a plain name, outermost first.

    What {!at} searches. Exposed because an editor wants the list for itself —
    every wall in a file, whether or not anything in the world is pointing at
    one — and because a test looking for a particular call should ask for it
    rather than work out where it must be. *)

val at : t -> line:int -> column:int -> call option
(** The application that starts exactly here, if one does.

    This is how a position from [ppx_camlcast] is turned back into a place in
    the file. It is an anchor and not a range — see
    {!Camlcast_loom.Element.pos}, which says why the position a description
    carries cannot bound anything — so the match is on where the expression
    begins, and everything else is read off this parse.

    {b Where an expression begins is the parser's answer, not the eye's.} A
    parenthesised one starts at its opening bracket, so [(Plane.make ~a:0.)]
    is anchored a column before the [P]. That costs nothing where it matters:
    the rewriter stamps the same location this reads back, so the two agree by
    construction. It costs something to anything that works out where a call
    must be by looking for its name, which is what {!calls} is for. *)

val number : float -> string
(** A float written so that it parses wherever a number can be replaced.

    {b Negative numbers are parenthesised, and that is not a matter of taste.} A
    constant's span is the bytes the parser gave it, and for a negative literal
    written [(-3.)] those bytes include the brackets — so a caller splicing a
    bare [-3.5] over them leaves [Vec.make -3.5 (-4.)], which is not that call
    with a different number in it but a subtraction. Bracketing every negative
    is correct against a span that had brackets and against one that did not,
    which is why it is done here rather than decided per site.

    [3.] rather than [3] for a whole number, floats being what a description
    measures in. *)

val slice : t -> span -> string
(** The bytes this span covers, as they are written.

    What an editor reads a name off with: the identifier a doorway was bound
    to, the number a coordinate currently holds. *)

val splice : t -> (span * string) list -> (string, [ `Msg of string ]) result
(** The text with each of these ranges replaced.

    All at once, and the order they are given in does not matter: replacing one
    range changes where every later one sits, so a caller applying them one at a
    time would have to do this arithmetic itself and would get it wrong on the
    second edit. Two ranges that overlap are refused rather than resolved, there
    being no answer to which of them meant it. *)
