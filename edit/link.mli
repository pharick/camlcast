(** From a thing in the world to the line that describes it.

    A frame is a forest of {!Camlcast_loom.Host.node}, and each node carries the
    position of the expression that built it — when [ppx_camlcast] put one
    there. This turns that position back into a place in a file, by parsing the
    file and finding the expression that starts there.

    {1 The answer is often "not that"}

    Three things can be true of a node, and a tool that showed only the third
    would be lying about the other two:

    - it carries no position at all, because the game was built without the
      rewriter, or because what described it is not one of the constructors the
      rewriter wraps;
    - it carries one, and the file cannot be read or does not parse;
    - it carries one and the expression is there, in which case which of its
      arguments can be changed is {!Camlcast_edit.Span.type-value}'s answer and
      is asked per argument.

    A world built by a function — twelve corners from [List.init] — lands in the
    third case with every argument {!Camlcast_edit.Span.Computed}. That is the
    honest result: the thing is drawn, it is walkable, it can be looked at, and
    it cannot be dragged. Nothing here refuses a file for being written the way
    its author wanted. *)

type source = {
  file : string;  (** as the compiler was given it, so relative to the build *)
  line : int;
  call : Span.call;
}
(** Where a node was written, and what is there. *)

type found =
  | Unpositioned
      (** no position on the node. Not an error: a release build carries none by
          design, and so does any build without the rewriter. *)
  | Unreadable of string
      (** a position, and a file this could not get to or make sense of *)
  | Found of source

type t
(** Files read and parsed so far. *)

val create : (string -> (string, [ `Msg of string ]) result) -> t
(** A link that reads files with this.

    The reader is given rather than assumed so that nothing here touches a disk:
    a test drives it from strings, and a game gives it something that reads its
    own source. Each file is read and parsed once and kept. *)

val find : t -> Camlcast.Prim.t Camlcast_loom.Host.node -> found
(** Where this node was written. *)

val locate : t -> Camlcast_loom.Element.pos option -> found
(** The same, for a position held on its own. *)

val parsed : t -> string -> (Span.t, [ `Msg of string ]) result
(** The file at this path, read and parsed, from the same cache {!find} uses.

    For the things that need the file rather than one call in it —
    {!Camlcast_edit.Graph.join} appends to the world's own list of children and
    has to see where that list ends. Reading it a second time would be a second
    answer about a file somebody may be editing. *)

val editable : source -> bool
(** Whether anything about this call can be dragged: whether any argument is
    {!Camlcast_edit.Span.Numbers}.

    A call with none is one to draw differently rather than one to hide. *)
