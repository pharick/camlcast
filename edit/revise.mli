(** Changing a file on disk, and taking the change back.

    {1 The first thing here that writes}

    Nothing else in this engine does. {!Camlcast.Asset} reads, and a grep
    across the three libraries for [open_out] finds nothing — an engine holds
    no content and has nothing to put anywhere. This is the exception, it is
    dev-time only, and a game that ships does not link it.

    Being the exception, it follows the one precedent there is, which is the
    House's own save file: {b write a temporary file beside the target and
    arrive by one rename.} A rename within a directory is atomic, so the file
    is either what it was or what it is meant to be and never half of each. A
    crash mid-write costs the temporary and nothing else.

    {2 What it will not write}

    Text that does not parse. An editor's whole claim is that it changes one
    expression and leaves the file alone, and the cheapest check that it has
    done so is that the result is still OCaml. A refusal here means an edit was
    built wrongly — a span landing mid-token, two edits that should have been
    refused as overlapping — and the file on disk is untouched, which is the
    only outcome worth having when the alternative is a game's source left
    broken by a tool.

    {1 Taking it back}

    An undo is not a reverse edit; it is the text that was there. Working out
    the inverse of a splice would be arithmetic with an off-by-one in it, and
    what it would buy is memory this does not need — a source file is small,
    and holding the whole of one costs less than the picture on screen. *)

type t
(** Files changed so far, and what they held before. *)

val create :
  read:(string -> (string, [ `Msg of string ]) result) ->
  write:(string -> string -> (unit, [ `Msg of string ]) result) ->
  t
(** A history that reads and writes with these.

    Given rather than assumed, for the reason {!Camlcast_edit.Link} takes a
    reader: a test drives this without a disk, and the disk is reached through
    {!to_disk} where it is wanted. *)

val to_disk : unit -> string -> string -> (unit, [ `Msg of string ]) result
(** A writer that puts text in a file: to a temporary beside it, then one
    rename over it. *)

val from_disk : unit -> string -> (string, [ `Msg of string ]) result
(** A reader that opens a file. *)

val apply :
  t ->
  path:string ->
  (Span.span * string) list ->
  (unit, [ `Msg of string ]) result
(** Splice these edits into that file and write it.

    Reads the file, applies every edit at once — see {!Span.splice}, which
    refuses two that overlap — checks the result parses, writes it, and records
    what was there before.

    The file is read afresh rather than taken from anything held here, so an
    edit made against a file somebody has changed underneath fails on the parse
    or lands where it should not. That is the reason {!Camlcast_edit.Link}
    keeps what it parsed and this does not: one is answering questions about a
    frame, the other is about to write. *)

val write : t -> path:string -> string -> (unit, [ `Msg of string ]) result
(** Put this text in that file, checking it parses and recording what was
    there. What {!Camlcast_edit.Scaffold} writes a new component with — there
    being no edits to splice into a file that does not exist yet. *)

val depth : t -> int
(** How many changes are recorded. *)

type undone =
  | Restored of string
      (** the file held something before, and holds it again *)
  | Created of string
      (** the file did not exist before this session wrote it, and is still
          there.

          {b Deleting it is not this module's decision.} Removing a file is the
          one thing here that cannot be taken back in turn, and a file the
          editor made a minute ago may have been added to by hand since. So the
          creation is undone as far as it safely can be — which is not at all —
          and the caller is told which file it was, to offer or not. *)

val undo : t -> (undone option, [ `Msg of string ]) result
(** Take back the most recent change, and say what that came to.

    [None] when there is nothing left, which is a state and not a failure — a
    menu offering undo should be able to ask. *)
