(** The one exception the demos raise, for art they cannot read.

    Every loader in the engine answers with a [result], and everything in these
    demos that can wait for an answer takes it that way. What cannot wait is a
    world forced inside a frame: {!Loading}, {!Typeface} and {!Text} each hold
    theirs behind a [lazy] so that a missing file does not stop
    [camlcast-demo --list] from listing anything, and by the time one is forced
    there is nowhere for an [Error] to go. So they raise, and
    {!Catalogue.attempt} turns the raise back into the [`Msg] the launcher
    reports on.

    This is here so that the catching side can name what it is catching. It used
    to catch [Failure], which those three raised with [failwith] — and [Failure]
    is not theirs: any [List.nth] off the end of a list, anywhere inside a
    demo's frame, arrived at the launcher dressed as a demo whose art could not
    be read, with a message about a file having nothing to do with it. One
    exception of our own costs a module of four lines and means the seam catches
    what it meant to.

    [Invalid_argument] is deliberately still not caught, and is the other kind
    of mistake: a world that does not join up, a font atlas the wrong shape.
    Stopping with one of those named is the honest report of it. *)

exception Unreadable of string

(** [or_raise what result] is the value, or [Unreadable] saying [what] and what
    the loader said about it.

    [what] names the thing that could not be read, because the message is the
    whole of what survives the trip to the launcher — the point of raising
    rather than crashing is that a player is told which file to go and look at.
*)
let or_raise what = function
  | Ok value -> value
  | Error (`Msg message) -> raise (Unreadable (what ^ ": " ^ message))
