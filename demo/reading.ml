(** The one exception the demos raise, for art they cannot read.

    Engine loaders return [result], and demo code that can propagate an answer
    does so. A world forced inside a frame cannot: {!Loading}, {!Typeface} and
    {!Text} hold theirs behind a [lazy] so a missing file does not stop
    [camlcast-demo --list] from listing, and by the time one is forced there is
    nowhere for an [Error] to go. They raise, and {!Catalogue.attempt} turns the
    raise back into the [`Msg] the launcher reports.

    A dedicated exception costs a module of four lines and lets the catching
    side name what it catches. Catching [Failure] would do if those three raised
    it with [failwith] — except that any [List.nth] off the end of a list inside
    a demo's frame raises [Failure] too, and would reach the launcher as an
    unreadable-art report naming an unrelated file.

    [Invalid_argument] is deliberately still not caught: it signals the other
    kind of mistake — a world that does not join up, a font atlas the wrong
    shape — and should stop the program with its name. *)

exception Unreadable of string

(** [or_raise what result] is the value, or raises [Unreadable] with [what] and
    the loader's message.

    [what] names the thing that could not be read, because the message is all
    that reaches the launcher — the point of raising rather than crashing is
    telling the player which file to check. *)
let or_raise what = function
  | Ok value -> value
  | Error (`Msg message) -> raise (Unreadable (what ^ ": " ^ message))
