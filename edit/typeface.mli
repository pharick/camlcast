(** The panel's own typeface, so the overlay draws before a game has any art.

    {b Why this library holds one and the engine does not.} The engine ships no
    colour, pattern or picture, because content is what a game brings and two
    games sharing an engine should not share a look. None of that applies here.
    This is a developer's panel, on a build a player never sees — a shipped game
    does not link this library at all — and it has to be readable at the point
    where the game is one room and no pictures. A tool that cannot be turned on
    until step thirteen of a guide is a tool with parts.

    So {!Camlcast_edit.Session.overlay} takes its font optionally and falls back
    to this. A game that has a face of its own may still pass it, and the panel
    will look like it belongs.

    Six by ten, code points 32 through 127, with a hollow box at ['\127'] for
    anything outside that. It is the same picture the demos draw with, carried
    as a mask in the source rather than read off the disk: a game using this
    library has no [assets/font.png] to read. *)

val builtin : Camlcast.Font.t Lazy.t
(** The face itself.

    Lazy because building it means generating a 96 × 60 mask, and a game that
    passes {!Camlcast_edit.Session.overlay} a font of its own should not pay for
    one it never draws. Forced at most once however many sessions ask. *)
