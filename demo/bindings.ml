(** {b The demos' controls.} One table, shared by all of them, because they all
    want the same thing: the engine's defaults for walking and looking, plus
    Escape to leave.

    Leaving is the part the engine will not assume — see
    {!Camlcast.Binding.default}. A demo is opened by the launcher and has to
    hand the player back to the list somehow, so it asks; a game with a pause
    screen of its own would want that key for the screen instead, and would
    write its own line here rather than this one.

    This is also all a game has to do to rebind anything: state a table once and
    pass it wherever it opens a window. {!Camlcast_demo.Controls} does the
    larger version, with the walking keys moved. *)

open Camlcast

let escapable = Binding.make ~leave:[ Input.Key Key.escape ] ()
