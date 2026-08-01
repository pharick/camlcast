(** {b The demos' controls.} One record, shared by all of them, because they all
    want the same thing: walk with WASD and the mouse, work a door with E, F3
    for the map, and Escape to hand the player back to the launcher's list.

    Which is {!Camlcast.Controls.default} exactly, and it is written out here
    all the same. A demo is a file you read, and the line that says what it
    answers to is worth having in it — a demo that wanted its own way out, or
    the map on another key, would change this one line and nothing else.

    That is also the whole of what a game does to rebind anything: state a
    record once and pass it wherever it starts a run. {!Camlcast_demo.Controls}
    does the larger version, with the walking keys moved. *)

open Camlcast

let escapable =
  Controls.make
    ~bindings:(Binding.make ~leave:[ Input.Key Key.escape ] ())
    ~use:[ Input.Key Key.e ] ~map:[ Input.Key Key.f3 ] ()
