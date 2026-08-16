(** {b The demos' controls.} One record shared by all demos: WASD and the mouse
    to walk, E to work a door, F3 for the map, Escape back to the launcher's
    list.

    This equals {!Camlcast.Controls.default}, written out here so the demo
    source states its own bindings; a demo wanting a different exit key or map
    key would change this one line and nothing else.

    Rebinding in a game works the same way: state a record once and pass it
    wherever a run starts. {!Camlcast_demo.Controls} is the larger version, with
    the walking keys moved. *)

open Camlcast

let escapable =
  Controls.make
    ~bindings:(Binding.make ~leave:[ Input.Key Key.escape ] ())
    ~use:[ Input.Key Key.e ] ~map:[ Input.Key Key.f3 ] ()
