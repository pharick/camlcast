(** What a run does with the player's controls by itself.

    A run acts on the controls for three different sorts of thing, and they used
    to arrive by three different roads: walking and looking through a
    {!Camlcast_core.Binding.t}, working whatever the crosshair is on through a
    single control, and the map over the world through a boolean with a key
    baked in behind it. Rebinding any one of them meant knowing which of the
    three roads it was on, and one of them could not be rebound at all.

    This is the one road. Every field is a part of what {!Run} acts on before
    any component has run, each of the last two is a list of
    {!Camlcast_core.Input.control}s so that a key and a mouse button can do the
    same job, and an empty list is how a game turns one off.

    What is {b not} here is anything a game means by a control — "interact",
    "chalk", "journal". Those are a game's own table from controls to actions,
    and a component reads them with {!Events.use_pressed} and its neighbours.
    Here is only what a component cannot be asked about, because the loop has to
    act on it before there is a frame to ask in. *)

open Camlcast_core

type t = {
  bindings : Binding.t;
      (** walking, looking, fullscreen, and the way out of the run *)
  use : Input.control list;
      (** any of these works whatever the crosshair is on — see {!P.wall} and
          its neighbours for [on_use] *)
  map : Input.control list;
      (** any of these turns {!Debug_map} on and off over the world.

          Empty is a game that has stopped wanting a map over its world, and it
          costs that game nothing at all: the walk over every wall of every room
          that feeds the map only happens while the map is up. *)
}
(** The whole of it. A record and not a set of arguments, so that a launcher can
    state its controls once and hand the same value to every run it plays. *)

val make :
  ?bindings:Binding.t ->
  ?use:Input.control list ->
  ?map:Input.control list ->
  unit ->
  t
(** {!default}, with the given parts replaced.

    Replacement and not merging, exactly as {!Camlcast_core.Binding.make} is: a
    given [~use] is the whole of what works a door, and not one more control
    that also does. *)

val default : t
(** [E] to work whatever you are looking at, [F3] for the map, and
    {!Camlcast_core.Binding.default} with Escape added to it as the way out.

    Escape is added here rather than in {!Camlcast_core.Binding.default}, for
    the reason that default gives for leaving it out: whether Escape ends a run
    is a game's to decide, and the engine will not assume it. A description
    played on a window of its own is exactly the case that has nothing else to
    end it, so this is the layer that assumes it — and a game with a pause
    screen of its own takes the key back with [~bindings]. *)
