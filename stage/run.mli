(** Playing a description on a window.

    Everything below the description — the framebuffer, the renderer, the event
    queue, the clock, SDL itself — is on the other side of this one function. A
    game hands over what its world should be and never sees any of it.

    {1 What the crosshair is on}

    Every frame, after the player has moved, the middle of the screen is cast
    through the world by {!Camlcast_core.Sight} — the same ray the renderer
    draws with, so what can be picked is exactly what can be seen — and whatever
    it lands on is told, if it asked to be. {!P.wall}, {!P.sprite} and
    {!P.doorway} are the three things that can ask.

    {1 The camera}

    The runtime holds the player and walks it. A description says where the
    world is and what is in it; where the eye stands is the loop's, moved from
    the key bindings through the same {!Camlcast_core.Engine.step} every game on
    the old API called on its first line.

    That is the uncontrolled case, and it is the only one this step has. A
    description that wants to put the camera somewhere itself — a cutscene, a
    lift, a death — is the controlled case, and it arrives with the input hooks.

    {1 What a description may not do yet}

    Change the shape of the world. The player carries a room index across
    frames, and nothing yet keeps that index meaning the same room when the
    rooms it counts are rebuilt. A description that adds or removes rooms
    between frames will walk the player into the wrong one. Descriptions that
    only change what is {e in} their rooms are fine. *)

open Camlcast_core

val play :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?debug:bool ->
  ?use:Input.control ->
  ?bindings:Binding.t ->
  P.t ->
  (Engine.ending, [ `Msg of string ]) result
(** Open a window, play this description on it until the player quits, and close
    it again.

    [title], [width] and [height] are the window's, and default to the engine's
    own. [bindings] defaults to the engine's table with Escape added to it,
    which is what a description with nothing else to end it wants.

    [use] is the control that works whatever the crosshair is on — see {!P.wall}
    and its neighbours for [on_use] — and is [E].

    [debug] leaves F3 bound to {!Debug_map} and is true. A game that has stopped
    wanting a map over its world passes false, and the key does nothing —
    including the walk over the world that {!Check.world} does to feed it, which
    only happens while the map is up.

    The description is rendered once before the first frame, so that the player
    can be spawned where its world says. After that it is rendered once per
    frame, and the components in it keep whatever they have accumulated.

    A description that is not a world fails on that first render, before the
    window has drawn anything, and it fails by raising {!Host.Malformed} rather
    than by returning an error — a malformed description is a mistake in the
    program and not a condition the program can be in. {!Check.report} in a test
    is how to find that out at a better moment. *)
