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

    {1 Worlds that change under the player}

    A description is rebuilt every frame, so a world can grow a room, lose one,
    or write its rooms in another order — and a {!Camlcast_core.Player.t}
    carries a bare room {e index}, which means something different the moment
    any of that happens.

    So the loop does not keep an index across frames. It keeps the room's
    {b name}, looks it up in each new world, and carries the pose into whatever
    index that name landed at this time. Names are what a description says and
    indices are what assembling one produces, so the name is the thing that was
    actually meant.

    A description that stops naming the room the player is standing in has
    removed the ground from under them, and there is no answer to that but to
    put them back at the world's spawn. That is what happens, and a game that
    means to move someone should say where with {!P.camera} rather than by
    deleting the floor. *)

open Camlcast_core

val carry : Scene.t -> was:string -> Player.t -> Player.t
(** The same pose, in the room this scene calls [was].

    What the loop does to a player between one frame's world and the next, and a
    function of values so it can be driven without a window. The pose is carried
    with an identity {!Camlcast_core.Transform}, so position, facing and pitch
    are untouched and only the index moves.

    If no room is called [was] any more, the answer is a player at the scene's
    own spawn. *)

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
