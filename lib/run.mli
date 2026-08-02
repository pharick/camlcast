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

type window = Engine.window
(** A window, and everything behind it: SDL, the renderer, and the buffer a
    frame is drawn into.

    Transparent, and only just: a program that opens one window and plays run
    after run on it — a launcher — has to be able to say what it is holding, and
    the equation is how. What a game does with one is {!on}, below, and nothing
    else here; everything that opens, draws into or closes one is in
    [camlcast.core] and stays there. *)

type ending = Engine.ending =
  | Closed  (** the window was shut, or the desktop asked the program to stop *)
  | Left  (** the run ended on its own terms: {!P.finish}, or a leaving key *)

val with_window :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  (window -> ('a, [ `Msg of string ]) result) ->
  ('a, [ `Msg of string ]) result
(** Open a window, hand it over, and close it again when the function is done
    with it — however it is done, error or exception included.

    A window and a run are two lifetimes, and this is the first. A game with one
    world to show never notices; a launcher does, because it plays run after run
    on the same window so that going back to its menu is the picture changing
    rather than the window vanishing and coming back at the size it first had.
*)

val on :
  window -> ?controls:Controls.t -> P.t -> (ending, [ `Msg of string ]) result
(** Play a description on a window already open, and say how it ended.

    {!play} is this and {!with_window} together, and is what a game with one
    world to show wants.

    [controls] is everything the loop acts on by itself — walking, looking,
    leaving, working what the crosshair is on, and the map — and defaults to
    {!Controls.default}.

    However the run ends — quit, ending, or a description that could not be
    built — the mount it was played on is destroyed before this returns, so
    every effect the description started is stopped while there is still a
    window for it to have been holding something from. *)

val crossings_of : Scene.t -> Player.movement -> Events.crossing list
(** The doorways a step went through, by the names the description gave them.

    What the loop hands a description as {!Events.crossings}, exposed for the
    same reason {!carry} is: it is a function of values, and a test that walks a
    player about should be able to ask it the same question the loop does rather
    than a similar one it wrote itself. *)

val aiming : Scene.t -> bool
(** Whether the crosshair is the player's this frame, and so whether the things
    it lands on are told about it.

    Gaze and use are not "something is under the middle of the screen". They are
    the player looking at a thing and working it, and there are two states a
    description can put a frame in where the middle of the screen is not that.
    Under {!P.cursor} the mouse is loose and does not turn the camera, so the
    crosshair sits wherever the view was left — a pause menu over a corridor,
    and the use control working the door behind it. Under a placed {!P.camera}
    the view is the description's rather than the player's, so a cutscene
    panning across a room would drag [on_gaze] enter and leave over everything
    it swept past.

    False in either. What is suppressed is the {e telling}: {!Events.aim} still
    reports what the crosshair is on, because a description that took the camera
    may well want to know what is in front of it. And whatever held the
    crosshair when the menu went up is told it has lost it, so nothing is left
    highlighted behind a description that has taken the screen.

    Exposed for the same reason {!carry} and {!crossings_of} are. *)

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
  ?controls:Controls.t ->
  P.t ->
  (ending, [ `Msg of string ]) result
(** Open a window, play this description on it until the player quits, and close
    it again.

    [title], [width] and [height] are the window's, and default to the engine's
    own. [controls] is {!Controls.default}: WASD and the mouse to move, Escape
    to leave, [E] to work whatever the crosshair is on, and [F3] for the map.
    One record, so that rebinding the map is the same kind of act as rebinding
    the way out.

    The description is rendered once before the first frame, so that the player
    can be spawned where its world says. After that it is rendered once per
    frame, and the components in it keep whatever they have accumulated.

    That first render runs effects, because every render does — so
    {!Events.use_frame} is called once with [dt = 0.] before anything is drawn,
    and the scene it belonged to is never drawn either: the loop asks for the
    next state before it draws, so what reaches the screen first is the second
    render. It costs nothing to a handler that scales its work by [dt], which is
    how one belongs written; {!Events.use_frame} says what it costs to one that
    does not. The alternative is a loop that starts without knowing where the
    player stands, and where a game's world says they start is a thing only the
    game can answer, by being asked.

    A description that is not a world fails on that first render, before the
    window has drawn anything, and it fails by raising {!Host.Malformed} rather
    than by returning an error — a malformed description is a mistake in the
    program and not a condition the program can be in. {!Check.report} in a test
    is how to find that out at a better moment. *)
