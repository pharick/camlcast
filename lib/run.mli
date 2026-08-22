(** Playing a description on a window.

    The framebuffer, the renderer, the event queue, the clock, and SDL itself
    all sit behind this module. A game hands over a description of its world and
    never sees any of them.

    {1 What the crosshair is on}

    Every frame, after the player has moved, the loop casts the middle of the
    screen through the world with {!Camlcast_core.Sight}. It is the same ray the
    renderer draws with, so what can be picked is exactly what can be seen.
    Whatever the ray lands on is notified, if it registered for that. {!P.wall},
    {!P.sprite} and {!P.doorway} are the three constructors that can register.

    {1 The camera}

    The runtime holds the player and moves it. A description says where the
    world is and what is in it. The eye's position belongs to the loop, which
    feeds the key bindings through {!Camlcast_core.Engine.step} — the same step
    a game written straight against the platform calls for itself.

    That is the uncontrolled case, and the only case this step handles. In the
    controlled case a description places the camera itself — a cutscene, a lift,
    a death — and that case arrives with the input hooks.

    {1 Worlds that change under the player}

    A description is rebuilt every frame, so a world can grow a room, lose one,
    or list its rooms in a different order. A {!Camlcast_core.Player.t} carries
    a bare room {e index}, and any of those changes gives that index a different
    meaning.

    The loop therefore keeps no index across frames. It keeps the room's
    {b name}, looks the name up in each new world, and carries the pose into
    whatever index the name landed at this time. Names are what a description
    states; indices are what assembling one produces; the name is the meaning to
    preserve.

    If a description stops naming the room the player is standing in, the player
    is placed back at the world's spawn; no other answer exists. A game that
    means to move the player should say where with {!P.camera}, not by deleting
    the room. *)

open Camlcast_core

type window = Engine.window
(** A window, and everything behind it: SDL, the renderer, and the buffer a
    frame is drawn into.

    The type is transparent, and only just. A launcher — a program that opens
    one window and plays run after run on it — has to be able to name what it is
    holding, and the type equation is how. The only operation on one here is
    {!on}, below. Everything that opens, draws into or closes a window is in
    [camlcast.core] and stays there. *)

type ending = Engine.ending =
  | Closed  (** the window was shut, or the desktop asked the program to stop *)
  | Returned
      (** the run ended on its own terms: {!P.finish}, or a leaving key *)

val with_window :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  (window -> ('a, [ `Msg of string ]) result) ->
  ('a, [ `Msg of string ]) result
(** Open a window, pass it to the function, and close it when the function is
    done — on error or exception as well as on success.

    A window and a run have separate lifetimes; this manages the window's. A
    game with one world to show never notices the difference. A launcher does:
    it plays run after run on the same window, so returning to its menu changes
    the picture rather than closing the window and reopening it at its original
    size. *)

val on :
  window -> ?controls:Controls.t -> P.t -> (ending, [ `Msg of string ]) result
(** Play a description on a window that is already open, and report how the run
    ended.

    {!play} combines this with {!with_window}, and is what a game with one world
    to show wants.

    [controls] covers everything the loop acts on by itself, and defaults to
    {!Controls.default}:

    - walking,
    - looking,
    - leaving,
    - working what the crosshair is on,
    - the map.

    However the run ends — quit, ending, or a description that could not be
    built — the mount it was played on is destroyed before this returns. Every
    effect the description started is therefore stopped while the window it may
    hold resources from still exists. *)

val crossings_of : Scene.t -> Player.movement -> Events.crossing list
(** The doorways a step went through, by the names the description gave them.

    This is what the loop hands a description as {!Events.crossings}. It is
    exposed for the same reason {!carry} is: it is a function of plain values,
    and a test that walks a player around should ask the same question the loop
    asks, not a similar one it wrote itself. *)

val aiming : Scene.t -> bool
(** Whether the crosshair is the player's this frame, and so whether the things
    it lands on are notified.

    Gaze and use mean the player looking at a thing and working it, not merely
    "something is under the middle of the screen". A description can put a frame
    in two states where the middle of the screen is not that:

    - Under {!P.cursor} the mouse is loose and does not turn the camera, so the
      crosshair sits wherever the view was left — a pause menu over a corridor,
      with the use control working the door behind it.
    - Under a placed {!P.camera} the view is the description's rather than the
      player's, so a cutscene panning across a room would drag [on_gaze] enter
      and leave over everything it swept past.

    The answer is false in either state. Only the {e notification} is
    suppressed: {!Events.aim} still reports what the crosshair is on, because a
    description that took the camera may want to know what is in front of it.
    Whatever held the crosshair when the state began is told it has lost it, so
    nothing stays highlighted behind a description that has taken the screen.
    {!P.highlight} is part of the notification: its ring is withheld on the same
    frames, so the door behind a pause menu is neither worked nor ringed.

    Exposed for the same reason {!carry} and {!crossings_of} are. *)

val carry : Scene.t -> was:string -> Player.t -> Player.t
(** The same pose, in the room this scene calls [was].

    This is what the loop does to a player between one frame's world and the
    next. It is a function of plain values, so it can be driven without a
    window. The pose is carried with an identity {!Camlcast_core.Transform}:
    position, facing and pitch are untouched, and only the index changes.

    If no room is called [was] any more, the result is a player at the scene's
    own spawn. *)

val play :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?controls:Controls.t ->
  P.t ->
  (ending, [ `Msg of string ]) result
(** Open a window, play this description on it until the player quits, and close
    the window.

    [title], [width] and [height] are the window's, and default to the engine's
    own. [controls] defaults to {!Controls.default}:

    - WASD and the mouse to move,
    - Escape to leave,
    - [E] to work whatever the crosshair is on,
    - [F3] for the map.

    The controls are one record, so rebinding the map is the same kind of act as
    rebinding the way out.

    The description is rendered once before the first frame, so the player can
    be spawned where its world says. After that it is rendered once per frame,
    and its components keep whatever state they have accumulated.

    That first render runs effects, because every render does.
    {!Events.use_frame} is therefore called once with [dt = 0.] before anything
    is drawn, and the scene from that render is never drawn: the loop asks for
    the next state before it draws, so the first thing on screen is the second
    render. This costs nothing to a handler that scales its work by [dt], which
    is how a handler should be written; {!Events.use_frame} says what it costs
    to one that does not. The alternative is a loop that starts without knowing
    where the player stands, and only the game's world can answer that, by being
    rendered.

    A description that is not a world fails on that first render, before the
    window has drawn anything. It fails by raising {!Host.Malformed} rather than
    by returning an error, because a malformed description is a mistake in the
    program and not a condition the program can be in. {!Check.report} in a test
    finds the mistake at a better moment. *)
