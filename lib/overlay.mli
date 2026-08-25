(** Turns what a description said to draw over the frame into pixels.

    {!Scene.hud} is a list of primitives rather than a list of closures, so a
    scene stays a value a test can read as easily as the loop can draw. This
    module is the one place that reads the list. A HUD is nothing more than
    that: a fold over {!Camlcast_core.Paint} and {!Camlcast_core.Font}, in the
    order the primitives were written. *)

open Camlcast_core

val draw :
  ?aim:World.t * Player.t * Sight.t -> Framebuffer.t -> Prim.t list -> unit
(** Draw these over a finished frame, first written first, so the last one is on
    top.

    [aim] is the world this frame was drawn from, the eye it was drawn from, and
    the one cast that frame made. {!P.highlight} needs it and nothing else does.
    Without [aim] a highlight draws nothing. That is what a test asserting where
    a label landed wants, and it is how {!Run} withholds the ring on a frame
    whose crosshair is not the player's: under a cursor or a placed camera,
    {!Run.aiming} answers no and no [aim] arrives here.

    The cast arrives rather than being made here so that the ring goes round
    exactly what {!Aim.crosshair} notified, and so that a description with two
    highlights in it does not pay for two more casts. See {!Aim.ring}.

    Anything that is not a HUD primitive is ignored rather than refused. What
    may go on a HUD is {!Host.assemble}'s question, and it has already been
    asked by the time anything reaches here. *)
