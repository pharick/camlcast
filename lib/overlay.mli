(** Turning what a description said to draw over the frame into pixels.

    {!Scene.hud} is a list of primitives rather than a list of closures, so a
    scene stays a value a test can read as easily as the loop can draw. This is
    the one place that reads them, and it is the whole of what a HUD is: a fold
    over {!Camlcast_core.Paint} and {!Camlcast_core.Font}, in the order they
    were written. *)

open Camlcast_core

val draw : ?aim:World.t * Player.t -> Framebuffer.t -> Prim.t list -> unit
(** Draw these over a finished frame, first written first, so the last one is on
    top.

    [aim] is the world and the eye this frame was drawn from, which
    {!P.highlight} needs and nothing else does. Without it a highlight draws
    nothing, which is what a test asserting where a label landed wants — and how
    {!Run} withholds the ring on a frame the crosshair is not the player's:
    under a cursor or a placed camera no [aim] arrives here, on {!Run.aiming}'s
    answer.

    Anything that is not a HUD primitive is ignored rather than refused: what
    may go on a HUD is {!Host.assemble}'s question, and it has already been
    asked by the time anything reaches here. *)
