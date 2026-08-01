(** What a frame of description comes to.

    A world, where the eye is if the description said, whether the run is over,
    whether the pointer is loose, what a HUD tree draws over the top, and every
    handler the description hung on something the crosshair can land on. A
    record rather than a bare {!Camlcast_core.World.t} because a frame turned
    out to be all of that — it began as the world alone, and each of the others
    arrived without a caller that matched on the record having to be rewritten,
    which is the argument for the shape restated as what happened. *)

open Camlcast_core

type t = {
  world : World.t;
  camera : Player.t option;
      (** where the description said the eye is, if it said.

          [None] is the uncontrolled case and the usual one: the runtime holds
          the player and walks it from the bindings. [Some] is a description
          that has taken the camera over — a cutscene, a lift, a death — and is
          the same controlled-or-not distinction a text input has. *)
  pointing : bool;
      (** whether the description asked for the pointer. See {!P.cursor}. *)
  finished : bool;
      (** whether the description said it is over. See {!P.finish}. *)
  targets : Aim.t;
      (** what each thing in the world asked to be told about the crosshair,
          found by what {!Camlcast_core.Sight} reports *)
  hud : Prim.t list;
      (** what to draw over the finished world, in the order it was written.

          Kept as the primitives themselves rather than as closures, because a
          scene should be a value a test can read as easily as the loop can
          draw. {!Overlay.draw} is what turns them into pixels. *)
}
