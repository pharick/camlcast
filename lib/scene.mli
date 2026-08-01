(** What a frame of description comes to.

    A record of one field today, and a record rather than a bare
    {!Camlcast_core.World.t} because of what is about to join it: where the
    camera stands, and whatever a HUD tree draws over the top. Those arrive in
    their own steps, and a caller that already pattern-matches on a record will
    not have to be rewritten when they do. *)

open Camlcast_core

type t = {
  world : World.t;
  camera : Player.t option;
      (** where the description said the eye is, if it said.

          [None] is the uncontrolled case and the usual one: the runtime holds
          the player and walks it from the bindings. [Some] is a description
          that has taken the camera over — a cutscene, a lift, a death — and is
          the same controlled-or-not distinction a text input has. *)
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
