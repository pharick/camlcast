(** [Engine.step] is the pure part of the loop: input in, new player out, plus
    the arithmetic it paces itself by. The rest of the module owns the window
    and cannot run headless. *)

open Raycaster
open Support

let player () = Player.create ~room:0 ~pos:centre ~angle:0.
let step motion = Engine.step world (player ()) motion
let heading (p : Player.t) = Float.atan2 p.dir.y p.dir.x

let standing_still () =
  let after = step Input.still in
  Alcotest.check vec "position is unchanged" centre after.Player.pos;
  Alcotest.check close "facing is unchanged" 0. (heading after)

(* Input hands step a finished per-frame delta — the speeds in Config are per
   second and it has already scaled them by the length of the frame — so step
   applies the motion as given rather than scaling it again. *)
let motion_is_applied_as_given () =
  Alcotest.check close "forward moves by that many cells" (centre.x +. 0.5)
    (step { Input.still with forward = 0.5 }).Player.pos.x;
  Alcotest.check close "and half as far for half the delta" (centre.x +. 0.25)
    (step { Input.still with forward = 0.25 }).Player.pos.x

let turning_is_applied_as_given () =
  Alcotest.check close "turn is the rotation, straight through" 0.4
    (heading (step { Input.still with turn = 0.4 }));
  Alcotest.check close "the other way round" (-0.4)
    (heading (step { Input.still with turn = -0.4 }))

(* Pitch is carried on the player and clamped, so it cannot tip past the limit
   however hard the mouse is thrown. *)
let pitch_is_carried_and_clamped () =
  Alcotest.check close "a small tip passes through" 0.1
    (step { Input.still with pitch = 0.1 }).Player.pitch;
  Alcotest.(check bool)
    "but it cannot tip past the limit" true
    ((step { Input.still with pitch = 10. }).Player.pitch
   <= Config.max_pitch +. 1e-9)

(* Turning is applied before moving, so a frame that does both walks in the
   direction the player ends up facing. *)
let turning_happens_before_moving () =
  Alcotest.(check bool)
    "the step follows the new facing" true
    ((step { Input.still with forward = 1.; turn = 1. }).Player.pos.y > centre.y)

let backwards_and_strafing () =
  Alcotest.(check bool)
    "backwards moves against dir" true
    ((step { Input.still with forward = -1. }).Player.pos.x < centre.x);
  Alcotest.check vec "strafing is sideways"
    (Vec.make centre.x (centre.y +. 1.))
    (step { Input.still with strafe = 1. }).Player.pos

let collisions_still_apply () =
  let far_side =
    List.fold_left
      (fun p _ -> Engine.step world p { Input.still with forward = 1. })
      (player ()) (List.init 200 Fun.id)
  in
  Alcotest.(check bool)
    "the loop cannot walk through a wall" false
    (Room.blocked room far_side.Player.pos)

(* The frame the simulation is advanced by is the real one, so that speed does
   not depend on how long rendering took — but only up to a limit, past which a
   stalled program would otherwise move the player an enormous distance in one
   step. *)
let a_frame_lasts_as_long_as_it_took () =
  Alcotest.check close "an ordinary frame is measured as it happened" 0.02
    (Engine.frame_time ~previous:1.5 ~now:1.52);
  Alcotest.check close "a very long one is capped" Config.max_frame_time
    (Engine.frame_time ~previous:1.5 ~now:12.);
  Alcotest.check close "and a clock that went backwards moves nothing" 0.
    (Engine.frame_time ~previous:1.5 ~now:1.4)

(* Whatever is left of the budget is slept off, so a cheap frame does not spin
   the CPU; an expensive one is late already and waits no longer. *)
let a_short_frame_sleeps_off_the_rest () =
  Alcotest.check close "a frame that took half the budget sleeps the other half"
    (Config.frame_budget /. 2.)
    (Engine.idle_time ~spent:(Config.frame_budget /. 2.));
  Alcotest.check close "one that filled it sleeps not at all" 0.
    (Engine.idle_time ~spent:Config.frame_budget);
  Alcotest.check close "nor does one that overran it" 0.
    (Engine.idle_time ~spent:(Config.frame_budget *. 3.))

let () =
  Alcotest.run "Engine"
    [
      ( "pacing",
        [
          case "a frame lasts as long as it took"
            a_frame_lasts_as_long_as_it_took;
          case "a short frame sleeps off the rest"
            a_short_frame_sleeps_off_the_rest;
        ] );
      ( "step",
        [
          case "standing still" standing_still;
          case "motion is applied as given" motion_is_applied_as_given;
          case "turning is applied as given" turning_is_applied_as_given;
          case "pitch is carried and clamped" pitch_is_carried_and_clamped;
          case "turning happens before moving" turning_happens_before_moving;
          case "backwards and strafing" backwards_and_strafing;
          case "collisions still apply" collisions_still_apply;
        ] );
    ]
