(** [Engine.step] is the pure part of the loop: input in, new player out. The
    rest of the module owns the window and cannot run headless. *)

open Raycaster
open Support

let player () = Player.create ~pos:centre ~angle:0.
let step motion = Engine.step room (player ()) motion
let heading (p : Player.t) = Float.atan2 p.dir.y p.dir.x

let standing_still () =
  let after = step Input.still in
  Alcotest.check vec "position is unchanged" centre after.Player.pos;
  Alcotest.check close "facing is unchanged" 0. (heading after)

(* Input hands step a finished per-frame delta, so step applies the motion as
   given rather than scaling it again. *)
let motion_is_applied_as_given () =
  Alcotest.check close "forward moves by that many cells"
    (centre.x +. Config.move_speed)
    (step { Input.still with forward = Config.move_speed }).Player.pos.x;
  Alcotest.check close "and half as far for half the delta"
    (centre.x +. (Config.move_speed /. 2.))
    (step { Input.still with forward = Config.move_speed /. 2. }).Player.pos.x

let turning_is_applied_as_given () =
  Alcotest.check close "turn is the rotation, straight through" Config.rot_speed
    (heading (step { Input.still with turn = Config.rot_speed }));
  Alcotest.check close "the other way round" (-.Config.rot_speed)
    (heading (step { Input.still with turn = -.Config.rot_speed }))

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
      (fun p _ -> Engine.step room p { Input.still with forward = 1. })
      (player ()) (List.init 200 Fun.id)
  in
  Alcotest.(check bool)
    "the loop cannot walk through a wall" false
    (World.blocked room far_side.Player.pos)

let () =
  Alcotest.run "Engine"
    [
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
