open Raycaster
open Support

let facing_east ?(pos = centre) () = Player.create ~pos ~angle:0.

(* Viewport builds every ray as [dir + right * k]. That is only correct while
   both are unit vectors and they stay perpendicular. *)
let camera_basis () =
  List.iter
    (fun angle ->
      let p = Player.create ~pos:centre ~angle in
      Alcotest.check close "dir is a unit vector" 1. (Vec.length p.Player.dir);
      Alcotest.check close "right is a unit vector" 1.
        (Vec.length p.Player.right);
      Alcotest.check close "right is perpendicular to dir" 0.
        (dot p.Player.dir p.Player.right))
    [ 0.; 0.9; 2.7; -1.4 ]

(* Facing east (+x), your right hand points south (+y). Getting this backwards
   would mirror the whole world. *)
let right_is_actually_to_the_right () =
  Alcotest.check vec "east faces south-right" (Vec.make 0. 1.)
    (facing_east ()).Player.right

let turning_preserves_the_basis () =
  let turned = Player.turn (facing_east ()) ~radians:0.7 in
  Alcotest.check close "dir stays a unit vector" 1.
    (Vec.length turned.Player.dir);
  Alcotest.check close "right stays a unit vector" 1.
    (Vec.length turned.Player.right);
  Alcotest.check close "the basis stays orthogonal" 0.
    (dot turned.Player.dir turned.Player.right)

(* Pitch is a separate, clamped axis: it tips the view but never past the limit
   where the sheared image would look wrong, and it never touches the basis. *)
let pitch_tips_within_a_limit () =
  let p = facing_east () in
  Alcotest.check close "starts level" 0. p.Player.pitch;
  Alcotest.check close "a small tip is kept" 0.1
    (Player.pitch_by p ~delta:0.1).Player.pitch;
  Alcotest.(check bool)
    "looking too far up is capped" true
    ((Player.pitch_by p ~delta:100.).Player.pitch <= Config.max_pitch +. 1e-9);
  Alcotest.(check bool)
    "and too far down" true
    ((Player.pitch_by p ~delta:(-100.)).Player.pitch
   >= -.Config.max_pitch -. 1e-9)

let turning_does_not_move_the_player () =
  let turned = Player.turn (facing_east ()) ~radians:0.7 in
  Alcotest.check vec "same position" centre turned.Player.pos;
  Alcotest.(check bool)
    "a positive turn swings dir towards +y" true (turned.Player.dir.y > 0.)

let walking_follows_the_facing () =
  let forward = Player.walk room (facing_east ()) ~forward:0.5 ~strafe:0. in
  Alcotest.check vec "forward moves along dir, without drift" (Vec.make 2.5 2.)
    forward.Player.pos;
  let back = Player.walk room (facing_east ()) ~forward:(-0.5) ~strafe:0. in
  Alcotest.check vec "backwards moves against dir" (Vec.make 1.5 2.)
    back.Player.pos

(* Sidestepping must cover the same ground as walking, or the two axes fight
   each other when you move diagonally. *)
let strafing_matches_walking_speed () =
  let strafed = Player.walk room (facing_east ()) ~forward:0. ~strafe:0.5 in
  Alcotest.check vec "sideways, and the same distance" (Vec.make 2. 2.5)
    strafed.Player.pos

let walls_block_movement () =
  let player =
    List.fold_left
      (fun p _ -> Player.walk room p ~forward:0.5 ~strafe:0.)
      (facing_east ()) (List.init 20 Fun.id)
  in
  Alcotest.(check bool)
    "the player never ends up inside a wall" false
    (World.blocked room player.Player.pos);
  Alcotest.(check bool)
    "and stops short of the east wall" true
    (player.Player.pos.x <= 4.0 -. Config.collision_padding)

(* Axes are resolved independently, so a blocked direction must not cancel the
   free one — otherwise you stick to walls instead of sliding along. *)
let a_blocked_axis_does_not_block_the_other () =
  let player = facing_east ~pos:(Vec.make 3.7 2.) () in
  Alcotest.check vec "x is blocked, y still moves" (Vec.make 3.7 2.5)
    (Player.walk room player ~forward:0.5 ~strafe:0.5).Player.pos

let spawn_uses_the_world () =
  Alcotest.check vec "spawn point" room.World.spawn
    (Player.spawn room).Player.pos

let () =
  Alcotest.run "Player"
    [
      ( "camera",
        [
          case "basis invariants" camera_basis;
          case "right is actually to the right" right_is_actually_to_the_right;
          case "turning preserves the basis" turning_preserves_the_basis;
          case "pitch tips within a limit" pitch_tips_within_a_limit;
          case "turning does not move the player"
            turning_does_not_move_the_player;
          case "spawn uses the world" spawn_uses_the_world;
        ] );
      ( "movement",
        [
          case "walking follows the facing" walking_follows_the_facing;
          case "strafing matches walking speed" strafing_matches_walking_speed;
        ] );
      ( "collision",
        [
          case "walls block movement" walls_block_movement;
          case "a blocked axis does not block the other"
            a_blocked_axis_does_not_block_the_other;
        ] );
    ]
