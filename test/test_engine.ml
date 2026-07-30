(** [Engine.step] and [Engine.simulate] are the pure part of the loop: input in,
    new player out, and what an arbitrary game state becomes over a frame. The
    rest of the module owns the window and cannot run headless; the arithmetic
    it paces itself by is {!Camlcast.Clock} and is tested there. *)

open Camlcast
open Support

let player () = Player.make ~room:0 ~pos:centre ~angle:0.
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

(* The loop runs an arbitrary state through the callbacks of an [Engine.game],
   and [simulate] is the whole of a frame that does not need a window: what the
   state becomes, given the frame's input. The game below is the smallest thing
   with a phase in it — it counts the frames it is given, adds up the time it is
   told has passed and the turning it is asked for, and ends after a second. *)
type phase = Playing | Ended
type session = { phase : phase; elapsed : float; frames : int; heading : float }

let start = { phase = Playing; elapsed = 0.; frames = 0; heading = 0. }

let counting =
  {
    Engine.update =
      (fun session ~dt ~motion ~actions:_ ->
        let elapsed = session.elapsed +. dt in
        {
          phase = (if elapsed >= 1. then Ended else Playing);
          elapsed;
          frames = session.frames + 1;
          heading = session.heading +. motion.Input.turn;
        });
    view = (fun _ -> (world, player ()));
    overlay = (fun _ _ -> ());
    pointing = (fun _ -> false);
    finished = (fun session -> session.phase = Ended);
    (* The table is read by the loop, which wants a window; [simulate] is the
       part of a frame that does not, so nothing here reaches it. *)
    bindings = Binding.default;
  }

(* [count] sixtieths of a second, each asking for a slightly wider turn than the
   last, so that a dropped frame shows up in the heading and not only in the
   clock. *)
let script count =
  List.init count (fun i ->
      (1. /. 60., { Input.still with turn = float_of_int i *. 0.01 }))

let play ?(pointing = false) ~focused frames =
  List.fold_left
    (fun session (dt, motion) ->
      Engine.simulate counting session ~focused ~pointing ~dt ~motion
        ~actions:Input.untouched)
    start frames

(* A scripted run lands exactly where the script adds up to: the loop hands the
   game one update per frame and nothing of its own. *)
let a_scripted_run_is_the_sum_of_its_frames () =
  let after = play ~focused:true (script 30) in
  Alcotest.(check int) "one update per frame" 30 after.frames;
  Alcotest.check close "the clock is the frames added up" 0.5 after.elapsed;
  Alcotest.check close "and the heading the turns" (0.01 *. 435.) after.heading

(* Phases live in the game's own state; the engine only asks whether it is over
   and takes the answer. *)
let a_phase_turns_over_when_the_game_says_so () =
  let midway = play ~focused:true (script 30) in
  Alcotest.(check bool)
    "half a second in, the run is still going" false
    (counting.Engine.finished midway);
  let after = play ~focused:true (script 90) in
  Alcotest.(check bool)
    "a second and a half in, it has ended" true
    (counting.Engine.finished after)

(* Losing focus stops the clock, not the loop: the game still gets its frames,
   so it can keep drawing, but nothing that runs on time moves in them. *)
let losing_focus_pauses_the_game () =
  let paused = play ~focused:false (script 90) in
  Alcotest.check close "no time passes behind another window" 0. paused.elapsed;
  Alcotest.check close "and the mouse does not turn the camera" 0.
    paused.heading;
  Alcotest.(check int) "but the game is still given its frames" 90 paused.frames;
  Alcotest.(check bool)
    "so a run that ends on the clock does not end while paused" false
    (counting.Engine.finished paused)

(* A screen the game draws over its world takes the mouse away from the camera,
   but not the clock away from the game: whether a journal pauses what is
   happening outside it is the game's decision, not the engine's. *)
let pointing_takes_the_mouse_but_not_the_clock () =
  let pointed = play ~focused:true ~pointing:true (script 30) in
  Alcotest.check close "the mouse does not turn the camera" 0. pointed.heading;
  Alcotest.check close "but the clock runs as it always did" 0.5 pointed.elapsed;
  Alcotest.(check int)
    "and the frames arrive as they always did" 30 pointed.frames

(* {1 Growing on a crossing}

   [Engine.run_world] asks a generator to extend the world when the horizon can
   have moved, and the horizon moves when the player goes {e through a doorway}.
   That is not the same question as whether they finished the frame somewhere
   else: a frame can round a jamb, or go all the way round a loop of rooms, and
   come back to the index it started with. Comparing the two indices calls both
   of those nothing happening, and the crossings are the only place they are
   written down. [Player.crossed] is that rule, and it is what a game reads
   whether it uses the wrapper or its own [Engine.run]. *)
let a_frame_that_crosses_nothing_does_not_grow () =
  let stayed = Player.traverse world (player ()) ~forward:0.5 ~strafe:0. in
  Alcotest.(check int)
    "the frame went through no doorway" 0
    (List.length stayed.Player.crossings);
  Alcotest.(check bool)
    "so the world is left alone" false (Player.crossed stayed)

(* A step round the loop fixture goes out of a room and back into it within one
   frame. The room index at the end is the one it set out with — which is
   exactly the case the old test missed — and a generator still has to hear
   about it. *)
let a_round_trip_still_grows () =
  let start = Player.make ~room:0 ~pos:centre ~angle:0. in
  let moved = Player.slide loop start (Vec.make 4. 2.5) in
  Alcotest.(check int)
    "it ends in the room it set out from" start.Player.room
    moved.Player.player.Player.room;
  Alcotest.(check int)
    "having gone through two doorways to get there" 2
    (List.length moved.Player.crossings);
  Alcotest.(check bool)
    "so the world is grown all the same" true (Player.crossed moved)

(* And it is grown {e once}, which is the half [Player.crossed] being a boolean
   already implies and nothing asserted. A step is clipped at each opening and
   the rest of it carried through, up to [Config.max_crossings_per_step] times
   per axis, so a frame can go through several doorways — and a generator hears
   about that frame once, with the pose it finished in. Building ahead of where
   the player now stands covers every room they passed through on the way, the
   renderer looking no deeper from there either; calling per doorway would ask
   for the same rooms over again with the same pose each time. *)
let extend_runs_once_a_frame_however_many_it_crossed () =
  let calls = ref 0 and asked = ref [] in
  let extend world (p : Player.t) =
    incr calls;
    asked := p.Player.room :: !asked;
    world
  in
  (* The round trip above, taken as a frame rather than a raw slide: facing
     along it so that [forward] alone is the step, since [Player.traverse]
     clamps a delta made of both to the longer of the two and would reshape it
     into something else. *)
  let leg = Vec.make 4. 2.5 in
  let start =
    Player.make ~room:0 ~pos:centre ~angle:(Float.atan2 leg.Vec.y leg.Vec.x)
  in
  let far = { Input.still with Input.forward = Vec.length leg } in
  let moved = Engine.move loop start far in
  Alcotest.(check bool)
    "the frame goes through more than one doorway" true
    (List.length moved.Player.crossings > 1);
  let _, after = Engine.grow ~extend loop start far in
  Alcotest.(check int)
    "the generator is asked once, not once per doorway" 1 !calls;
  Alcotest.(check int)
    "and asked about the room the frame finished in" after.Player.room
    (List.hd !asked);
  (* And a frame that crossed nothing does not ask at all. *)
  calls := 0;
  ignore
    (Engine.grow ~extend loop start { Input.still with Input.forward = 0.5 });
  Alcotest.(check int) "a frame that crossed nothing asks nothing" 0 !calls

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
      ( "state",
        [
          case "a scripted run is the sum of its frames"
            a_scripted_run_is_the_sum_of_its_frames;
          case "a phase turns over when the game says so"
            a_phase_turns_over_when_the_game_says_so;
          case "losing focus pauses the game" losing_focus_pauses_the_game;
          case "pointing takes the mouse but not the clock"
            pointing_takes_the_mouse_but_not_the_clock;
        ] );
      ( "growing",
        [
          case "a frame that crosses nothing does not grow"
            a_frame_that_crosses_nothing_does_not_grow;
          case "a round trip still grows" a_round_trip_still_grows;
          case "extend runs once a frame however many it crossed"
            extend_runs_once_a_frame_however_many_it_crossed;
        ] );
    ]
