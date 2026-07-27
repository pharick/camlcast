(** [Input] reads SDL, but the part worth testing is the part that does not.
    [Input.advance] is the edges and the hold timer, and it is a pure function
    of the previous frame, what is held now and how long the frame lasted — so
    these drive it with scripted keys and never open a window. *)

open Raycaster
open Tsdl
open Support

let e = Input.Key Sdl.Scancode.e
let c = Input.Key Sdl.Scancode.c
let click = Input.Button Input.Left
let tick = 1. /. 60.

(* One frame, in which [held] is everything the player is holding down. *)
let frame ?(dt = tick) held actions =
  Input.advance actions ~down:(fun control -> List.mem control held) ~pointer:(0, 0) ~dt

(* [count] frames of holding the same thing. *)
let frames ?dt held count actions =
  List.fold_left (fun actions () -> frame ?dt held actions) actions
    (List.init count (fun _ -> ()))

(* [count] frames the window spent out of focus, which is what [Engine.loop]
   does with one instead of sampling it. *)
let frames_frozen count actions =
  List.fold_left
    (fun actions () -> Input.freeze actions)
    actions
    (List.init count (fun _ -> ()))

let nothing_is_held_to_begin_with () =
  Alcotest.(check bool) "not down" false (Input.down Input.untouched e);
  Alcotest.(check bool) "not pressed" false (Input.pressed Input.untouched e);
  Alcotest.(check bool) "not released" false (Input.released Input.untouched e);
  Alcotest.check close "and held for no time" 0.
    (Input.held_for Input.untouched e)

(* A press is an edge: it is true for the one frame the key goes down, however
   long it is then held. Anything that should happen once per press reads it. *)
let a_press_is_only_the_frame_it_went_down () =
  let first = frame [ e ] Input.untouched in
  Alcotest.(check bool) "the frame it goes down" true (Input.pressed first e);
  let second = frame [ e ] first in
  Alcotest.(check bool) "not the frame after" false (Input.pressed second e);
  Alcotest.(check bool) "though it is still down" true (Input.down second e)

let a_release_is_only_the_frame_it_came_up () =
  let held = frames [ e ] 3 Input.untouched in
  Alcotest.(check bool) "not while held" false (Input.released held e);
  let up = frame [] held in
  Alcotest.(check bool) "the frame it comes up" true (Input.released up e);
  let after = frame [] up in
  Alcotest.(check bool) "not the frame after" false (Input.released after e);
  Alcotest.(check bool) "and it is not down" false (Input.down after e)

(* The hold counts from the frame after the press, so a press and a duration of
   zero arrive together and the count is of time actually observed. *)
let a_hold_adds_the_frames_up () =
  Alcotest.check close "nothing yet on the press itself" 0.
    (Input.held_for (frame [ e ] Input.untouched) e);
  Alcotest.check close "one frame's worth on the next" tick
    (Input.held_for (frames [ e ] 2 Input.untouched) e);
  Alcotest.check close "and fifty-nine after sixty frames" (59. *. tick)
    (Input.held_for (frames [ e ] 60 Input.untouched) e)

(* The duration survives one frame past the release, which is what lets a game
   tell a tap from a deliberate hold at the moment the key comes up — a door
   that opens on a press and commits on a long hold asks exactly this. *)
let a_hold_can_still_be_read_when_it_ends () =
  let long = frame [] (frames [ e ] 60 Input.untouched) in
  Alcotest.(check bool) "the release is reported" true (Input.released long e);
  Alcotest.check close "with the hold that led to it" (59. *. tick)
    (Input.held_for long e);
  Alcotest.check close "and it is gone the frame after" 0.
    (Input.held_for (frame [] long) e);
  let tap = frame [] (frame [ e ] Input.untouched) in
  Alcotest.check close "a tap released the same way reads as no hold at all" 0.
    (Input.held_for tap e)

let letting_go_starts_the_next_hold_from_nothing () =
  let again = frame [ e ] (frame [] (frames [ e ] 30 Input.untouched)) in
  Alcotest.(check bool) "the second press is an edge of its own" true
    (Input.pressed again e);
  Alcotest.check close "counting from nothing" 0. (Input.held_for again e)

let controls_are_counted_separately () =
  let both = frames [ e ] 10 Input.untouched |> frame [ e; c ] in
  Alcotest.(check bool) "the one just added is pressed" true
    (Input.pressed both c);
  Alcotest.(check bool) "the one already down is not" false
    (Input.pressed both e);
  Alcotest.check close "and keeps its own hold" (10. *. tick)
    (Input.held_for both e);
  Alcotest.check close "which is not the new one's" 0. (Input.held_for both c)

(* A mouse button is a control like any other, and shares no numbering with the
   keyboard however the two are laid out underneath. *)
let a_button_is_a_control_like_any_other () =
  let clicked = frames [ click ] 5 Input.untouched in
  Alcotest.(check bool) "the button is down" true (Input.down clicked click);
  Alcotest.check close "and holds like a key" (4. *. tick)
    (Input.held_for clicked click);
  Alcotest.(check bool)
    "without any key going down with it" false
    (List.exists (Input.down clicked)
       [ e; c; Input.Key Sdl.Scancode.escape; Input.Button Input.Right ])

(* The frame's length is what the hold is measured in, not the frame count, so
   a machine that renders slowly counts the same seconds as one that races. *)
let a_hold_is_measured_in_seconds_not_frames () =
  let slow = frames ~dt:0.1 [ e ] 6 Input.untouched in
  let fast = frames ~dt:0.01 [ e ] 51 Input.untouched in
  Alcotest.check close "half a second, slowly" 0.5 (Input.held_for slow e);
  Alcotest.check close "and the same half, quickly" 0.5 (Input.held_for fast e)

(* {1 Frames nobody was there for}

   [Engine.simulate] stops the clock and drops the motion of a frame the window
   spent out of focus, but the hold timer is fed the real length of the frame at
   the moment the controls are sampled — before any of that — so it is the one
   thing that would keep running while the game was behind another window. A
   minute away would come back as a minute of holding.

   [freeze] is what a frame is worth instead: the state as it stood, no edges,
   and no seconds added. *)
let an_unfocused_frame_costs_nothing () =
  let held = frames [ e ] 30 Input.untouched in
  let before = Input.held_for held e in
  let away = frames_frozen 600 held in
  Alcotest.(check bool) "what was down is still down" true (Input.down away e);
  Alcotest.check close "and has been held for exactly as long as it had" before
    (Input.held_for away e);
  Alcotest.(check bool) "nothing was pressed while away" false
    (Input.pressed away e);
  Alcotest.(check bool)
    "and nothing released" false (Input.released away e)

(* What did change while nobody was looking arrives as an ordinary edge on the
   first frame that is looked at, which is the first frame a game could have
   done anything about it. *)
let coming_back_reads_the_change_as_an_edge () =
  let away = frames_frozen 10 (frames [ e ] 30 Input.untouched) in
  let back = frame [] away in
  Alcotest.(check bool) "the key let go of while away comes up now" true
    (Input.released back e);
  Alcotest.check close "with the hold it actually had" (29. *. tick)
    (Input.held_for back e);
  let pressed = frame [ e; c ] away in
  Alcotest.(check bool) "and one pressed while away goes down now" true
    (Input.pressed pressed c)

let () =
  Alcotest.run "Input"
    [
      ( "edges",
        [
          case "nothing is held to begin with" nothing_is_held_to_begin_with;
          case "a press is only the frame it went down"
            a_press_is_only_the_frame_it_went_down;
          case "a release is only the frame it came up"
            a_release_is_only_the_frame_it_came_up;
          case "controls are counted separately" controls_are_counted_separately;
          case "a button is a control like any other"
            a_button_is_a_control_like_any_other;
        ] );
      ( "holds",
        [
          case "a hold adds the frames up" a_hold_adds_the_frames_up;
          case "a hold can still be read when it ends"
            a_hold_can_still_be_read_when_it_ends;
          case "letting go starts the next hold from nothing"
            letting_go_starts_the_next_hold_from_nothing;
          case "a hold is measured in seconds not frames"
            a_hold_is_measured_in_seconds_not_frames;
        ] );
      ( "focus",
        [
          case "an unfocused frame costs nothing"
            an_unfocused_frame_costs_nothing;
          case "coming back reads the change as an edge"
            coming_back_reads_the_change_as_an_edge;
        ] );
    ]
