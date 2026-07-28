(** The arithmetic the loop paces itself by. Reading the clock needs SDL; what
    is done to the two readings does not, and that is all of this. *)

open Camlcast
open Support

(* The frame the simulation is advanced by is the real one, so that speed does
   not depend on how long rendering took — but only up to a limit, past which a
   stalled program would otherwise move the player an enormous distance in one
   step. *)
let a_frame_lasts_as_long_as_it_took () =
  Alcotest.check close "an ordinary frame is measured as it happened" 0.02
    (Clock.frame_time ~previous:1.5 ~now:1.52);
  Alcotest.check close "a very long one is capped" Config.max_frame_time
    (Clock.frame_time ~previous:1.5 ~now:12.);
  Alcotest.check close "and a clock that went backwards moves nothing" 0.
    (Clock.frame_time ~previous:1.5 ~now:1.4)

(* Whatever is left of the budget is slept off, so a cheap frame does not spin
   the CPU; an expensive one is late already and waits no longer. *)
let a_short_frame_sleeps_off_the_rest () =
  Alcotest.check close "a frame that took half the budget sleeps the other half"
    (Config.frame_budget /. 2.)
    (Clock.idle_time ~spent:(Config.frame_budget /. 2.));
  Alcotest.check close "one that filled it sleeps not at all" 0.
    (Clock.idle_time ~spent:Config.frame_budget);
  Alcotest.check close "nor does one that overran it" 0.
    (Clock.idle_time ~spent:(Config.frame_budget *. 3.))

let () =
  Alcotest.run "Clock"
    [
      ( "pacing",
        [
          case "a frame lasts as long as it took"
            a_frame_lasts_as_long_as_it_took;
          case "a short frame sleeps off the rest"
            a_short_frame_sleeps_off_the_rest;
        ] );
    ]
