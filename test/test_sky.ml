open Raycaster
open Support

(* The sky is a vertical gradient: pale near the horizon, deep blue overhead.
   Away from the sun, higher up must be bluer and darker. *)
let gradient_climbs_to_the_zenith () =
  let away = Sky.sun_azimuth +. Float.pi in
  let horizon = Sky.color ~azimuth:away ~up:0. in
  let zenith = Sky.color ~azimuth:away ~up:1. in
  Alcotest.(check bool)
    "the zenith is bluer than the horizon" true
    (zenith.Color.b - zenith.Color.r > horizon.Color.b - horizon.Color.r);
  Alcotest.(check bool)
    "and darker overall" true
    (zenith.Color.r + zenith.Color.g < horizon.Color.r + horizon.Color.g)

(* The sun is a bright spot: looking straight at it is much brighter than
   looking away from it at the same elevation. *)
let the_sun_is_a_bright_spot () =
  let brightness (c : Color.t) = c.r + c.g + c.b in
  let at_sun = Sky.color ~azimuth:Sky.sun_azimuth ~up:Sky.sun_height in
  let away =
    Sky.color ~azimuth:(Sky.sun_azimuth +. Float.pi) ~up:Sky.sun_height
  in
  Alcotest.(check bool)
    "the sun is brighter than the sky away from it" true
    (brightness at_sun > brightness away);
  Alcotest.(check bool)
    "the sun is nearly white" true
    (at_sun.Color.r > 230 && at_sun.Color.g > 230 && at_sun.Color.b > 200)

(* The seam at +/-pi must not matter: looking at the sun, and at the same
   direction plus a full turn, must give the same sky (bar rounding). *)
let azimuth_wraps_around () =
  let a = Sky.color ~azimuth:Sky.sun_azimuth ~up:Sky.sun_height in
  let b =
    Sky.color ~azimuth:(Sky.sun_azimuth +. (2. *. Float.pi)) ~up:Sky.sun_height
  in
  let near p q = abs (p - q) <= 1 in
  Alcotest.(check bool)
    "a full turn is the same sky" true
    (near a.Color.r b.Color.r && near a.Color.g b.Color.g
   && near a.Color.b b.Color.b)

let () =
  Alcotest.run "Sky"
    [
      ( "gradient",
        [
          case "climbs to the zenith" gradient_climbs_to_the_zenith;
          case "azimuth wraps around" azimuth_wraps_around;
        ] );
      ("sun", [ case "is a bright spot" the_sun_is_a_bright_spot ]);
    ]
