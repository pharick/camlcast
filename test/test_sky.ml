open Raycaster
open Support

(* A sky is a value now, so the suite brings its own rather than reaching for
   whichever one some level happens to use. *)
let day =
  {
    Sky.horizon = Color.rgb 176 196 222;
    zenith = Color.rgb 40 62 126;
    sun = Color.rgb 255 246 216;
    sun_azimuth = -0.9;
    sun_height = 0.5;
    sun_radius = 0.55;
    gradient = 2.2;
  }

let color = Sky.color day
let sun_azimuth = day.sun_azimuth
let sun_height = day.sun_height

(* The sky is a vertical gradient: pale near the horizon, deep blue overhead.
   Away from the sun, higher up must be bluer and darker. *)
let gradient_climbs_to_the_zenith () =
  let away = sun_azimuth +. Float.pi in
  let horizon = color ~azimuth:away ~up:0. in
  let zenith = color ~azimuth:away ~up:1. in
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
  let at_sun = color ~azimuth:sun_azimuth ~up:sun_height in
  let away =
    color ~azimuth:(sun_azimuth +. Float.pi) ~up:sun_height
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
  let a = color ~azimuth:sun_azimuth ~up:sun_height in
  let b =
    color ~azimuth:(sun_azimuth +. (2. *. Float.pi)) ~up:sun_height
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
