open Camlcast
open Support

(* A sky is a value now, so the suite brings its own rather than reaching for
   whichever one some level happens to use. *)
let day =
  Sky.make ~horizon:(Color.rgb 176 196 222) ~zenith:(Color.rgb 40 62 126)
    ~sun:(Color.rgb 255 246 216) ~sun_azimuth:(-0.9) ~sun_height:0.5
    ~sun_radius:0.55 ~gradient:2.2 ()

let color = Sky.color day
let sun_azimuth = day.sun_azimuth
let sun_height = day.sun_height

(* The default is the noon above — pinned so that a change to it is a decision
   and not a drift. *)
let the_default_is_the_settled_noon () =
  Alcotest.(check bool) "default is the suite's own day" true (Sky.default = day)

(* sun_radius divides in color, so a sky with a radius of zero would put nan
   into every pixel the sun touches — which is exactly what make now refuses,
   along with the nan that would do the same through any other field. *)
let a_degenerate_sun_is_refused () =
  let refused what message build =
    Alcotest.check_raises what (Invalid_argument message) (fun () ->
        ignore (build ()))
  in
  List.iter
    (fun r ->
      refused (Printf.sprintf "sun_radius %f" r)
        "Sky.make: sun_radius has to be a positive size" (fun () ->
          Sky.make ~sun_radius:r ()))
    [ 0.; -0.5; Float.nan ];
  refused "a negative gradient" "Sky.make: gradient has to be zero or above"
    (fun () -> Sky.make ~gradient:(-1.) ());
  refused "a nan sun_height" "Sky.make: sun_height has to be finite" (fun () ->
      Sky.make ~sun_height:Float.nan ());
  refused "an infinite azimuth" "Sky.make: sun_azimuth has to be finite"
    (fun () -> Sky.make ~sun_azimuth:Float.infinity ())

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
  let away = color ~azimuth:(sun_azimuth +. Float.pi) ~up:sun_height in
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
  let b = color ~azimuth:(sun_azimuth +. (2. *. Float.pi)) ~up:sun_height in
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
          case "the default is the settled noon" the_default_is_the_settled_noon;
          case "a degenerate sun is refused" a_degenerate_sun_is_refused;
        ] );
      ("sun", [ case "is a bright spot" the_sun_is_a_bright_spot ]);
    ]
