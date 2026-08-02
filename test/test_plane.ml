open Camlcast_core
open Support

let elevation () =
  let flat = Plane.horizontal 2.5 in
  Alcotest.check close "a horizontal plane is that height everywhere" 2.5
    (Plane.elevation flat (Vec.make 7. (-3.)));
  let tilted = Plane.make ~a:0.5 ~b:(-0.25) ~c:1. in
  Alcotest.check close "a*x + b*y + c"
    (1. +. (0.5 *. 4.) -. (0.25 *. 8.))
    (Plane.elevation tilted (Vec.make 4. 8.))

let gradient () =
  let tilted = Plane.make ~a:0.5 ~b:(-0.25) ~c:1. in
  Alcotest.check close "rise per unit along a direction"
    ((0.5 *. 2.) -. (0.25 *. 3.))
    (Plane.gradient tilted (Vec.make 2. 3.));
  Alcotest.check close "a horizontal plane never rises" 0.
    (Plane.gradient (Plane.horizontal 4.) (Vec.make 2. 3.))

(* On a flat floor one cell below the eye, the closed form must reduce to the
   textbook one: distance is inversely proportional to how far the pixel is
   below the horizon. *)
let flat_floor_matches_the_textbook () =
  let floor = Plane.horizontal 0. in
  let eye_z = 1. and eye_pos = Vec.make 0. 0. and dir = Vec.make 1. 0. in
  List.iter
    (fun row_factor ->
      match Plane.view_distance floor ~eye_z ~eye_pos ~dir ~row_factor with
      | Some d ->
          Alcotest.check close
            (Printf.sprintf "row factor %g" row_factor)
            (eye_z /. row_factor) d
      | None -> Alcotest.fail "the floor should be in view below the horizon")
    [ 0.25; 0.5; 1.; 2. ]

let the_floor_is_only_below_the_horizon () =
  let floor = Plane.horizontal 0. in
  let args row_factor =
    Plane.view_distance floor ~eye_z:1. ~eye_pos:(Vec.make 0. 0.)
      ~dir:(Vec.make 1. 0.) ~row_factor
  in
  Alcotest.(check bool)
    "above the horizon the floor is not in view" true
    (args (-0.5) = None);
  Alcotest.(check bool)
    "at the horizon it is infinitely far" true
    (args 0. = None)

let the_ceiling_is_only_above_the_horizon () =
  let ceiling = Plane.horizontal 3. in
  let args row_factor =
    Plane.view_distance ceiling ~eye_z:1. ~eye_pos:(Vec.make 0. 0.)
      ~dir:(Vec.make 1. 0.) ~row_factor
  in
  (match args (-0.5) with
  | Some d -> Alcotest.(check bool) "a ceiling above the horizon" true (d > 0.)
  | None -> Alcotest.fail "the ceiling should be in view above the horizon");
  Alcotest.(check bool)
    "below the horizon the ceiling is not in view" true
    (args 0.5 = None)

(* [view_distance] is [cast] plus a judgement, and the renderer wants the half
   without it: a raw distance whose sign it reads itself, and [infinity] — not
   an option — for the ray that never meets the plane. Both halves are load
   bearing over there. The sign is how [draw_planes] tells a plane behind the
   eye, which it leaves to the haze, from one a doorway clipped, which it leaves
   alone; and [infinity] is what lets it compare a floor's cast against a
   ceiling's with [<=] and take the nearer without opening two boxes a pixel. *)
let cast_answers_before_it_judges () =
  let floor = Plane.horizontal 0. in
  let cast row_factor =
    Plane.cast ~eye_z:1.
      ~base:(Plane.elevation floor (Vec.make 0. 0.))
      ~gradient:(Plane.gradient floor (Vec.make 1. 0.))
      ~row_factor
  in
  Alcotest.check close "below the horizon, the distance itself" 2. (cast 0.5);
  (* Where [view_distance] says None because the surface is behind the eye,
     this says how far behind — finite, and negative. *)
  Alcotest.(check bool)
    "above the horizon, a negative distance and not an absence" true
    (cast (-0.5) = -2.);
  Alcotest.(check bool)
    "and view_distance is the same call, judged" true
    (Plane.view_distance floor ~eye_z:1. ~eye_pos:(Vec.make 0. 0.)
       ~dir:(Vec.make 1. 0.) ~row_factor:(-0.5)
    = None);
  (* Parallel: infinity rather than the enormous finite number the division
     would otherwise give, which is the whole reason for the epsilon. *)
  Alcotest.(check bool)
    "along the horizon, infinitely far" true
    (cast 0. = infinity);
  Alcotest.(check bool)
    "and just inside the epsilon, still infinitely far" true
    (cast 1e-12 = infinity);
  Alcotest.(check bool)
    "while just outside it the division is allowed to happen" true
    (Float.is_finite (cast 1e-8))

let () =
  Alcotest.run "Plane"
    [
      ("geometry", [ case "elevation" elevation; case "gradient" gradient ]);
      ( "casting",
        [
          case "flat floor matches the textbook" flat_floor_matches_the_textbook;
          case "the floor is only below the horizon"
            the_floor_is_only_below_the_horizon;
          case "the ceiling is only above the horizon"
            the_ceiling_is_only_above_the_horizon;
          case "cast answers before it judges" cast_answers_before_it_judges;
        ] );
    ]
