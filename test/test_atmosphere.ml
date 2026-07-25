open Raycaster
open Support

let daylight =
  Atmosphere.make ~haze:(Color.rgb 24 24 32) ~fog_distance:12.
    ~min_brightness:0.25
    ~light:(Vec.make (-0.4) (-0.9))
    ~ambient:0.6 ~directional:0.4

(* Walls face every direction, so shading follows the normal's angle to the
   light rather than a fixed x/y rule. It must stay within the band the
   atmosphere declares, or a wall would go black or blow out on orientation
   alone. *)
let orientation_shades_within_its_band () =
  List.iter
    (fun angle ->
      let f = Atmosphere.face_shading daylight (Vec.of_angle angle) in
      Alcotest.(check bool)
        (Printf.sprintf "%.2f rad stays in band" angle)
        true
        (f >= daylight.Atmosphere.ambient -. 1e-12
        && f <= daylight.Atmosphere.ambient +. daylight.Atmosphere.directional
                +. 1e-12))
    (List.init 16 (fun i -> float_of_int i *. Float.pi /. 8.));
  let light = daylight.Atmosphere.light in
  Alcotest.check close "square-on reaches the top of the band"
    (daylight.Atmosphere.ambient +. daylight.Atmosphere.directional)
    (Atmosphere.face_shading daylight light);
  Alcotest.check close "edge-on falls to the bottom of it"
    daylight.Atmosphere.ambient
    (Atmosphere.face_shading daylight (Vec.perp light))

(* Both faces of a wall light the same — a wall is a segment, not a surface with
   a front and a back, so its normal's sign is an artefact of which way round
   the endpoints were written. *)
let shading_ignores_which_way_the_normal_points () =
  List.iter
    (fun angle ->
      let n = Vec.of_angle angle in
      Alcotest.check close
        (Printf.sprintf "%.2f rad" angle)
        (Atmosphere.face_shading daylight n)
        (Atmosphere.face_shading daylight (Vec.scale n (-1.))))
    (List.init 8 (fun i -> float_of_int i *. Float.pi /. 4.))

(* A world with no discernible light source: every wall the same brightness
   whichever way it faces, so only distance tells them apart. This is a setting,
   not a special case — the same function with the band closed up. *)
let a_sourceless_atmosphere_flattens_orientation () =
  let dark =
    Atmosphere.make ~haze:(Color.rgb 8 8 9) ~fog_distance:9.
      ~min_brightness:0.06
      ~light:(Vec.make (-0.4) (-0.9))
      ~ambient:1. ~directional:0.
  in
  let a = Atmosphere.face_shading dark (Vec.of_angle 0.)
  and b = Atmosphere.face_shading dark (Vec.of_angle 1.3) in
  Alcotest.check close "orientation makes no difference at all" a b;
  Alcotest.check close "and nothing is dimmed by it" 1. a

let fog_fades_with_distance () =
  Alcotest.check close "no fog at zero distance" 1. (Atmosphere.fog daylight 0.);
  Alcotest.(check bool)
    "fog grows with distance" true
    (Atmosphere.fog daylight 4. > Atmosphere.fog daylight 8.);
  Alcotest.check close "fog bottoms out at its own distance"
    daylight.Atmosphere.min_brightness
    (Atmosphere.fog daylight daylight.Atmosphere.fog_distance);
  Alcotest.check close "and stays there" daylight.Atmosphere.min_brightness
    (Atmosphere.fog daylight (daylight.Atmosphere.fog_distance *. 10.))

(* Two atmospheres over the same geometry is the whole point of it being a
   value: a corridor closes in and goes black where a courtyard stays open. *)
let a_closer_atmosphere_fades_sooner () =
  let close_in =
    Atmosphere.make ~haze:(Color.rgb 8 8 9) ~fog_distance:9.
      ~min_brightness:0.06
      ~light:(Vec.make (-0.4) (-0.9))
      ~ambient:0.85 ~directional:0.15
  in
  Alcotest.(check bool)
    "the same wall is dimmer in the closer air" true
    (Atmosphere.fog close_in 6. < Atmosphere.fog daylight 6.);
  Alcotest.(check bool)
    "and what it fades to is darker" true
    (close_in.Atmosphere.haze.Color.r < daylight.Atmosphere.haze.Color.r)

(* The light direction is only ever used for a cosine, so make normalises it
   once rather than trusting every caller to. *)
let make_normalises_the_light () =
  let stretched =
    Atmosphere.make ~haze:(Color.rgb 0 0 0) ~fog_distance:10.
      ~min_brightness:0.1 ~light:(Vec.make 0. 40.) ~ambient:0.5
      ~directional:0.5
  in
  Alcotest.check close "the light is a unit vector" 1.
    (Vec.length stretched.Atmosphere.light);
  Alcotest.check close "so shading still reaches the top of the band" 1.
    (Atmosphere.face_shading stretched (Vec.make 0. 1.))

let () =
  Alcotest.run "Atmosphere"
    [
      ( "light",
        [
          case "orientation shades within its band"
            orientation_shades_within_its_band;
          case "shading ignores which way the normal points"
            shading_ignores_which_way_the_normal_points;
          case "a sourceless atmosphere flattens orientation"
            a_sourceless_atmosphere_flattens_orientation;
          case "make normalises the light" make_normalises_the_light;
        ] );
      ( "fog",
        [
          case "fog fades with distance" fog_fades_with_distance;
          case "a closer atmosphere fades sooner" a_closer_atmosphere_fades_sooner;
        ] );
    ]
