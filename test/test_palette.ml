open Raycaster
open Support

let known_ids = [ 1; 2; 3; 4 ]

let wall_ids_are_distinct () =
  let colors = List.map Palette.wall_color known_ids in
  Alcotest.(check int)
    "every wall id has its own colour" (List.length known_ids)
    (List.length (List.sort_uniq compare colors))

let unknown_ids_fall_back () =
  Alcotest.check color "an unmapped id still renders" (Palette.wall_color 42)
    (Palette.wall_color 99);
  Alcotest.(check bool)
    "the fallback is not one of the real walls" false
    (List.mem (Palette.wall_color 99) (List.map Palette.wall_color known_ids))

let patterns_are_distinct () =
  let fingerprint id =
    List.map
      (fun (u, v) -> Texture.sample (Palette.pattern id) ~u ~v)
      [ (0, 0); (8, 8); (17, 3); (33, 40) ]
  in
  let used = List.map fingerprint known_ids in
  Alcotest.(check int)
    "every wall id is built differently" (List.length known_ids)
    (List.length (List.sort_uniq compare used))

(* Ids 1-4 are solid; 5 and 6 are the see-through grille and window. *)
let some_ids_are_see_through () =
  List.iter
    (fun id ->
      Alcotest.(check bool)
        (Printf.sprintf "id %d is solid" id)
        true (Palette.pattern id).Texture.opaque)
    known_ids;
  List.iter
    (fun id ->
      Alcotest.(check bool)
        (Printf.sprintf "id %d is see-through" id)
        false (Palette.pattern id).Texture.opaque)
    [ 5; 6 ]

(* Walls face every direction now, so shading follows the normal's angle to the
   light rather than a fixed x/y rule. It must stay within a band so no wall
   goes black or blows out. *)
let orientation_shades_within_a_band () =
  List.iter
    (fun angle ->
      let f = Palette.face_shading (Vec.of_angle angle) in
      Alcotest.(check bool)
        (Printf.sprintf "%.2f rad stays in band" angle)
        true
        (f > 0. && f <= 1.))
    (List.init 16 (fun i -> float_of_int i *. Float.pi /. 8.));
  (* A wall square-on to the light is brighter than one edge-on to it. *)
  let square = Palette.face_shading Palette.light in
  let edge = Palette.face_shading (Vec.perp Palette.light) in
  Alcotest.(check bool) "facing the light is brighter" true (square > edge)

let fog_fades_with_distance () =
  Alcotest.check close "no fog at zero distance" 1. (Palette.fog 0.);
  Alcotest.(check bool)
    "fog grows with distance" true
    (Palette.fog 4. > Palette.fog 8.);
  Alcotest.check close "fog bottoms out" Config.min_brightness
    (Palette.fog Config.fog_distance);
  Alcotest.check close "and stays there" Config.min_brightness
    (Palette.fog (Config.fog_distance *. 10.))

(* What the renderer calls per wall: base colour, dimmed by orientation and by
   how far away it is. *)
let shaded_wall_combines_orientation_and_fog () =
  let w = Room.wall ~height:2. ~texture:1 (Vec.make 0. 0.) (Vec.make 0. 4.) in
  Alcotest.check color "orientation and fog are multiplied"
    (Color.shade (Palette.wall_color 1)
       (Palette.face_shading w.Room.normal *. Palette.fog 4.))
    (Palette.shaded_wall w ~distance:4.);
  Alcotest.(check bool)
    "a distant wall is darker than a near one" true
    ((Palette.shaded_wall w ~distance:9.).Color.r
   < (Palette.shaded_wall w ~distance:1.).Color.r)

(* The floor and ceiling are textured in world space, sampled from the pattern
   by the fractional part of the coordinates so it tiles every world unit. *)
let planes_are_textured () =
  Alcotest.(check int)
    "the pattern tiles every world unit"
    (Palette.plane_texel Palette.floor_pattern ~x:0.25 ~y:0.75)
    (Palette.plane_texel Palette.floor_pattern ~x:3.25 ~y:(-1.25));
  let shades =
    List.map
      (fun (x, y) -> Palette.plane_texel Palette.floor_pattern ~x ~y)
      [ (0.1, 0.1); (0.3, 0.1); (0.1, 0.3); (0.4, 0.4); (0.9, 0.2) ]
  in
  Alcotest.(check bool)
    "the texture has more than one shade across a tile" true
    (List.length (List.sort_uniq compare shades) > 1);
  Alcotest.(check bool)
    "floor and ceiling read differently" true
    (Palette.floor_color <> Palette.ceiling_color
    || Palette.floor_pattern != Palette.ceiling_pattern)

let () =
  Alcotest.run "Palette"
    [
      ( "walls",
        [
          case "ids are distinct" wall_ids_are_distinct;
          case "unknown ids fall back" unknown_ids_fall_back;
          case "patterns are distinct" patterns_are_distinct;
          case "some ids are see-through" some_ids_are_see_through;
        ] );
      ( "shading",
        [
          case "orientation shades within a band"
            orientation_shades_within_a_band;
          case "fog fades with distance" fog_fades_with_distance;
          case "shaded_wall combines orientation and fog"
            shaded_wall_combines_orientation_and_fog;
        ] );
      ("planes", [ case "planes are textured" planes_are_textured ]);
    ]
