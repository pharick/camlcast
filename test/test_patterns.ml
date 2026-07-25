open Raycaster
open Camlcast_demo
open Support

let patterns =
  [
    ("brick", Patterns.brick);
    ("panel", Patterns.panel);
    ("stone", Patterns.stone);
    ("checker", Patterns.checker);
    ("plain", Patterns.plain);
  ]

let brick_has_mortar_courses () =
  (* Rows 0 and 1 of every course are mortar, whatever the column. *)
  List.iter
    (fun u ->
      Alcotest.(check int)
        (Printf.sprintf "column %d of the course line is mortar" u)
        130
        (Texture.sample Patterns.brick ~u ~v:0))
    [ 0; 7; 33; 63 ];
  Alcotest.(check bool)
    "the body of a brick is brighter than its mortar" true
    (Texture.sample Patterns.brick ~u:8 ~v:8 > 130)

(* Running bond: the vertical joint at the left of one course must not be
   there in the next, or the wall looks like a stack of columns. *)
let brick_courses_stagger () =
  Alcotest.(check int)
    "course 0 has a joint at u = 0" 130
    (Texture.sample Patterns.brick ~u:0 ~v:8);
  Alcotest.(check bool)
    "course 1 does not" true
    (Texture.sample Patterns.brick ~u:0 ~v:24 > 130);
  Alcotest.(check int)
    "its joint has moved half a brick along" 130
    (Texture.sample Patterns.brick ~u:16 ~v:24)

let panel_is_bevelled () =
  Alcotest.(check bool)
    "the top left edge catches the light" true
    (Texture.sample Patterns.panel ~u:0 ~v:0
    > Texture.sample Patterns.panel ~u:16 ~v:16);
  Alcotest.(check bool)
    "the bottom right edge is in shadow" true
    (Texture.sample Patterns.panel ~u:31 ~v:31
    < Texture.sample Patterns.panel ~u:16 ~v:16)

let plain_is_flat () =
  let first = Texture.sample Patterns.plain ~u:0 ~v:0 in
  let flat = ref true in
  for v = 0 to Texture.size - 1 do
    for u = 0 to Texture.size - 1 do
      if Texture.sample Patterns.plain ~u ~v <> first then flat := false
    done
  done;
  Alcotest.(check bool) "every texel is the same" true !flat

let patterns_are_distinct () =
  let fingerprint pattern =
    List.map
      (fun (u, v) -> Texture.sample pattern ~u ~v)
      [ (0, 0); (8, 8); (16, 16); (31, 5); (40, 33); (63, 63) ]
  in
  let fingerprints = List.map (fun (_, p) -> fingerprint p) patterns in
  Alcotest.(check int)
    "no two patterns are the same wall" (List.length patterns)
    (List.length (List.sort_uniq compare fingerprints))

(* Solid patterns are flagged opaque; the see-through ones are not, and carry
   real holes (the grille) or translucency (the glass). Which pass a wall is
   drawn in follows from this flag and nothing else. *)
let solid_patterns_are_opaque () =
  List.iter
    (fun (name, p) ->
      Alcotest.(check bool) (name ^ " is opaque") true p.Texture.opaque)
    patterns

let see_through_patterns_are_not () =
  Alcotest.(check bool) "bars is not opaque" false Patterns.bars.Texture.opaque;
  Alcotest.(check bool)
    "glass is not opaque" false Patterns.glass.Texture.opaque;
  Alcotest.(check int)
    "a bar texel is solid" 255
    (Texture.alpha Patterns.bars ~u:0 ~v:0);
  Alcotest.(check int)
    "a gap between bars is clear" 0
    (Texture.alpha Patterns.bars ~u:10 ~v:10);
  Alcotest.(check bool)
    "a glass pane is translucent" true
    (let a = Texture.alpha Patterns.glass ~u:16 ~v:16 in
     a > 0 && a < 255)

let () =
  Alcotest.run "Patterns"
    [
      ( "masonry",
        [
          case "brick has mortar courses" brick_has_mortar_courses;
          case "brick courses stagger" brick_courses_stagger;
        ] );
      ( "surfaces",
        [
          case "panel is bevelled" panel_is_bevelled;
          case "plain is flat" plain_is_flat;
          case "patterns are distinct" patterns_are_distinct;
        ] );
      ( "transparency",
        [
          case "solid patterns are opaque" solid_patterns_are_opaque;
          case "see-through patterns are not" see_through_patterns_are_not;
        ] );
    ]
