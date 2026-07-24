open Raycaster
open Support

let patterns =
  [
    ("brick", Texture.brick);
    ("panel", Texture.panel);
    ("stone", Texture.stone);
    ("checker", Texture.checker);
    ("plain", Texture.plain);
  ]

(* Every texel is uploaded straight into a byte of an ARGB pixel, so anything
   outside 0..255 would wrap round into a different colour. *)
let texels_are_bytes () =
  List.iter
    (fun (name, pattern) ->
      let worst = ref 128 in
      for v = 0 to Texture.size - 1 do
        for u = 0 to Texture.size - 1 do
          let texel = Texture.sample pattern ~u ~v in
          if texel < 0 || texel > 255 then worst := texel
        done
      done;
      Alcotest.(check bool)
        (Printf.sprintf "%s stays in 0..255 (saw %d)" name !worst)
        true
        (!worst >= 0 && !worst <= 255))
    patterns

(* checker is the one pattern whose layout can be read off by eye, so it is
   also the one that pins down which way round sample's u and v go. *)
let sampling_is_row_major () =
  let checker = Texture.checker in
  let light = Texture.sample checker ~u:0 ~v:0 in
  Alcotest.(check int)
    "one square across is the other shade"
    (Texture.sample checker ~u:0 ~v:16)
    (Texture.sample checker ~u:16 ~v:0);
  Alcotest.(check bool)
    "and differs from the first" true
    (Texture.sample checker ~u:16 ~v:0 <> light);
  Alcotest.(check int)
    "two squares diagonally is back to the first shade" light
    (Texture.sample checker ~u:16 ~v:16)

let brick_has_mortar_courses () =
  (* Rows 0 and 1 of every course are mortar, whatever the column. *)
  List.iter
    (fun u ->
      Alcotest.(check int)
        (Printf.sprintf "column %d of the course line is mortar" u)
        130
        (Texture.sample Texture.brick ~u ~v:0))
    [ 0; 7; 33; 63 ];
  Alcotest.(check bool)
    "the body of a brick is brighter than its mortar" true
    (Texture.sample Texture.brick ~u:8 ~v:8 > 130)

(* Running bond: the vertical joint at the left of one course must not be
   there in the next, or the wall looks like a stack of columns. *)
let brick_courses_stagger () =
  Alcotest.(check int)
    "course 0 has a joint at u = 0" 130
    (Texture.sample Texture.brick ~u:0 ~v:8);
  Alcotest.(check bool)
    "course 1 does not" true
    (Texture.sample Texture.brick ~u:0 ~v:24 > 130);
  Alcotest.(check int)
    "its joint has moved half a brick along" 130
    (Texture.sample Texture.brick ~u:16 ~v:24)

let panel_is_bevelled () =
  Alcotest.(check bool)
    "the top left edge catches the light" true
    (Texture.sample Texture.panel ~u:0 ~v:0
    > Texture.sample Texture.panel ~u:16 ~v:16);
  Alcotest.(check bool)
    "the bottom right edge is in shadow" true
    (Texture.sample Texture.panel ~u:31 ~v:31
    < Texture.sample Texture.panel ~u:16 ~v:16)

let plain_is_flat () =
  let first = Texture.sample Texture.plain ~u:0 ~v:0 in
  let flat = ref true in
  for v = 0 to Texture.size - 1 do
    for u = 0 to Texture.size - 1 do
      if Texture.sample Texture.plain ~u ~v <> first then flat := false
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

(* Ray.offset reaches 1.0 exactly when a ray strikes a corner; without the
   clamp that would index one past the last column. *)
let offsets_map_into_the_texture () =
  Alcotest.(check int) "the left edge" 0 (Texture.column_of_offset 0.);
  Alcotest.(check int)
    "just inside the right edge" (Texture.size - 1)
    (Texture.column_of_offset 0.999);
  Alcotest.(check int)
    "a corner hit clamps" (Texture.size - 1)
    (Texture.column_of_offset 1.0);
  Alcotest.(check int)
    "and so does anything below zero" 0
    (Texture.column_of_offset (-0.5));
  Alcotest.(check int)
    "the middle of the face is the middle of the texture" (Texture.size / 2)
    (Texture.column_of_offset 0.5)

(* Solid patterns are flagged opaque; the see-through ones are not, and carry
   real holes (the grille) or translucency (the glass). *)
let solid_patterns_are_opaque () =
  List.iter
    (fun (name, p) ->
      Alcotest.(check bool) (name ^ " is opaque") true p.Texture.opaque)
    [ ("brick", Texture.brick); ("checker", Texture.checker) ]

let see_through_patterns_are_not () =
  Alcotest.(check bool) "bars is not opaque" false Texture.bars.Texture.opaque;
  Alcotest.(check bool) "glass is not opaque" false Texture.glass.Texture.opaque;
  Alcotest.(check int)
    "a bar texel is solid" 255
    (Texture.alpha Texture.bars ~u:0 ~v:0);
  Alcotest.(check int)
    "a gap between bars is clear" 0
    (Texture.alpha Texture.bars ~u:10 ~v:10);
  Alcotest.(check bool)
    "a glass pane is translucent" true
    (let a = Texture.alpha Texture.glass ~u:16 ~v:16 in
     a > 0 && a < 255)

let () =
  Alcotest.run "Texture"
    [
      ( "pixels",
        [
          case "texels are bytes" texels_are_bytes;
          case "sampling is row major" sampling_is_row_major;
          case "offsets map into the texture" offsets_map_into_the_texture;
        ] );
      ( "patterns",
        [
          case "brick has mortar courses" brick_has_mortar_courses;
          case "brick courses stagger" brick_courses_stagger;
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
