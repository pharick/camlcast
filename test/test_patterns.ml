open Camlcast
open Camlcast_demo
open Support

(* The colours these are tested at. They are the showcase's own, because the
   assertions below about what is red and what is grey are assertions about a
   wall somebody will actually walk past. *)
let clay = Color.rgb 200 70 70
let lime = Color.rgb 188 182 172
let slate = Color.rgb 160 160 160
let steel = Color.rgb 105 108 120

(* Each pattern with its colours filled in, which is the form the level uses
   them in: a function of [u] and [v] and nothing else. *)
let brick = Patterns.brick ~color:clay ~mortar:lime
let panel = Patterns.panel ~color:slate
let stone = Patterns.stone ~color:slate
let checker = Patterns.checker ~color:slate
let plain = Patterns.plain ~color:slate
let bars = Patterns.bars ~color:steel
let glass = Patterns.glass ~lead:steel ~pane:(Color.rgb 150 205 230)

let patterns =
  [
    ("brick", brick);
    ("panel", panel);
    ("stone", stone);
    ("checker", checker);
    ("plain", plain);
  ]

(* Mortar is a flat level of a flat colour, so it is one exact value and a test
   can say "this texel is mortar" rather than "this texel is darker". *)
let mortar_texel = Color.level lime 210

let brick_has_mortar_courses () =
  (* Rows 0 and 1 of every course are mortar, whatever the column. *)
  List.iter
    (fun u ->
      Alcotest.check color
        (Printf.sprintf "column %d of the course line is mortar" u)
        mortar_texel (brick ~u ~v:0))
    [ 0; 7; 33; 63 ];
  Alcotest.(check bool)
    "the body of a brick is not mortar" true
    (brick ~u:8 ~v:8 <> mortar_texel)

(* Running bond: the vertical joint at the left of one course must not be
   there in the next, or the wall looks like a stack of columns. *)
let brick_courses_stagger () =
  Alcotest.check color "course 0 has a joint at u = 0" mortar_texel
    (brick ~u:0 ~v:8);
  Alcotest.(check bool)
    "course 1 does not" true
    (brick ~u:0 ~v:24 <> mortar_texel);
  Alcotest.check color "its joint has moved half a brick along" mortar_texel
    (brick ~u:16 ~v:24)

(* The thing a pattern carrying only a brightness could not have said. Mortar is
   lime and brick is clay, so the joint reads as a different material and not as
   a paler brick — which is what it would have had to be if the wall's one
   colour had arrived from the material afterwards. *)
let mortar_is_its_own_colour () =
  let joint = brick ~u:0 ~v:0 and body = brick ~u:8 ~v:8 in
  Alcotest.(check bool)
    "the body of the brick is strongly red" true
    (body.Color.r - body.Color.b > 80);
  Alcotest.(check bool)
    "the mortar between is not" true
    (abs (joint.Color.r - joint.Color.b) < 30)

(* The same, for the other two-colour pattern: the lead of a leaded window is a
   dark grey and the pane is a pale blue-green, and neither is the other dimmed.
   The pane is the brighter of the two and the bluer. *)
let glass_has_lead_and_pane () =
  let lead, _ = glass ~u:0 ~v:0 and pane, _ = glass ~u:16 ~v:16 in
  Alcotest.(check bool)
    "the pane is brighter than the lead" true
    (pane.Color.b > lead.Color.b);
  Alcotest.(check bool)
    "and bluer than it is red" true
    (pane.Color.b - pane.Color.r > 40);
  Alcotest.(check bool)
    "where the lead is near neutral" true
    (abs (lead.Color.b - lead.Color.r) < 20)

(* Colour goes in before [u] and [v], so a pattern is reused by applying it
   again rather than by dressing it afterwards. The shape has to survive that:
   the same function at two colours must put its light and dark texels in the
   same places, or "the same pattern in another colour" is not what happened.
   Surfaces relies on exactly this for its two chequered floors. *)
let one_pattern_serves_many_colours () =
  let yellow = Patterns.checker ~color:(Color.rgb 220 200 90)
  and brown = Patterns.checker ~color:(Color.rgb 116 110 98) in
  let squares f =
    List.map (fun (u, v) -> f ~u ~v) [ (8, 8); (24, 8); (8, 24) ]
  in
  let bright c = c.Color.r + c.Color.g + c.Color.b in
  let shape f =
    match squares f with
    | [ a; b; c ] -> (bright a > bright b, bright a > bright c)
    | _ -> assert false
  in
  Alcotest.(check bool)
    "the light and dark squares land in the same places" true
    (shape yellow = shape brown);
  Alcotest.(check bool)
    "but the colours differ" true
    (yellow ~u:8 ~v:8 <> brown ~u:8 ~v:8)

let panel_is_bevelled () =
  let lit c = c.Color.r in
  Alcotest.(check bool)
    "the top left edge catches the light" true
    (lit (panel ~u:0 ~v:0) > lit (panel ~u:16 ~v:16));
  Alcotest.(check bool)
    "the bottom right edge is in shadow" true
    (lit (panel ~u:31 ~v:31) < lit (panel ~u:16 ~v:16))

let plain_is_flat () =
  let first = plain ~u:0 ~v:0 in
  let flat = ref true in
  for v = 0 to Texture.default_size - 1 do
    for u = 0 to Texture.default_size - 1 do
      if plain ~u ~v <> first then flat := false
    done
  done;
  Alcotest.(check bool) "every texel is the same" true !flat

let patterns_are_distinct () =
  let fingerprint f =
    List.map
      (fun (u, v) -> f ~u ~v)
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
    (fun (name, f) ->
      Alcotest.(check bool)
        (name ^ " is opaque") true
        (Texture.opaque (Texture.generate f)))
    patterns

let see_through_patterns_are_not () =
  let bars = Texture.generate_masked bars
  and glass = Texture.generate_masked glass in
  Alcotest.(check bool) "bars is not opaque" false (Texture.opaque bars);
  Alcotest.(check bool) "glass is not opaque" false (Texture.opaque glass);
  Alcotest.(check int) "a bar texel is solid" 255 (Texture.alpha bars ~u:0 ~v:0);
  Alcotest.(check int)
    "a gap between bars is clear" 0
    (Texture.alpha bars ~u:10 ~v:10);
  Alcotest.(check bool)
    "a glass pane is translucent" true
    (let a = Texture.alpha glass ~u:16 ~v:16 in
     a > 0 && a < 255)

let () =
  Alcotest.run "Patterns"
    [
      ( "masonry",
        [
          case "brick has mortar courses" brick_has_mortar_courses;
          case "brick courses stagger" brick_courses_stagger;
          case "mortar is its own colour" mortar_is_its_own_colour;
        ] );
      ( "colour",
        [
          case "glass has lead and pane" glass_has_lead_and_pane;
          case "one pattern serves many colours" one_pattern_serves_many_colours;
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
