open Raycaster
open Support

let flat brightness = Texture.generate (fun ~u:_ ~v:_ -> brightness)

let banded =
  Texture.generate (fun ~u ~v -> if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 120)

let holes =
  Texture.generate_masked (fun ~u ~v ->
      if u mod 16 < 5 || v mod 16 < 5 then (200, 255) else (0, 0))

(* Colour and pattern are chosen independently: the colour says what a surface
   is made of, the pattern says how it was put together. The same masonry is red
   brick or grey stone depending only on the colour beside it. *)
let colour_and_pattern_are_independent () =
  let red = Material.make ~color:(Color.rgb 200 70 70) ~pattern:banded
  and grey = Material.make ~color:(Color.rgb 160 160 160) ~pattern:banded in
  Alcotest.(check bool)
    "two colours can share a pattern" true
    (red.Material.pattern == grey.Material.pattern);
  Alcotest.(check bool)
    "and stay different materials" true
    (red.Material.color <> grey.Material.color)

(* Opacity is a property of the pattern, not something the material declares, so
   it cannot drift out of step with the alpha the renderer will actually read. *)
let opacity_comes_from_the_pattern () =
  Alcotest.(check bool)
    "a solid pattern makes a solid material" true
    (Material.opaque (Material.make ~color:(Color.rgb 1 2 3) ~pattern:banded));
  Alcotest.(check bool)
    "one with holes does not" false
    (Material.opaque (Material.make ~color:(Color.rgb 1 2 3) ~pattern:holes))

(* The floor and ceiling are textured in world space, sampled by the fractional
   part of the coordinates so the pattern tiles every world unit. Tiling in
   world space rather than across the plane is what makes an incline visible. *)
let planes_tile_every_world_unit () =
  let m = Material.make ~color:(Color.rgb 116 110 98) ~pattern:banded in
  Alcotest.(check int)
    "a whole number of units along is the same texel"
    (Material.plane_texel m ~x:0.25 ~y:0.75)
    (Material.plane_texel m ~x:3.25 ~y:(-1.25));
  Alcotest.(check int)
    "and negative coordinates tile the same way"
    (Material.plane_texel m ~x:0.25 ~y:0.75)
    (Material.plane_texel m ~x:(-2.75) ~y:(-9.25));
  let shades =
    List.map
      (fun (x, y) -> Material.plane_texel m ~x ~y)
      [ (0.1, 0.1); (0.6, 0.1); (0.1, 0.6); (0.6, 0.6); (0.9, 0.2) ]
  in
  Alcotest.(check bool)
    "the pattern has more than one shade across a tile" true
    (List.length (List.sort_uniq compare shades) > 1)

(* The two ends of the brightness range, so nothing has quietly inverted: a
   texel at full brightness comes out the material's own colour, and a black
   texel comes out black whatever the colour is. *)
let brightness_scales_the_colour () =
  let white = Material.make ~color:(Color.rgb 200 100 50) ~pattern:(flat 255)
  and black = Material.make ~color:(Color.rgb 200 100 50) ~pattern:(flat 0) in
  Alcotest.(check int)
    "a full texel is full colour" 255
    (Material.plane_texel white ~x:0.5 ~y:0.5);
  Alcotest.(check int)
    "an empty one is black" 0
    (Material.plane_texel black ~x:0.5 ~y:0.5)

let () =
  Alcotest.run "Material"
    [
      ( "make",
        [
          case "colour and pattern are independent"
            colour_and_pattern_are_independent;
          case "opacity comes from the pattern" opacity_comes_from_the_pattern;
        ] );
      ( "planes",
        [
          case "planes tile every world unit" planes_tile_every_world_unit;
          case "brightness scales the colour" brightness_scales_the_colour;
        ] );
    ]
