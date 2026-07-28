open Camlcast
open Support

let flat c = Texture.generate (fun ~u:_ ~v:_ -> c)

let banded =
  Texture.generate (fun ~u ~v ->
      Color.level (Color.rgb 200 70 70)
        (if ((u / 16) + (v / 16)) land 1 = 0 then 240 else 120))

let holes =
  Texture.generate_masked (fun ~u ~v ->
      if u mod 16 < 5 || v mod 16 < 5 then (Color.rgb 200 200 200, 255)
      else (Color.rgb 0 0 0, 0))

(* A material is its pattern and nothing else yet, so two walls made of the same
   stuff cost one set of arrays between them however many rooms they are in.
   That is the sharing Material's docstring claims, and it is physical equality
   or it is not sharing. *)
let materials_share_a_pattern () =
  let here = Material.make ~pattern:banded
  and there = Material.make ~pattern:banded in
  Alcotest.(check bool)
    "the same pattern is the same arrays" true
    (here.Material.pattern == there.Material.pattern)

(* Opacity is a property of the pattern, not something the material declares, so
   it cannot drift out of step with the alpha the renderer will actually read. *)
let opacity_comes_from_the_pattern () =
  Alcotest.(check bool)
    "a solid pattern makes a solid material" true
    (Material.opaque (Material.make ~pattern:banded));
  Alcotest.(check bool)
    "one with holes does not" false
    (Material.opaque (Material.make ~pattern:holes))

(* The floor and ceiling are textured in world space, sampled by the fractional
   part of the coordinates so the pattern tiles every world unit. Tiling in
   world space rather than across the plane is what makes an incline visible. *)
let planes_tile_every_world_unit () =
  let m = Material.make ~pattern:banded in
  Alcotest.check color "a whole number of units along is the same texel"
    (Material.plane_texel m ~x:0.25 ~y:0.75)
    (Material.plane_texel m ~x:3.25 ~y:(-1.25));
  Alcotest.check color "and negative coordinates tile the same way"
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

(* Nothing stands between the pattern and the plane. What a texel was drawn as
   is what [plane_texel] hands back — no colour of the material's own multiplies
   it on the way, because the material has not got one. The only thing that
   still touches it is the fog the renderer applies afterwards. *)
let a_plane_shows_the_pattern_as_it_is () =
  let orange = Color.rgb 200 100 50 in
  Alcotest.check color "the texel arrives unchanged" orange
    (Material.plane_texel (Material.make ~pattern:(flat orange)) ~x:0.5 ~y:0.5);
  Alcotest.check color "black stays black" (Color.rgb 0 0 0)
    (Material.plane_texel
       (Material.make ~pattern:(flat (Color.rgb 0 0 0)))
       ~x:0.5 ~y:0.5)

let () =
  Alcotest.run "Material"
    [
      ( "make",
        [
          case "materials share a pattern" materials_share_a_pattern;
          case "opacity comes from the pattern" opacity_comes_from_the_pattern;
        ] );
      ( "planes",
        [
          case "planes tile every world unit" planes_tile_every_world_unit;
          case "a plane shows the pattern as it is"
            a_plane_shows_the_pattern_as_it_is;
        ] );
    ]
