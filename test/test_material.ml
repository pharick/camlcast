open Camlcast_core
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

(* A material is currently its pattern and nothing else, so two walls of the
   same material cost one set of arrays between them however many rooms they are
   in. Material's docstring claims this sharing, and sharing means physical
   equality. *)
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

(* [opaque] answers for the whole surface and [opaque_at] for one point of it.
   {!Sight} asks the pointwise one, because the renderer decides per texel: a
   solid texel is written over the column and hides what is behind, a clear one
   leaves it showing. [holes] is bar where either index falls in the first five
   of every sixteen, so a point in a bar and a point in a hole answer
   differently. The whole-surface question cannot express that difference. *)
let opacity_is_asked_of_a_point () =
  let m = Material.make ~pattern:holes in
  (* A texel column is 1/64 of a cell, so 2/64 along is inside the first bar and
     8/64 is well into the hole after it. Heights are measured up from the foot
     and rows count down from the top, so a hole in [v] is high up the cell. *)
  Alcotest.(check bool)
    "solid where the pattern is bar" true
    (Material.opaque_at m ~along:0.03 ~above:0.97);
  Alcotest.(check bool)
    "and see-through where it is not" false
    (Material.opaque_at m ~along:0.13 ~above:0.85);
  (* Either index being bar suffices: the fixture's bars run both ways. *)
  Alcotest.(check bool)
    "a bar in one axis alone still stops it" true
    (Material.opaque_at m ~along:0.13 ~above:0.97);
  (* The pattern tiles every world unit, the same way a plane's does, so a wall
     several cells along and several cells up shows the same texel. *)
  Alcotest.(check bool)
    "and it tiles every cell" true
    (Material.opaque_at m ~along:5.03 ~above:3.97)

(* Partly transparent is not solid. The renderer writes a pixel outright, and
   records its distance, only at a full 255. Anything less it blends, which
   leaves what is behind showing through and therefore pickable. Drawing the
   line anywhere else would make a pane of glass impossible to look through. *)
let a_half_transparent_texel_is_not_solid () =
  let veil alpha =
    Material.make
      ~pattern:
        (Texture.generate_masked (fun ~u:_ ~v:_ ->
             (Color.rgb 200 200 200, alpha)))
  in
  Alcotest.(check bool)
    "a full 255 is solid" true
    (Material.opaque_at (veil 255) ~along:0.5 ~above:0.5);
  Alcotest.(check bool)
    "one short of it is not" false
    (Material.opaque_at (veil 254) ~along:0.5 ~above:0.5);
  Alcotest.(check bool)
    "and neither is a clear one" false
    (Material.opaque_at (veil 0) ~along:0.5 ~above:0.5)

(* The two questions must not disagree: the renderer routes a whole wall by the
   cheap one and [Sight] asks the expensive one of a single ray. A pattern is
   [Texture.opaque] only when every texel is solid, so a material that reports
   opaque answers true at every point of itself. Swept across a cell in both
   directions rather than asserted at a point, because the claim is "everywhere"
   and one sample does not cover that. *)
let a_solid_material_is_solid_everywhere () =
  let m = Material.make ~pattern:banded in
  Alcotest.(check bool) "the whole surface says solid" true (Material.opaque m);
  let steps = List.init 17 (fun i -> float_of_int i /. 16.) in
  List.iter
    (fun along ->
      List.iter
        (fun above ->
          if not (Material.opaque_at m ~along ~above) then
            Alcotest.failf "an opaque material was see-through at %f, %f" along
              above)
        steps)
    steps

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

(* [plane_texel] returns the texel exactly as the pattern drew it. No colour of
   the material's own multiplies it, because the material has none. The only
   later modification is the fog the renderer applies afterwards. *)
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
      ( "opacity at a point",
        [
          case "opacity is asked of a point" opacity_is_asked_of_a_point;
          case "a half transparent texel is not solid"
            a_half_transparent_texel_is_not_solid;
          case "a solid material is solid everywhere"
            a_solid_material_is_solid_everywhere;
        ] );
      ( "planes",
        [
          case "planes tile every world unit" planes_tile_every_world_unit;
          case "a plane shows the pattern as it is"
            a_plane_shows_the_pattern_as_it_is;
        ] );
    ]
