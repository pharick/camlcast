(** The face the panel falls back to, and whether it is still the one it was
    taken from.

    [Camlcast_edit.Typeface] carries a copy of [assets/font.png] in its source,
    as a mask, because a game using this library has no such file to read. A
    copy is a thing that goes stale, so the suite reads the original — named as
    a dependency of this stanza, so it is the repository's own — and compares
    every pixel. Redraw the picture without regenerating the module and this is
    what says so. *)

open Camlcast

let case name body = Alcotest.test_case name `Quick body
let built = Lazy.force Camlcast_edit.Typeface.builtin

(* By path rather than by Image.of_asset: Asset looks beside the executable and
   one directory above it, and a suite two levels down from the build root is
   not in reach of assets/. The path is relative to test/edit/, where dune runs
   this. *)
let drawn =
  match Image.load "../../assets/font.png" with
  | Ok atlas -> atlas
  | Error (`Msg message) -> Alcotest.failf "assets/font.png: %s" message

let the_grid_is_the_one_the_demos_use () =
  Alcotest.(check int) "one cell across" 6 built.Font.width;
  Alcotest.(check int) "one cell down" 10 built.Font.height;
  Alcotest.(check int) "cells across the atlas" 16 built.Font.columns;
  Alcotest.(check int) "the first code point" 32 built.Font.first;
  Alcotest.(check (option char))
    "and a box for anything past the last" (Some '\127') built.Font.fallback

let it_is_the_same_size_as_the_picture () =
  Alcotest.(check int) "across" drawn.Image.width built.Font.atlas.Image.width;
  Alcotest.(check int) "down" drawn.Image.height built.Font.atlas.Image.height

(* Ink for ink. The picture's alpha is only ever 0 or 255 -- checked here as
   well, since a redrawn one with soft edges would make the mask a rounding of
   it rather than a copy, and that is worth being told about rather than
   silently losing. *)
let every_pixel_agrees () =
  let width = drawn.Image.width and height = drawn.Image.height in
  let soft = ref 0 and differing = ref 0 in
  for v = 0 to height - 1 do
    for u = 0 to width - 1 do
      let i = (v * width) + u in
      let theirs = drawn.Image.alpha.(i)
      and ours = built.Font.atlas.Image.alpha.(i) in
      if theirs <> 0 && theirs <> 255 then incr soft;
      if theirs > 127 <> (ours > 127) then incr differing
    done
  done;
  Alcotest.(check int) "the picture is a mask already, not a shading" 0 !soft;
  Alcotest.(check int) "and the copy has ink exactly where it has" 0 !differing

let () =
  Alcotest.run "Typeface"
    [
      ( "the face",
        [
          case "the grid is the one the demos use"
            the_grid_is_the_one_the_demos_use;
          case "it is the same size as the picture"
            it_is_the_same_size_as_the_picture;
          case "every pixel agrees" every_pixel_agrees;
        ] );
    ]
