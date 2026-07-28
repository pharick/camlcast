(** Decoding a picture file.

    The fixtures are written by [tools/make_art.py], which shares no code with
    what is being tested here: it is Python's own zlib and a PNG header written
    by hand. That matters more than it looks like it does. A decoder that
    swapped red and blue, or read each row at the wrong offset, would still
    agree with an encoder in this repository that made the same mistake — so the
    values below are literals in both places and in neither's code.

    [swatch.png] is 4 x 3 and no two of its channels are the same function of
    the position, so every way of getting a pixel wrong produces a different
    wrong answer. *)

open Camlcast
open Support

let read path =
  match Surface.read path with Ok s -> s | Error (`Msg m) -> Alcotest.fail m

let fails path =
  match Surface.read path with
  | Ok _ -> Alcotest.fail (path ^ " decoded, and should not have")
  | Error (`Msg m) -> m

(* Red, green, blue and alpha, each landing where it was put. This is the test
   the endianness of the conversion format exists to pass. *)
let channels_arrive_in_order () =
  let s = read "fixtures/swatch.png" in
  Alcotest.(check int) "the width" 4 s.Surface.width;
  Alcotest.(check int) "the height" 3 s.Surface.height;
  List.iter
    (fun (x, y, r, g, b, a) ->
      let c, alpha = Surface.sample s ~x ~y in
      Alcotest.check color (Printf.sprintf "(%d, %d)" x y) (Color.rgb r g b) c;
      Alcotest.(check int) (Printf.sprintf "(%d, %d) alpha" x y) a alpha)
    [
      (0, 0, 10, 100, 200, 245);
      (3, 0, 70, 100, 170, 155);
      (0, 2, 10, 180, 190, 185);
      (3, 2, 70, 180, 160, 95);
      (1, 1, 30, 140, 185, 185);
    ]

(* Rows are laid end to end at four bytes a pixel, whatever the file's own
   arrangement was. A picture wider than it is tall would still decode if the
   two extents were swapped and the pixels transposed with them, so the check
   worth making is that a named pixel is where its coordinates say. *)
let rows_are_laid_out_by_width () =
  let s = read "fixtures/swatch.png" in
  Alcotest.(check int)
    "one row down is one width along" 16
    (Surface.offset s ~x:0 ~y:1);
  Alcotest.(check int)
    "and the last pixel is the last one" 44
    (Surface.offset s ~x:3 ~y:2)

(* A JPEG has no alpha channel; converting to one that does must fill it solid,
   not clear, or every photograph loaded would come out invisible.

   Its colours are not worth asserting as values. This fixture is four pixels by
   three of maximum-frequency content, which is the worst case a lossy codec can
   be handed — the decoded blue at the origin is 165 against the 200 that went
   in — so what survives is the {e shape} of the picture and not its numbers.
   That is still enough to catch a channel that moved: red climbs left to right,
   green climbs top to bottom, and blue is the largest of the three, none of
   which would hold if two of them had been swapped. *)
let a_format_without_alpha_arrives_solid () =
  let s = read "fixtures/swatch.jpg" in
  Alcotest.(check int) "the width" 4 s.Surface.width;
  Alcotest.(check int) "the height" 3 s.Surface.height;
  for y = 0 to 2 do
    for x = 0 to 3 do
      Alcotest.(check int)
        (Printf.sprintf "(%d, %d) is solid" x y)
        255 (Surface.alpha s ~x ~y)
    done
  done;
  let at x y = fst (Surface.sample s ~x ~y) in
  Alcotest.(check bool)
    "red climbs across the picture" true
    ((at 0 0).Color.r < (at 3 0).Color.r);
  Alcotest.(check bool)
    "green climbs down it" true
    ((at 0 0).Color.g < (at 0 2).Color.g);
  Alcotest.(check bool)
    "and blue is the largest channel at the origin" true
    ((at 0 0).Color.b > (at 0 0).Color.g && (at 0 0).Color.g > (at 0 0).Color.r)

let a_missing_file_is_an_error () =
  let m = fails "fixtures/nothing-here.png" in
  Alcotest.(check bool)
    (Printf.sprintf "and says something: %s" m)
    true
    (String.length m > 0)

(* Not every file with pixels in the name has pixels in it. *)
let a_file_that_is_not_a_picture_is_an_error () =
  ignore (fails "fixtures/notanimage.txt")

let () =
  Alcotest.run "Surface"
    [
      ( "decoding",
        [
          case "channels arrive in order" channels_arrive_in_order;
          case "rows are laid out by width" rows_are_laid_out_by_width;
          case "a format without alpha arrives solid"
            a_format_without_alpha_arrives_solid;
        ] );
      ( "failing",
        [
          case "a missing file is an error" a_missing_file_is_an_error;
          case "a file that is not a picture is an error"
            a_file_that_is_not_a_picture_is_an_error;
        ] );
    ]
