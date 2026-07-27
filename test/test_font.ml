(** A fixed-grid bitmap font: where each glyph is in the atlas, and how a string
    lays out.

    All of the layout is a pure function of a string and the two cell
    dimensions, so most of this needs no atlas worth looking at and none of it
    needs a window. The one test that draws uses {!Framebuffer.offscreen} and
    reads the pixels back.

    The atlas here is 96 x 60 — sixteen cells of 6 x 10 across, six down, the
    same shape as the real one — and every pixel of cell [n] is the grey [n], so
    a glyph that lands on the screen says which cell it came from. *)

open Raycaster
open Support

let cell_w = 6
let cell_h = 10
let columns = 16

let atlas =
  Image.make ~height:60 96 (fun ~u ~v ->
      let n = (v / cell_h * columns) + (u / cell_w) in
      (Color.rgb n n n, 255))

let font = Font.make ~atlas ~width:cell_w ~height:cell_h ~first:32 ()

let boxed =
  Font.make ~fallback:'\127' ~atlas ~width:cell_w ~height:cell_h ~first:32 ()

(* The grid is the whole design: a glyph's place is arithmetic on its code
   point, so there is no table that can disagree with the picture. *)
let a_glyph_is_arithmetic_on_its_code_point () =
  let at c = Font.cell font c in
  Alcotest.(check (option (pair int int)))
    "the first cell is the first code point"
    (Some (0, 0))
    (at ' ');
  (* 'A' is 65, which is 33 past the first: two full rows of sixteen and one
     across. *)
  Alcotest.(check (option (pair int int))) "'A'" (Some (6, 20)) (at 'A');
  Alcotest.(check (option (pair int int)))
    "the last cell of the first row"
    (Some (90, 0))
    (at '/');
  Alcotest.(check (option (pair int int)))
    "the first of the second"
    (Some (0, 10))
    (at '0');
  Alcotest.(check (option (pair int int)))
    "and the very last cell the atlas has"
    (Some (90, 50))
    (at '\127')

let a_character_off_the_grid_has_no_cell () =
  Alcotest.(check (option (pair int int)))
    "below the first code point" None (Font.cell font '\001');
  Alcotest.(check (option (pair int int)))
    "and past the last cell" None (Font.cell font '\200');
  Alcotest.(check int) "the grid holds what it says" 96 (Font.capacity font)

(* Without a fallback a stray character draws nothing; with one it draws the
   box. Either way it keeps its place, which is what stops one wrong character
   becoming a wrong line. *)
let a_fallback_stands_in_for_what_is_missing () =
  Alcotest.(check (option (pair int int)))
    "nothing, by default" None (Font.glyph font '\200');
  Alcotest.(check (option (pair int int)))
    "the box, when there is one"
    (Some (90, 50))
    (Font.glyph boxed '\200');
  Alcotest.(check (option (pair int int)))
    "and a character that is present is unaffected"
    (Some (6, 20))
    (Font.glyph boxed 'A')

let a_cell_must_have_a_size () =
  List.iter
    (fun (w, h) ->
      Alcotest.check_raises (Printf.sprintf "%dx%d" w h)
        (Invalid_argument "Font.make: a cell must have a positive size")
        (fun () -> ignore (Font.make ~atlas ~width:w ~height:h ~first:32 ())))
    [ (0, 10); (6, 0); (-6, 10) ]

let measuring () =
  let w, h = Font.measure font "" in
  Alcotest.(check int) "empty text is nothing wide" 0 w;
  Alcotest.(check int) "but still a line tall" cell_h h;
  Alcotest.(check (pair int int))
    "one line is its length by a line"
    (3 * cell_w, cell_h)
    (Font.measure font "abc");
  Alcotest.(check (pair int int))
    "several is the longest by the count"
    (6 * cell_w, 3 * cell_h)
    (Font.measure font "abc\nlonger\nxy");
  (* A trailing newline is a line, because it is somewhere the next character
     would go. *)
  Alcotest.(check (pair int int))
    "and a trailing newline opens one"
    (3 * cell_w, 2 * cell_h)
    (Font.measure font "abc\n")

let wrapping_breaks_on_spaces () =
  let wrap text width = Font.wrap font text ~width in
  Alcotest.(check (list string))
    "text that fits is left alone" [ "one two" ]
    (wrap "one two" (7 * cell_w));
  Alcotest.(check (list string))
    "one pixel short of fitting breaks" [ "one"; "two" ]
    (wrap "one two" ((7 * cell_w) - 1));
  Alcotest.(check (list string))
    "and it breaks as late as it can" [ "aa bb"; "cc dd" ]
    (wrap "aa bb cc dd" (5 * cell_w));
  Alcotest.(check (list string))
    "newlines already there are kept" [ "aa"; "bb" ]
    (wrap "aa\nbb" (10 * cell_w))

(* The breaks that break onto nothing are breaks too. Dropping them made this
   disagree with [measure], which counts them, so a wrapped paragraph was drawn
   shorter than the panel sized for it — and a paragraph break in the source
   closed up on its way to the screen. *)
let wrapping_keeps_the_lines_that_are_empty () =
  let wrap text = Font.wrap font text ~width:(10 * cell_w) in
  Alcotest.(check (list string))
    "nothing at all is still one line" [ "" ] (wrap "");
  Alcotest.(check (list string))
    "a blank line between two is kept" [ "aa"; ""; "bb" ] (wrap "aa\n\nbb");
  Alcotest.(check (list string))
    "and a trailing newline opens one" [ "aa"; "" ] (wrap "aa\n");
  (* Which is the whole point of keeping them: wrap then measure agrees with
     draw, for the same text [measuring] checks a trailing newline on. *)
  let wrapped = wrap "aa\n\nbb\n" in
  Alcotest.(check (pair int int))
    "so wrapping then measuring is the height it draws at"
    (2 * cell_w, List.length wrapped * cell_h)
    (Font.measure font (String.concat "\n" wrapped))

(* A word with no space in it cannot be broken on one, and running off the page
   is worse than breaking mid-word, so it is broken. *)
let wrapping_breaks_a_word_that_cannot_fit () =
  Alcotest.(check (list string))
    "cut into full lines" [ "abcd"; "efgh"; "ij" ]
    (Font.wrap font "abcdefghij" ~width:(4 * cell_w));
  Alcotest.(check (list string))
    "and what is left of it flows on" [ "abcd"; "ef x" ]
    (Font.wrap font "abcdef x" ~width:(4 * cell_w));
  (* Cutting one flushes the line it interrupted, so that line is written once
     and not twice. *)
  Alcotest.(check (list string))
    "the line it interrupted is not written twice"
    [ "ab"; "abcd"; "efgh"; "ij" ]
    (Font.wrap font "ab abcdefghij" ~width:(4 * cell_w));
  (* Never an empty line the text did not ask for, and never wider than asked,
     however narrow the ask. *)
  let lines = Font.wrap font "an unreasonably narrow column" ~width:1 in
  Alcotest.(check bool)
    "a column too narrow for a glyph still gives one character a line" true
    (lines <> [] && List.for_all (fun l -> String.length l <= 1) lines)

(* The link between the two halves: the cell arithmetic above, and the pixels
   that actually arrive on the buffer. Cell [n] is the grey [n], so what lands
   on the screen names the cell it was taken from. *)
let drawing_puts_the_right_cell_in_the_right_place () =
  let fb = Framebuffer.offscreen ~width:40 ~height:20 in
  Font.draw fb font "A0" ~x:2 ~y:3 ~color:(Color.rgb 255 255 255);
  let grey n = Color.rgb n n n in
  Alcotest.check color "'A' is cell 33" (grey 33)
    (Framebuffer.pixel fb ~x:2 ~y:3);
  Alcotest.check color "across its whole cell" (grey 33)
    (Framebuffer.pixel fb ~x:(2 + cell_w - 1) ~y:(3 + cell_h - 1));
  (* '0' is 48, which is cell 16 — and it sits one advance along, not one atlas
     cell along, which is the same number here only because they agree. *)
  Alcotest.check color "'0' is cell 16, one advance along" (grey 16)
    (Framebuffer.pixel fb ~x:(2 + cell_w) ~y:3);
  Alcotest.check color "and nothing was drawn before the start"
    (Color.rgb 0 0 0)
    (Framebuffer.pixel fb ~x:1 ~y:3)

(* The colour comes from the caller and the atlas supplies the shape, the same
   way Color.level scales a surface pattern's colour by what its function
   computed. A white atlas cell therefore comes out as exactly the colour asked
   for. *)
let drawing_tints_the_atlas () =
  let white =
    Image.make ~height:60 96 (fun ~u:_ ~v:_ -> (Color.rgb 255 255 255, 255))
  in
  let font = Font.make ~atlas:white ~width:cell_w ~height:cell_h ~first:32 () in
  let fb = Framebuffer.offscreen ~width:20 ~height:20 in
  Font.draw fb font "A" ~x:0 ~y:0 ~color:(Color.rgb 200 100 50);
  Alcotest.check color "the colour asked for" (Color.rgb 200 100 50)
    (Framebuffer.pixel fb ~x:2 ~y:2)

(* Text is clipped by Paint like everything else, so a line running off the edge
   is cut rather than wrapped, refused, or writing outside the buffer. *)
let drawing_is_clipped_not_truncated () =
  let fb = Framebuffer.offscreen ~width:15 ~height:20 in
  (* Four glyphs at six pixels each is 24, well past the 15 available. *)
  Font.draw fb font "AAAA" ~x:0 ~y:0 ~color:(Color.rgb 255 255 255);
  Alcotest.check color "the first glyph is there" (Color.rgb 33 33 33)
    (Framebuffer.pixel fb ~x:0 ~y:0);
  Alcotest.check color "and the last pixel that fits" (Color.rgb 33 33 33)
    (Framebuffer.pixel fb ~x:14 ~y:0);
  (* Starting off the top-left corner, the visible part is still right. *)
  let fb = Framebuffer.offscreen ~width:15 ~height:20 in
  Font.draw fb font "A" ~x:(-2) ~y:(-3) ~color:(Color.rgb 255 255 255);
  Alcotest.check color "a glyph half off the corner draws its other half"
    (Color.rgb 33 33 33)
    (Framebuffer.pixel fb ~x:0 ~y:0)

let () =
  Alcotest.run "Font"
    [
      ( "the grid",
        [
          case "a glyph is arithmetic on its code point"
            a_glyph_is_arithmetic_on_its_code_point;
          case "a character off the grid has no cell"
            a_character_off_the_grid_has_no_cell;
          case "a fallback stands in for what is missing"
            a_fallback_stands_in_for_what_is_missing;
          case "a cell must have a size" a_cell_must_have_a_size;
        ] );
      ( "layout",
        [
          case "measuring" measuring;
          case "wrapping breaks on spaces" wrapping_breaks_on_spaces;
          case "wrapping keeps the lines that are empty"
            wrapping_keeps_the_lines_that_are_empty;
          case "wrapping breaks a word that cannot fit"
            wrapping_breaks_a_word_that_cannot_fit;
        ] );
      ( "drawing",
        [
          case "puts the right cell in the right place"
            drawing_puts_the_right_cell_in_the_right_place;
          case "tints the atlas" drawing_tints_the_atlas;
          case "is clipped, not truncated" drawing_is_clipped_not_truncated;
        ] );
    ]
